#include "RegisterManager.h"
#include "AArch64Instructions.h"
#include <algorithm>
#include <iostream>

RegisterManager::RegisterManager(AArch64Instructions& instructions) : instructions_(instructions) {
    // Initialize available general-purpose registers.
    // Exclude X0-X2 (argument/return), X28 (global pointer), X29 (frame pointer), X30 (link register), SP, XZR.
    // X16, X17 are also temporary registers, often used for veneers/PLT. Let's avoid them for now.
    // So, available registers will be X3-X8 and X19-X27.
    for (uint32_t i = 3; i <= 8; ++i) { // Caller-saved, general purpose
        available_regs_.push_back(i);
    }
    for (uint32_t i = 19; i <= 27; ++i) { // Callee-saved, general purpose
        available_regs_.push_back(i);
    }
    // Initialize LRU list with available registers (least recently used first)
    for (uint32_t reg : available_regs_) {
        lru_list_.push_back(reg);
        lru_map_[reg] = --lru_list_.end();
    }
}

uint32_t RegisterManager::acquireRegister(const std::string& varName, int stackOffset) {
    // 1. Check if variable is already in a register
    if (var_to_reg_.count(varName)) {
        return var_to_reg_[varName];
    }

    // 2. Try to find a free register
    uint32_t reg = findFreeRegister();
    if (reg == 0xFFFFFFFF) { // No free register found, need to spill
        if (used_regs_.empty()) {
            throw std::runtime_error("RegisterManager: No registers available to acquire or spill for variable " + varName);
        }
        // Basic spilling strategy: spill the first used register
        uint32_t regToSpill = *used_regs_.begin();
        spillRegister(regToSpill);

        // Remove the spilled register from active management
        std::string spilledVarName = reg_to_var_[regToSpill];
        var_to_reg_.erase(spilledVarName);
        reg_to_var_.erase(regToSpill);
        var_to_stack_offset_.erase(spilledVarName);
        used_regs_.erase(regToSpill);
        available_regs_.push_back(regToSpill); // Make it available again

        reg = findFreeRegister(); // Now a register should be available
        if (reg == 0xFFFFFFFF) {
            throw std::runtime_error("RegisterManager: Failed to acquire register after spilling.");
        }
    }

    // 3. Assign register to variable
    var_to_reg_[varName] = reg;
    reg_to_var_[reg] = varName;
    var_to_stack_offset_[varName] = stackOffset;
    used_regs_.insert(reg);

    // 4. Load variable's value from stack into the acquired register
    instructions_.ldr(reg, AArch64Instructions::X29, stackOffset, "Load variable " + varName + " into " + instructions_.regName(reg));

    return reg;
}

void RegisterManager::releaseRegister(uint32_t reg) {
    if (used_regs_.count(reg)) {
        // If the register holds a dirty value, spill it back to memory
        if (dirty_regs_.count(reg)) {
            spillRegister(reg);
        }

        // Remove mappings and return to available pool
        std::string varName = reg_to_var_[reg];
        var_to_reg_.erase(varName);
        reg_to_var_.erase(reg);
        var_to_stack_offset_.erase(varName);
        used_regs_.erase(reg);
        available_regs_.push_back(reg); 
    } else {
        // This should not happen if logic is correct
        // std::cerr << "Warning: Attempted to release an unused register: " << instructions_.regName(reg) << std::endl;
    }
}

void RegisterManager::releaseRegisterWithoutSpill(uint32_t reg) {
    if (used_regs_.count(reg)) {
        // Remove mappings and return to available pool
        std::string varName = reg_to_var_[reg];
        var_to_reg_.erase(varName);
        reg_to_var_.erase(reg);
        var_to_stack_offset_.erase(varName);
        dirty_regs_.erase(reg); // No longer tracking dirty state for this variable in this register
        used_regs_.erase(reg);
        available_regs_.push_back(reg); 
        
        // Remove from LRU list and map if it was there
        if (lru_map_.count(reg)) {
            lru_list_.erase(lru_map_[reg]);
            lru_map_.erase(reg);
        }
    } else {
        // This should not happen if logic is correct
        // std::cerr << "Warning: Attempted to release an unused register without spilling: " << instructions_.regName(reg) << std::endl;
    }
}

void RegisterManager::removeVariableFromRegister(const std::string& varName) {
    auto it = var_to_reg_.find(varName);
    if (it != var_to_reg_.end()) {
        uint32_t reg = it->second;
        var_to_reg_.erase(varName);
        reg_to_var_.erase(reg);
        var_to_stack_offset_.erase(varName);
        dirty_regs_.erase(reg); // No longer tracking dirty state for this variable in this register
        used_regs_.erase(reg); // Mark as no longer used by a variable
        // Do NOT add to available_regs_ here, as the register might still be in use by the system (e.g., saved by caller)
    } else {
        // std::cerr << "Warning: Attempted to remove variable from register, but it was not in one: " << varName << std::endl;
    }
}

void RegisterManager::reassignRegister(const std::string& varName, uint32_t reg, int stackOffset) {
    // This is used when a register's value is restored (e.g., after a function call)
    // and we want to re-establish its association with a variable.
    if (var_to_reg_.count(varName)) {
        // If the variable was already in a register, release the old one first.
        releaseRegister(var_to_reg_[varName]);
    }

    var_to_reg_[varName] = reg;
    reg_to_var_[reg] = varName;
    var_to_stack_offset_[varName] = stackOffset;
    used_regs_.insert(reg);
    // Do NOT load from stack here, as the value is assumed to be restored by the caller.
    // Do NOT mark dirty, as its dirty state depends on subsequent operations.
}

uint32_t RegisterManager::getVariableRegister(const std::string& varName) const {
    auto it = var_to_reg_.find(varName);
    if (it != var_to_reg_.end()) {
        return it->second;
    }
    return 0xFFFFFFFF; // Indicate not in a register
}

std::string RegisterManager::getVariableName(uint32_t reg) const {
    auto it = reg_to_var_.find(reg);
    if (it != reg_to_var_.end()) {
        return it->second;
    }
    return ""; // Indicate no variable associated with this register
}

void RegisterManager::markDirty(const std::string& varName) {
    uint32_t reg = getVariableRegister(varName);
    if (reg != 0xFFFFFFFF) {
        dirty_regs_.insert(reg);
    } else {
        // This should not happen if markDirty is called only for variables in registers
        // std::cerr << "Warning: Attempted to mark dirty a variable not in a register: " << varName << std::endl;
    }
}

void RegisterManager::spillAllDirtyRegisters() {
    // Create a copy to avoid iterator invalidation issues if spillRegister modifies dirty_regs_
    std::set<uint32_t> currentDirtyRegs = dirty_regs_;
    for (uint32_t reg : currentDirtyRegs) {
        spillRegister(reg);
    }
    dirty_regs_.clear();
}

void RegisterManager::spillRegister(uint32_t reg) {
    if (reg_to_var_.count(reg)) {
        std::string varName = reg_to_var_[reg];
        int stackOffset = var_to_stack_offset_[varName];
        instructions_.str(reg, AArch64Instructions::X29, stackOffset, "Spill " + varName + " from " + instructions_.regName(reg) + " to stack");
        dirty_regs_.erase(reg);
    }
}

uint32_t RegisterManager::findFreeRegister() {
    if (!available_regs_.empty()) {
        uint32_t reg = available_regs_.back();
        available_regs_.pop_back();
        return reg;
    }
    return 0xFFFFFFFF; // No free register
}

void RegisterManager::clear() {
    available_regs_.clear();
    used_regs_.clear();
    var_to_reg_.clear();
    reg_to_var_.clear();
    var_to_stack_offset_.clear();
    dirty_regs_.clear();

    // Re-populate available registers
    for (uint32_t i = 3; i <= 8; ++i) {
        available_regs_.push_back(i);
    }
    for (uint32_t i = 19; i <= 27; ++i) {
        available_regs_.push_back(i);
    }
}
