#ifndef REGISTER_MANAGER_H
#define REGISTER_MANAGER_H

#include <cstdint>
#include <vector>
#include <string>
#include <unordered_map>
#include <set>
#include <list>

class AArch64Instructions; // Forward declaration

class RegisterManager {
public:
    RegisterManager(AArch64Instructions& instructions);

    // Acquires a register for a variable. Spills if necessary.
    uint32_t acquireRegister(const std::string& varName, int stackOffset);

    // Releases a register, potentially spilling its value to memory.
    void releaseRegister(uint32_t reg);

    // Releases a register without spilling its value to memory.
    void releaseRegisterWithoutSpill(uint32_t reg);

    // Removes a variable from its assigned register without spilling.
    void removeVariableFromRegister(const std::string& varName);

    // Re-establishes a variable-to-register mapping without loading from stack.
    void reassignRegister(const std::string& varName, uint32_t reg, int stackOffset);

    // Gets the register holding a variable, or 0xFFFFFFFF if not in a register.
    uint32_t getVariableRegister(const std::string& varName) const;

    // Marks a variable's register as dirty (modified in register, needs to be written back to memory).
    void markDirty(const std::string& varName);

    // Spills all dirty registers to memory.
    void spillAllDirtyRegisters();

    // Spills a specific register to memory.
    void spillRegister(uint32_t reg);

    // Gets all currently used registers (including those holding variables).
    const std::set<uint32_t>& getUsedRegisters() const { return used_regs_; }

    // Gets the variable name associated with a register.
    std::string getVariableName(uint32_t reg) const;

    // Clears all register allocations (e.g., at function exit).
    void clear();

private:
    AArch64Instructions& instructions_;

    // Available general-purpose registers (excluding special ones like SP, FP, LR, XZR, X28 (global pointer))
    std::vector<uint32_t> available_regs_;
    std::set<uint32_t> used_regs_; // All registers currently in use

    // Map from variable name to the register it's currently in
    std::unordered_map<std::string, uint32_t> var_to_reg_;
    // Map from register to the variable it holds
    std::unordered_map<uint32_t, std::string> reg_to_var_;
    // Map from variable name to its stack offset
    std::unordered_map<std::string, int> var_to_stack_offset_;
    // Set of registers whose values are dirty (modified in register, not yet in memory)
    std::set<uint32_t> dirty_regs_;

    // For LRU spilling strategy
    std::list<uint32_t> lru_list_; // Front is most recently used, back is least recently used
    std::unordered_map<uint32_t, std::list<uint32_t>::iterator> lru_map_;

    // Helper to find a free register
    uint32_t findFreeRegister();
};

#endif // REGISTER_MANAGER_H