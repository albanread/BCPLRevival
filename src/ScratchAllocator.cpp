#include "ScratchAllocator.h"
#include <stdexcept>
#include <cassert> // For assert()
#include <algorithm> // For std::find

ScratchAllocator::ScratchAllocator() {
    // Populate the pool with AArch64's primary caller-saved scratch registers.
    // Using their integer identifiers (9 through 15).
    for (uint32_t i = 9; i <= 15; ++i) {
        available_regs_.push_back(i);
    }
}

ScratchAllocator::~ScratchAllocator() {
    // In a debug build, this assertion will fire if we have a logic error
    // where a register was acquired but never released.
    assert(used_regs_.empty() && "Error: Not all scratch registers were released.");
}

uint32_t ScratchAllocator::acquire() {
    if (available_regs_.empty()) {
        throw std::runtime_error("Compiler Error: Out of scratch registers!");
    }

    uint32_t reg = available_regs_.back();
    available_regs_.pop_back();
    used_regs_.push_back(reg);
    return reg;
}

void ScratchAllocator::release(uint32_t reg) {
    auto it = std::find(used_regs_.begin(), used_regs_.end(), reg);
    if (it != used_regs_.end()) {
        used_regs_.erase(it);
        available_regs_.push_back(reg);
    } else {
        // This case indicates a logic error, like releasing a register that wasn't acquired.
        assert(false && "Attempted to release a register that was not in use.");
    }
}

const std::vector<uint32_t>& ScratchAllocator::getUsedRegisters() const {
    return used_regs_;
}
