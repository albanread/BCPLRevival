#ifndef SCRATCH_ALLOCATOR_H
#define SCRATCH_ALLOCATOR_H

#include <vector>
#include <stdexcept> // For std::runtime_error
#include <cstdint>   // For uint32_t
#include <cassert>   // For assert()

/**
 * @class ScratchAllocator
 * @brief Manages a pool of temporary (scratch) general-purpose registers.
 *
 * This class provides a simple mechanism to acquire and release scratch
 * registers, ensuring that the same register is not used for two different
 * purposes at the same time within a single expression evaluation.
 */
class ScratchAllocator {
public:
    ScratchAllocator();
    ~ScratchAllocator();

    /**
     * @brief Acquires a single available scratch register from the pool.
     * @return A uint32_t representing the acquired register.
     * @throws std::runtime_error if no scratch registers are available.
     */
    uint32_t acquire();

    /**
     * @brief Releases a scratch register, returning it to the pool.
     * @param reg The register to be released.
     */
    void release(uint32_t reg);

    /**
     * @brief Returns a const reference to the list of currently used registers.
     * @return A const reference to a vector of uint32_t representing used registers.
     */
    const std::vector<uint32_t>& getUsedRegisters() const;

    private:
    std::vector<uint32_t> available_regs_;
    std::vector<uint32_t> used_regs_;
};

#endif // SCRATCH_ALLOCATOR_H
