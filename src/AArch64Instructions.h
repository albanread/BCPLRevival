// AArch64Instructions.h
#ifndef AARCH64_INSTRUCTIONS_H
#define AARCH64_INSTRUCTIONS_H

#include <cstdint>
#include <vector>
#include <string>

class LabelManager;

class AArch64Instructions {
public:
    // Register definitions
    static const uint32_t X0 = 0;   // First argument/return value
    static const uint32_t X1 = 1;   // Second argument/B register
    static const uint32_t X2 = 2;   // Third argument/C register
    static const uint32_t X3 = 3;
    static const uint32_t X4 = 4;
    static const uint32_t X5 = 5;
    static const uint32_t X6 = 6;
    static const uint32_t X7 = 7;
    static const uint32_t X28 = 28; // Global pointer (G)
    static const uint32_t X29 = 29; // Frame pointer (FP)
    static const uint32_t X30 = 30; // Link register (LR)
    static const uint32_t SP = 31;  // Stack pointer
    static const uint32_t XZR = 31; // Zero register (when used as source)

    struct Instruction {
        uint32_t encoding;
        std::string assembly;
        std::string comment;
        bool needsLabelResolution = false;
        std::string targetLabel;
        size_t address; // Address of the instruction in the generated code
        bool hasLabel = false;
        std::string label;

        bool isStore() const { return (encoding & 0x3B000000) == 0x38000000; } // Simplified check for STR/LDR (immediate offset)
        bool isLoad() const { return (encoding & 0x3B000000) == 0x38000000; } // Simplified check for STR/LDR (immediate offset)
        void resolveLabel(int32_t offset) { encoding |= ((offset / 4) & 0x7FFFF) << 5; } // Simplified for B/BL
        std::string toString() const { return assembly; }
    };

    void setPendingLabel(const std::string& label);

private:
    std::vector<Instruction> instructions;
    std::string pendingLabel_;
    void addInstruction(Instruction instr);

public:
    // Basic instruction generators
    void mov(uint32_t rd, uint32_t rm, const std::string& comment = "");
    void movz(uint32_t rd, uint16_t imm16, uint8_t shift, const std::string& comment = "");
    void movk(uint32_t rd, uint16_t imm16, uint8_t shift, const std::string& comment = "");
    enum ShiftType {
        LSL, // Logical Shift Left
        LSR, // Logical Shift Right
        ASR, // Arithmetic Shift Right
        ROR  // Rotate Right
    };

    void add(uint32_t rd, uint32_t rn, uint32_t rm, ShiftType shift_type, uint32_t shift_amount, const std::string& comment = "");
    void add(uint32_t rd, uint32_t rn, uint32_t imm, const std::string& comment = "");
    void sub(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment = "");
    void mul(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment = "");
    void sdiv(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment = "");
    void lsl(uint32_t rd, uint32_t rn, uint32_t imm, const std::string& comment = "");
    void msub(uint32_t rd, uint32_t rn, uint32_t rm, uint32_t ra, const std::string& comment = "");
    void stp(uint32_t rt1, uint32_t rt2, uint32_t rn, int32_t imm, const std::string& comment = "");
    void ldp(uint32_t rt1, uint32_t rt2, uint32_t rn, int32_t imm, const std::string& comment = "");
    void str(uint32_t rt, uint32_t rn, int32_t imm, const std::string& comment = "");
    void ldr(uint32_t rt, uint32_t rn, int32_t imm, const std::string& comment = "");
    void b(const std::string& label, const std::string& comment = "");
    void bl(const std::string& label, const std::string& comment = "");
    void ret(const std::string& comment = "");

    void adr(uint32_t rd, const std::string& label, const std::string& comment = "");
    void br(uint32_t rn, const std::string& comment = "");

    // Helper methods for common BCPL patterns
    void moveAtoB() { mov(X1, X0, "B := A"); }
    void moveBtoC() { mov(X2, X1, "C := B"); }
    void loadImmediate(uint32_t rd, int64_t value, const std::string& comment = "");


    void neg(uint32_t rd, uint32_t rm, const std::string& comment = "");
    void eor(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment = "");
    void cmp(uint32_t rn, uint32_t rm, const std::string& comment = "");
    void beq(const std::string& label, const std::string& comment = "");
    void bne(const std::string& label, const std::string& comment = "");
    void bge(const std::string& label, const std::string& comment = "");
    void blt(const std::string& label, const std::string& comment = "");
    void ble(const std::string& label, const std::string& comment = "");
    void bgt(const std::string& label, const std::string& comment = "");
    void cset(uint32_t rd, uint32_t cond, const std::string& comment = "");

    enum Condition {
        EQ = 0b0000,
        NE = 0b0001,
        CS = 0b0010, HS = 0b0010,
        CC = 0b0011, LO = 0b0011,
        MI = 0b0100,
        PL = 0b0101,
        VS = 0b0110,
        VC = 0b0111,
        HI = 0b1000,
        LS = 0b1001,
        GE = 0b1010,
        LT = 0b1011,
        GT = 0b1100,
        LE = 0b1101,
        AL = 0b1110,
        NV = 0b1111,
    };


    // Helper method to convert register numbers to strings
    std::string regName(uint32_t reg) const;

    // Get the current instruction address (for label resolution)
    size_t getCurrentAddress() const;

    // Resolve a branch instruction's offset
    void resolveBranch(size_t instructionIndex, int32_t offset);
    void addUnresolvedBranch(const std::string& label);
    void resolveBranches(const LabelManager& labelManager);

    void clear();
    Instruction& at(size_t index);
    size_t size() const;

    std::vector<Instruction>& getInstructions() { return instructions; }
    const std::vector<Instruction>& getInstructions() const { return instructions; }
};

#endif // AARCH64_INSTRUCTIONS_H