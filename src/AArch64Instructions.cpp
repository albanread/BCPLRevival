#include "AArch64Instructions.h"
#include "LabelManager.h"
#include <stdexcept>
#include <sstream>
#include <iomanip>
#include <algorithm> // For std::sort

// Define static members
const uint32_t AArch64Instructions::X0;
const uint32_t AArch64Instructions::X1;
const uint32_t AArch64Instructions::X2;
const uint32_t AArch64Instructions::X28;
const uint32_t AArch64Instructions::X29;
const uint32_t AArch64Instructions::X30;
const uint32_t AArch64Instructions::SP;
const uint32_t AArch64Instructions::XZR;

void AArch64Instructions::setPendingLabel(const std::string& label) {
    pendingLabel_ = label;
}

void AArch64Instructions::addInstruction(Instruction instr) {
    if (!pendingLabel_.empty()) {
        instr.hasLabel = true;
        instr.label = pendingLabel_;
        pendingLabel_.clear();
    }
    instructions.push_back(instr);
}

void AArch64Instructions::mov(uint32_t rd, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0xAA0003E0 | (rm << 16) | rd;
    addInstruction({encoding, "mov " + regName(rd) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::movz(uint32_t rd, uint16_t imm16, uint8_t shift, const std::string& comment) {
    uint32_t encoding = 0xD2800000 | (static_cast<uint32_t>(shift) << 21) | (imm16 << 5) | rd;
    addInstruction({encoding, "movz " + regName(rd) + ", #0x" +
                         std::to_string(imm16) +
                         (shift ? ", lsl #" + std::to_string(shift * 16) : ""), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::movk(uint32_t rd, uint16_t imm16, uint8_t shift, const std::string& comment) {
    uint32_t encoding = 0xF2800000 | (static_cast<uint32_t>(shift) << 21) | (imm16 << 5) | rd;
    addInstruction({encoding, "movk " + regName(rd) + ", #0x" +
                         std::to_string(imm16) +
                         (shift ? ", lsl #" + std::to_string(shift * 16) : ""), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::add(uint32_t rd, uint32_t rn, uint32_t rm, ShiftType shift_type, uint32_t shift_amount, const std::string& comment) {
    uint32_t encoding = 0x8B000000 | (rm << 16) | (rn << 5) | rd;
    // Add shift encoding
    switch (shift_type) {
        case LSL: encoding |= (0b00 << 22); break;
        case LSR: encoding |= (0b01 << 22); break;
        case ASR: encoding |= (0b10 << 22); break;
        case ROR: encoding |= (0b11 << 22); break;
    }
    encoding |= (shift_amount << 10);

    std::string shift_str;
    if (shift_amount > 0) {
        switch (shift_type) {
            case LSL: shift_str = ", lsl #" + std::to_string(shift_amount); break;
            case LSR: shift_str = ", lsr #" + std::to_string(shift_amount); break;
            case ASR: shift_str = ", asr #" + std::to_string(shift_amount); break;
            case ROR: shift_str = ", ror #" + std::to_string(shift_amount); break;
        }
    }
    addInstruction({encoding, "add " + regName(rd) + ", " + regName(rn) + ", " + regName(rm) + shift_str, comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::add(uint32_t rd, uint32_t rn, uint32_t imm, const std::string& comment) {
    uint32_t encoding = 0x91000000 | (imm << 10) | (rn << 5) | rd;
    addInstruction({encoding, "add " + regName(rd) + ", " + regName(rn) + ", #" + std::to_string(imm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::sub(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0xCB000000 | (rm << 16) | (rn << 5) | rd;
    addInstruction({encoding, "sub " + regName(rd) + ", " + regName(rn) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::mul(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0x9B007C00 | (rm << 16) | (rn << 5) | rd;
    addInstruction({encoding, "mul " + regName(rd) + ", " + regName(rn) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::sdiv(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment) {
    addInstruction({0x9AC00C00 | (rm << 16) | (rn << 5) | rd, "sdiv " + regName(rd) + ", " + regName(rn) + ", " + regName(rm), comment});
}

void AArch64Instructions::lsl(uint32_t rd, uint32_t rn, uint32_t imm, const std::string& comment) {
    addInstruction({0xD3400000 | (imm << 10) | (rn << 5) | rd, "lsl " + regName(rd) + ", " + regName(rn) + ", #" + std::to_string(imm), comment});
}

void AArch64Instructions::msub(uint32_t rd, uint32_t rn, uint32_t rm, uint32_t ra, const std::string& comment) {
    addInstruction({0x9B000000 | (rm << 16) | (ra << 10) | (rn << 5) | rd, "msub " + regName(rd) + ", " + regName(rn) + ", " + regName(rm) + ", " + regName(ra), comment});
}

std::string AArch64Instructions::regName(uint32_t reg) const {
    if (reg >= 0 && reg <= 30) {
        return "x" + std::to_string(reg);
    } else if (reg == SP) {
        return "sp";
    } else if (reg == XZR) {
        return "xzr";
    }
    return "unknown";
}

size_t AArch64Instructions::getCurrentAddress() const {
    return instructions.size() * 4; // Each instruction is 4 bytes
}

void AArch64Instructions::stp(uint32_t rt1, uint32_t rt2, uint32_t rn, int32_t imm, const std::string& comment) {
    uint32_t encoding = 0xA9000000 | ((imm & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
    addInstruction({encoding, "stp " + regName(rt1) + ", " + regName(rt2) + ", [" +
                         regName(rn) + ", #" + std::to_string(imm) + "]", comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::ldp(uint32_t rt1, uint32_t rt2, uint32_t rn, int32_t imm, const std::string& comment) {
    uint32_t encoding = 0xA9400000 | ((imm & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
    addInstruction({encoding, "ldp " + regName(rt1) + ", " + regName(rt2) + ", [" +
                         regName(rn) + ", #" + std::to_string(imm) + "]", comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::str(uint32_t rt, uint32_t rn, int32_t imm, const std::string& comment) {
    uint32_t encoding = 0xF9000000 | ((imm / 8) << 10) | (rn << 5) | rt;
    addInstruction({encoding, "str " + regName(rt) + ", [" + regName(rn) +
                         (imm != 0 ? ", #" + std::to_string(imm) : "") + "]", comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::ldr(uint32_t rt, uint32_t rn, int32_t imm, const std::string& comment) {
    uint32_t encoding = 0xF9400000 | ((imm / 8) << 10) | (rn << 5) | rt;
    addInstruction({encoding, "ldr " + regName(rt) + ", [" + regName(rn) +
                         (imm != 0 ? ", #" + std::to_string(imm) : "") + "]", comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::b(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x14000000;
    addInstruction({encoding, "b " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::bl(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x94000000;
    addInstruction({encoding, "bl " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::ret(const std::string& comment) {
    uint32_t encoding = 0xD65F03C0;
    addInstruction({encoding, "ret", comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::adr(uint32_t rd, const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x10000000 | rd; // Placeholder, will be patched
    addInstruction({encoding, "adr " + regName(rd) + ", " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::br(uint32_t rn, const std::string& comment) {
    uint32_t encoding = 0xD61F0000 | (rn << 5);
    addInstruction({encoding, "br " + regName(rn), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::loadImmediate(uint32_t rd, int64_t value, const std::string& comment) {
    std::stringstream ss;
    ss << "Loading " << value << " into " << regName(rd);
    std::string baseComment = comment.empty() ? ss.str() : comment;

    if (value >= 0 && value < 65536) {
        movz(rd, value & 0xFFFF, 0, baseComment);
    } else {
        movz(rd, value & 0xFFFF, 0, baseComment + " (low)");
        if (value & 0xFFFF0000) {
            movk(rd, (value >> 16) & 0xFFFF, 1, baseComment + " (high)");
        }
        if (value & 0xFFFF00000000) {
            movk(rd, (value >> 32) & 0xFFFF, 2, baseComment + " (upper)");
        }
        if (value & 0xFFFF000000000000) {
            movk(rd, (value >> 48) & 0xFFFF, 3, baseComment + " (top)");
        }
    }
}

void AArch64Instructions::neg(uint32_t rd, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0xCB0003E0 | (rm << 16) | rd; // SUB rd, XZR, rm
    addInstruction({encoding, "neg " + regName(rd) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::eor(uint32_t rd, uint32_t rn, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0xCA000000 | (rm << 16) | (rn << 5) | rd;
    addInstruction({encoding, "eor " + regName(rd) + ", " + regName(rn) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::cmp(uint32_t rn, uint32_t rm, const std::string& comment) {
    uint32_t encoding = 0xEB00001F | (rm << 16) | (rn << 5); // SUBS XZR, rn, rm
    addInstruction({encoding, "cmp " + regName(rn) + ", " + regName(rm), comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::beq(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000; // B.EQ
    addInstruction({encoding, "b.eq " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::bne(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000 | 0b0001; // B.NE (condition code 0b0001)
    addInstruction({encoding, "b.ne " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::bge(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000 | 0b1010; // B.GE (condition code 0b1010)
    addInstruction({encoding, "b.ge " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::blt(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000 | 0b1011; // B.LT (condition code 0b1011)
    addInstruction({encoding, "b.lt " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::ble(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000 | 0b1101; // B.LE (condition code 0b1101)
    addInstruction({encoding, "b.le " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::bgt(const std::string& label, const std::string& comment) {
    uint32_t encoding = 0x54000000 | 0b1100; // B.GT (condition code 0b1100)
    addInstruction({encoding, "b.gt " + label, comment, true, label, getCurrentAddress()});
}

void AArch64Instructions::cset(uint32_t rd, uint32_t cond, const std::string& comment) {
    uint32_t encoding = 0x9A9F0000 | (cond << 12) | rd;
    std::string condStr;
    switch (cond) {
        case EQ: condStr = "eq"; break;
        case NE: condStr = "ne"; break;
        case CS: condStr = "cs"; break;
        case CC: condStr = "cc"; break;
        case MI: condStr = "mi"; break;
        case PL: condStr = "pl"; break;
        case VS: condStr = "vs"; break;
        case VC: condStr = "vc"; break;
        case HI: condStr = "hi"; break;
        case LS: condStr = "ls"; break;
        case GE: condStr = "ge"; break;
        case LT: condStr = "lt"; break;
        case GT: condStr = "gt"; break;
        case LE: condStr = "le"; break;
        case AL: condStr = "al"; break;
        case NV: condStr = "nv"; break;
        default: condStr = "unknown"; break;
    }
    addInstruction({encoding, "cset " + regName(rd) + ", " + condStr, comment, false, "", getCurrentAddress()});
}

void AArch64Instructions::resolveBranch(size_t instructionIndex, int32_t offset) {
    if (instructionIndex >= instructions.size()) {
        throw std::runtime_error("Invalid instruction index for branch resolution");
    }

    Instruction& instr = instructions[instructionIndex];
    uint32_t opcode = instr.encoding & 0xFC000000; // Extract top 6 bits

    if (opcode == 0x14000000) { // B instruction
        instr.encoding |= ((offset / 4) & 0x03FFFFFF);
    } else if ((opcode & 0xFE000000) == 0x54000000) { // Conditional branch (B.cond)
        instr.encoding |= ((offset / 4) & 0x0007FFFF);
    } else if (opcode == 0x94000000) { // BL instruction
        instr.encoding |= ((offset / 4) & 0x03FFFFFF);
    } else {
        throw std::runtime_error("Attempted to resolve non-branch instruction");
    }
    instr.needsLabelResolution = false;
}

void AArch64Instructions::addUnresolvedBranch(const std::string& label) {
    // Placeholder implementation
}

void AArch64Instructions::resolveBranches(const LabelManager& labelManager) {
    // Placeholder implementation
}

void AArch64Instructions::clear() {
    instructions.clear();
}

AArch64Instructions::Instruction& AArch64Instructions::at(size_t index) {
    return instructions.at(index);
}

size_t AArch64Instructions::size() const {
    return instructions.size();
}
