// codegen/instruction.rs

/// Represents a single AArch64 general-purpose 64-bit register.
/// The mapping from BCPL virtual registers is included for clarity.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Register {
    X0, X1, X2, X3, X4, X5, X6, X7, X8,
    X9, X10, X11, X12, X13, X14, X15, X16, X17, X18,
    X19, X20, X21, X22, X23, X24, X25, X26, X27, X28,
    X29, X30,
    SP, // Stack Pointer
    XZR, // Zero Register
}

/// Static mappings from Cintcode registers to ARM64 registers.
impl Register {
    pub const A: Register = Register::X0;       // Accumulator, arg1, return value [cite: armcode.md]
    pub const B: Register = Register::X1;       // Second accumulator, arg2 [cite: armcode.md]
    pub const C: Register = Register::X2;       // Char/Work, arg3 [cite: armcode.md]
    pub const P: Register = Register::X29;      // Frame Pointer [cite: armcode.md]
    pub const G: Register = Register::X28;      // Global Vector Pointer [cite: armcode.md]
    pub const ST: Register = Register::X27;     // Status Register [cite: armcode.md]
    pub const COUNT: Register = Register::X26;  // Debug Count Register [cite: armcode.md]
    pub const MW: Register = Register::X25;     // Memory Word for 64-bit extension [cite: armcode.md]
    pub const LR: Register = Register::X30;     // Link Register [cite: armcode.md]
}

/// Represents a single AArch64 floating-point/SIMD register.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum FPRegister {
    D0, D1, D2, D3, D4, D5, D6, D7, // FP args and return
    // ... up to D31
}

/// Represents the condition for conditional instructions (e.g., B.cond, CSET).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Condition {
    EQ, // Equal
    NE, // Not Equal
    LT, // Less Than (signed)
    LE, // Less Than or Equal (signed)
    GT, // Greater Than (signed)
    GE, // Greater Than or Equal (signed)
}

/// Represents an operand for an instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Reg(Register),
    Imm(i64),
    MemOffset { base: Register, offset: i32 },
    MemIndexed { base: Register, index: Register, scale: u8 },
    Label(String),
}

/// Represents a single AArch64 instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    // --- Data Processing ---
    MOV { rd: Register, rn: Operand },          // Move value to register
    ADD { rd: Register, rn: Register, rm: Operand }, // Addition
    SUB { rd: Register, rn: Register, rm: Operand }, // Subtraction
    MUL { rd: Register, rn: Register, rm: Register }, // Multiplication (A := B * A)
    SDIV { rd: Register, rn: Register, rm: Register },// Signed Division (A := B / A)
    CMP { rn: Register, rm: Operand },          // Compare
    CSET { rd: Register, cond: Condition },     // Set register to 1 or 0 based on condition
    NEG { rd: Register, rn: Register },          // Negate (A := -A)
    MVN { rd: Register, rn: Register },          // Bitwise NOT (A := ~A)
    AND { rd: Register, rn: Register, rm: Register }, // Bitwise AND
    ORR { rd: Register, rn: Register, rm: Register }, // Bitwise OR
    EOR { rd: Register, rn: Register, rm: Register }, // Bitwise XOR
    LSL { rd: Register, rn: Register, rm: Operand }, // Logical Shift Left
    LSR { rd: Register, rn: Register, rm: Operand }, // Logical Shift Right

    // --- Added Instructions ---
    MLS { rd: Register, rn: Register, rm: Register, ra: Register }, // Multiply-Subtract, for MOD
    MOVZ { rd: Register, imm: u16, shift: u8 }, // Move with Zero (for large immediates)
    MOVK { rd: Register, imm: u16, shift: u8 }, // Move with Keep (for large immediates)
    SBFX { rd: Register, rn: Register, lsb: u8, width: u8 }, // Signed Bitfield Extract
    UBFX { rd: Register, rn: Register, lsb: u8, width: u8 }, // Unsigned Bitfield Extract
    BFI  { rd: Register, rn: Register, lsb: u8, width: u8 }, // Bitfield Insert

    // --- Memory Access ---
    ADRP { rd: Register, label: String },       // Load page address of a label
    LDR { rt: Register, addr: Operand },        // Load 64-bit word
    LDRW { rt: Register, addr: Operand },       // Load 32-bit word (for characters)
    STR { rt: Register, addr: Operand },        // Store 64-bit word
    STRW { rt: Register, addr: Operand },       // Store 32-bit word (for characters)
    STP { rt1: Register, rt2: Register, addr: Operand }, // Store Pair of registers
    LDP { rt1: Register, rt2: Register, addr: Operand }, // Load Pair of registers

    // --- Control Flow ---
    B { label: String },                        // Unconditional branch
    BL { label: String },                       // Branch with Link (function call)
    BLR { rn: Register },                       // Branch with Link to Register (indirect call)
    RET,                                        // Return from subroutine
    Bcond { cond: Condition, label: String },   // Conditional Branch
    BRK,                                        // Breakpoint

    // --- Floating Point ---
    FADD { rd: FPRegister, rn: FPRegister, rm: FPRegister }, // FP Add
    FSUB { rd: FPRegister, rn: FPRegister, rm: FPRegister }, // FP Subtract
    FMUL { rd: FPRegister, rn: FPRegister, rm: FPRegister }, // FP Multiply
    FDIV { rd: FPRegister, rn: FPRegister, rm: FPRegister }, // FP Divide
    FCVTZS { rd: Register, rn: FPRegister },    // FP Convert to Signed Integer (Trunc)
    SCVTF { rd: FPRegister, rn: Register },     // Signed Integer Convert to FP
    FCMP { rn: FPRegister, rm: FPRegister },    // FP Compare
    FNEG { rd: FPRegister, rn: FPRegister },    // FP Negate
    FABS { rd: FPRegister, rn: FPRegister },    // FP Absolute Value
}

// --- Helper Functions for Assembly Formatting ---

fn format_reg(r: Register) -> String {
    match r {
        Register::SP => "sp".to_string(),
        Register::XZR => "xzr".to_string(),
        Register::P => "fp".to_string(), // Use fp for frame pointer
        Register::LR => "lr".to_string(), // Use lr for link register
        _ => format!("x{}", r as u8),
    }
}

fn format_fpreg(r: FPRegister) -> String {
    format!("d{}", r as u8)
}

fn format_operand(o: &Operand) -> String {
    match o {
        Operand::Reg(r) => format_reg(*r),
        Operand::Imm(i) => format!("#{}", i),
        Operand::MemOffset { base, offset } => format!("[{}, #{}]", format_reg(*base), offset),
        Operand::MemIndexed { base, index, scale } => {
            format!("[{}, {}, lsl #{}]", format_reg(*base), format_reg(*index), scale)
        }
        Operand::Label(l) => l.clone(),
    }
}

fn format_cond(c: Condition) -> &'static str {
    match c {
        Condition::EQ => "eq",
        Condition::NE => "ne",
        Condition::LT => "lt",
        Condition::LE => "le",
        Condition::GT => "gt",
        Condition::GE => "ge",
    }
}


impl Instruction {
    /// Produces a human-readable assembly language string for the instruction.
    pub fn to_asm(&self) -> String {
        match self {
            // Data Processing
            Instruction::MOV { rd, rn } => format!("\tmov\t\t{}, {}", format_reg(*rd), format_operand(rn)),
            Instruction::ADD { rd, rn, rm } => format!("\tadd\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_operand(rm)),
            Instruction::SUB { rd, rn, rm } => format!("\tsub\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_operand(rm)),
            Instruction::MUL { rd, rn, rm } => format!("\tmul\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm)),
            Instruction::SDIV { rd, rn, rm } => format!("\tsdiv\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm)),
            Instruction::CMP { rn, rm } => format!("\tcmp\t\t{}, {}", format_reg(*rn), format_operand(rm)),
            Instruction::CSET { rd, cond } => format!("\tcset\t{}, {}", format_reg(*rd), format_cond(*cond)),
            Instruction::NEG { rd, rn } => format!("\tneg\t\t{}, {}", format_reg(*rd), format_reg(*rn)),
            Instruction::MVN { rd, rn } => format!("\tmvn\t\t{}, {}", format_reg(*rd), format_reg(*rn)),
            Instruction::AND { rd, rn, rm } => format!("\tand\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm)),
            Instruction::ORR { rd, rn, rm } => format!("\torr\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm)),
            Instruction::EOR { rd, rn, rm } => format!("\teor\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm)),
            Instruction::LSL { rd, rn, rm } => format!("\tlsl\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_operand(rm)),
            Instruction::LSR { rd, rn, rm } => format!("\tlsr\t\t{}, {}, {}", format_reg(*rd), format_reg(*rn), format_operand(rm)),

            // Added
            Instruction::MLS { rd, rn, rm, ra } => format!("\tmls\t\t{}, {}, {}, {}", format_reg(*rd), format_reg(*rn), format_reg(*rm), format_reg(*ra)),
            Instruction::MOVZ { rd, imm, shift } => format!("\tmovz\t{}, #{}, lsl #{}", format_reg(*rd), imm, shift),
            Instruction::MOVK { rd, imm, shift } => format!("\tmovk\t{}, #{}, lsl #{}", format_reg(*rd), imm, shift),
            Instruction::SBFX { rd, rn, lsb, width } => format!("\tsbfx\t{}, {}, #{}, #{}", format_reg(*rd), format_reg(*rn), lsb, width),
            Instruction::UBFX { rd, rn, lsb, width } => format!("\tubfx\t{}, {}, #{}, #{}", format_reg(*rd), format_reg(*rn), lsb, width),
            Instruction::BFI { rd, rn, lsb, width } => format!("\tbfi\t\t{}, {}, #{}, #{}", format_reg(*rd), format_reg(*rn), lsb, width),

            // Memory
            Instruction::ADRP { rd, label } => format!("\tadrp\t{}, {}", format_reg(*rd), label),
            Instruction::LDR { rt, addr } => format!("\tldr\t\t{}, {}", format_reg(*rt), format_operand(addr)),
            Instruction::LDRW { rt, addr } => format!("\tldrw\t{}, {}", format_reg(*rt), format_operand(addr)),
            Instruction::STR { rt, addr } => format!("\tstr\t\t{}, {}", format_reg(*rt), format_operand(addr)),
            Instruction::STRW { rt, addr } => format!("\tstrw\t{}, {}", format_reg(*rt), format_operand(addr)),
            Instruction::STP { rt1, rt2, addr } => format!("\tstp\t\t{}, {}, {}", format_reg(*rt1), format_reg(*rt2), format_operand(addr)),
            Instruction::LDP { rt1, rt2, addr } => format!("\tldp\t\t{}, {}, {}", format_reg(*rt1), format_reg(*rt2), format_operand(addr)),

            // Control Flow
            Instruction::B { label } => format!("\tb\t\t{}", label),
            Instruction::BL { label } => format!("\tbl\t\t{}", label),
            Instruction::BLR { rn } => format!("\tblr\t\t{}", format_reg(*rn)),
            Instruction::RET => "\tret".to_string(),
            Instruction::Bcond { cond, label } => format!("\tb.{}\t{}", format_cond(*cond), label),
            Instruction::BRK => "\tbrk\t\t#0".to_string(),

            // Floating Point
            Instruction::FADD { rd, rn, rm } => format!("\tfadd\t{}, {}, {}", format_fpreg(*rd), format_fpreg(*rn), format_fpreg(*rm)),
            Instruction::FSUB { rd, rn, rm } => format!("\tfsub\t{}, {}, {}", format_fpreg(*rd), format_fpreg(*rn), format_fpreg(*rm)),
            Instruction::FMUL { rd, rn, rm } => format!("\tfmul\t{}, {}, {}", format_fpreg(*rd), format_fpreg(*rn), format_fpreg(*rm)),
            Instruction::FDIV { rd, rn, rm } => format!("\tfdiv\t{}, {}, {}", format_fpreg(*rd), format_fpreg(*rn), format_fpreg(*rm)),
            Instruction::FCVTZS { rd, rn } => format!("\tfcvtzs\t{}, {}", format_reg(*rd), format_fpreg(*rn)),
            Instruction::SCVTF { rd, rn } => format!("\tscvtf\t{}, {}", format_fpreg(*rd), format_reg(*rn)),
            Instruction::FCMP { rn, rm } => format!("\tfcmp\t{}, {}", format_fpreg(*rn), format_fpreg(*rm)),
            Instruction::FNEG { rd, rn } => format!("\tfneg\t{}, {}", format_fpreg(*rd), format_fpreg(*rn)),
            Instruction::FABS { rd, rn } => format!("\tfabs\t{}, {}", format_fpreg(*rd), format_fpreg(*rn)),
        }
    }

    /// Encodes the Instruction enum into its 32-bit binary representation.
    pub fn encode(&self) -> u32 {
        // TODO: Implement encoding logic.
        0 // Placeholder
    }
}