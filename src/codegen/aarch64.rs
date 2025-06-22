// codegen/aarch64.rs

// Import the necessary definitions from our other modules and the project's semantic stage.
use super::instruction::{Instruction, Operand, Register};
use crate::semantic::symbol_table::{ProgramRepresentation, SectionRepresentation, FunctionInfo};

/// Generates AArch64 machine code from a semantic representation.
/// This struct will hold the state for the code generation process.
pub struct Aarch64CodeGenerator {
    instructions: Vec<Instruction>,
    // The RegisterAllocator would go here.
}

impl Aarch64CodeGenerator {
    /// Creates a new, empty code generator.
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    /// A helper function to add a new instruction to our list.
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    /// Generates the standard function prologue based on the AAPCS64 ABI.
    /// This sets up the stack frame for a new function.
    /// The `frame_size` is calculated during semantic analysis.
    fn generate_prologue(&mut self, frame_size: u32) {
        // 1. Push the Frame Pointer (x29) and Link Register (x30) to the stack.
        self.emit(Instruction::STP {
            rt1: Register::P, // FP (x29)
            rt2: Register::LR,
            addr: Operand::MemOffset { base: Register::SP, offset: -16 },
        });

        // 2. Set the new Frame Pointer to the current Stack Pointer.
        self.emit(Instruction::MOV {
            rd: Register::P,
            rn: Operand::Reg(Register::SP),
        });

        // 3. Allocate space on the stack for local variables if needed.
        if frame_size > 0 {
            self.emit(Instruction::SUB {
                rd: Register::SP,
                rn: Register::SP,
                rm: Operand::Imm(frame_size as i64),
            });
        }
    }

    /// Generates the standard function epilogue.
    /// This restores the previous stack frame and returns to the caller.
    fn generate_epilogue(&mut self) {
        // 1. Deallocate stack space by moving the Frame Pointer back to the Stack Pointer.
        self.emit(Instruction::MOV {
            rd: Register::SP,
            rn: Operand::Reg(Register::P),
        });

        // 2. Restore the caller's Frame Pointer and Link Register from the stack.
        self.emit(Instruction::LDP {
            rt1: Register::P,
            rt2: Register::LR,
            addr: Operand::MemOffset { base: Register::SP, offset: 16 },
        });

        // 3. Return to the address stored in the Link Register.
        self.emit(Instruction::RET);
    }

    /// The main entry point for the generator. It takes the semantic representation
    /// of the program and returns a list of instructions.
    pub fn generate(
        &mut self,
        representation: &ProgramRepresentation
    ) -> &Vec<Instruction> {
        // Iterate over all sections in the program (e.g., "_main")
        for section in representation.sections.values() {
            self.visit_section(section);
        }
        &self.instructions
    }

    /// Visits a program section and generates code for all its functions.
    fn visit_section(&mut self, section: &SectionRepresentation) {
        // Iterate over all functions defined in this section.
        for function in section.functions.values() {
            self.visit_function(function);
        }
    }

    /// Generates code for a single function.
    fn visit_function(&mut self, function: &FunctionInfo) {
        // 1. Get the required stack frame size from the function's info.
        let frame_size = function.stack_frame.get_frame_size() as u32;

        // 2. Generate the function prologue with the correct size.
        self.generate_prologue(frame_size);

        // --- TODO: Code generation for the function body will go here ---

        // 3. Generate the function epilogue.
        self.generate_epilogue();
    }
}
