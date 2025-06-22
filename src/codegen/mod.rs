// codegen/mod.rs

// Declare the sub-modules of codegen and make them public within the crate.
pub mod aarch64;
pub mod instruction;
pub mod regalloc;

// Use the public items from the sub-modules and the semantic analysis stage.
use crate::semantic::symbol_table::ProgramRepresentation;
use aarch64::Aarch64CodeGenerator;
use instruction::Instruction;

/// The public entry point for the entire code generation stage.
///
/// This function orchestrates the transformation from the semantic representation
/// of the program into a linear list of machine instructions.
///
/// It now takes the `ProgramRepresentation` from the semantic analyzer
/// as input.
pub fn generate(representation: &ProgramRepresentation) -> Vec<Instruction> {
    // 1. Create a new instance of our AArch64 code generator.
    let mut generator = Aarch64CodeGenerator::new();

    // 2. Call the generator's main method, passing the semantic representation
    //    so it has access to all necessary information like stack frames.
    //    We clone the result because the generator owns the instruction list,
    //    and we are returning a new owned Vec.
    let instructions = generator.generate(representation).clone();

    // 3. Return the final list of instructions to the caller (lib.rs).
    instructions
}
