// Declare the sub-modules of codegen
pub mod aarch64;
pub mod regalloc;
pub mod instruction;

// A placeholder function for the codegen entry point
pub fn generate() -> Result<(), ()> {
    println!("-> codegen::generate()");
    aarch64::generate_code();
    Ok(())
}
