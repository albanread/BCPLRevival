use super::regalloc; // Use the sibling module

pub fn generate_code() {
    println!("----> codegen::aarch64::generate_code()");
    regalloc::allocate_registers();
}

