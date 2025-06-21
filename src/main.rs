use std::env;
use std::fs;

fn main() {
    println!("--- BCPL JIT Compiler ---");

    // Get the filename from command line args or use default
    let filename = env::args()
        .nth(1)
        .unwrap_or_else(|| "test.b".to_string());

    // Read the source file
    let source = fs::read_to_string(&filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    // Use the library crate we are a part of
    use bcpl_jit::compile_and_run;
    compile_and_run(&source);

    println!("\n--- Compilation Finished ---");
}