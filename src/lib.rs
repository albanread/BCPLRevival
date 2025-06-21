//! # BCPL JIT Compiler Library
//!
//! This crate contains all the core logic for the compiler, organized into
//! separate modules for each compilation stage.

// Declare the top-level modules.
pub mod runtime;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod codegen;

// A public function that will orchestrate the compilation process.
// This is the main entry point into our library.
pub fn compile_and_run(source_code: &str) {
    println!("Compiler invoked with source:");
    println!("{}", source_code);

    // 1. Lexing
    let tokens = match lexer::scan(source_code) {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexing Error at line {}, col {}: {}", e.line, e.col, e.message);
            return;
        }
    };
    println!("\n1. Lexer finished. Found {} tokens.", tokens.len());
    // lexer::print_tokens(&tokens); // Can be noisy, uncomment for debugging.

    // 2. Parsing
    let ast = match parser::parse(&tokens) {
        Ok(ast) => {
            println!("\n2. Parser finished. AST created successfully.");
            parser::print_ast(&ast);
            ast // Return the AST for the next stage
        }
        Err(errors) => {
            eprintln!("\nParsing failed with {} error(s):", errors.len());
            for e in errors {
                eprintln!("- {}", e);
            }
            return;
        }
    };

    // 3. Semantic Analysis
    let mut analyzer = semantic::SemanticAnalyzer::new();
    let representation = match analyzer.analyze(&ast) {
        Ok(rep) => {
            println!("\n3. Semantic analysis passed.");
            // NEW: Call the report method to print the summary.
            rep.report();
            rep // Return the representation for the next stage
        }
        Err(errors) => {
            eprintln!("\nSemantic analysis failed with {} error(s):", errors.len());
            for e in errors {
                eprintln!("- {}", e);
            }
            return;
        }
    };

    // 4. Code Generation (Placeholder)
    println!("\n4. Code Generation finished. (Skipped)");
    // codegen::generate(&representation).expect("Code generation failed.");


    // 5. Runtime Execution (Placeholder)
    println!("\n5. Runtime finished. (Skipped)");
    // runtime::execute().expect("Runtime execution failed.");
}
