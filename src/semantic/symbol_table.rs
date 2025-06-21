//! # BCPL Symbol Table and Memory Representation
//!
//! This module defines the core data structures used by the semantic analyzer
//! to represent the program's memory layout, symbols, and other metadata
//! needed for code generation.

use crate::parser::Program;
use std::collections::HashMap;

// --- High-Level Program Representation ---

/// The final output of the semantic analysis phase.
/// This is a complete, annotated representation of the program.
#[derive(Debug, Default)]
pub struct ProgramRepresentation {
    pub sections: HashMap<String, SectionRepresentation>,
    pub ast: Program,
}

/// Represents a single, self-contained compilation unit (a SECTION).
#[derive(Debug, Default)]
pub struct SectionRepresentation {
    pub name: String,
    pub functions: HashMap<String, FunctionInfo>,
    pub data_segment: DataSegment,
}

impl ProgramRepresentation {
    /// Prints a human-readable report of the program's memory layout.
    pub fn report(&self) {
        println!("\n--- Semantic Analysis Report ---");

        let mut sorted_sections: Vec<_> = self.sections.values().collect();
        sorted_sections.sort_by_key(|s| &s.name);

        for section in sorted_sections {
            println!("\n========================================");
            println!("SECTION: {}", section.name);
            println!("========================================");

            // --- Function Report ---
            println!("\n[Functions]");
            if section.functions.is_empty() {
                println!("(No functions defined in this section)");
            } else {
                println!("{:<20} | {:<10} | {:<20}", "Name", "Arity", "Stack Frame Size");
                println!("{:-<20}-|-{:-<10}-|-{:-<20}", "", "", "");
                let mut sorted_functions: Vec<_> = section.functions.values().collect();
                sorted_functions.sort_by_key(|f| &f.name);
                for f in sorted_functions {
                    println!("{:<20} | {:<10} | {} bytes", f.name, f.arity, f.stack_frame_size);
                    // NEW: Report on the functions that this function calls.
                    if !f.calls.is_empty() {
                        let calls_str = f.calls.join(", ");
                        println!("{:<20}   Calls: {}", "", calls_str);
                    }
                }
            }

            // --- Data Segment Report ---
            println!("\n[Data Segment]");
            if section.data_segment.buffer.is_empty() {
                println!("(No data allocated in this section)");
            } else {
                println!("Total Size: {} bytes", section.data_segment.buffer.len());
                println!("{:<10} | {:<10} | {:<30}", "Offset", "Length", "Content");
                println!("{:-<10}-|-{:-<10}-|-{:-<30}", "", "", "");

                let mut sorted_strings: Vec<_> = section.data_segment.string_literals.iter().collect();
                sorted_strings.sort_by_key(|&(_, offset)| *offset);

                for (content, offset) in sorted_strings {
                    let escaped_content = content.replace('\n', "\\n");
                    let display_content = if escaped_content.len() > 25 {
                        format!("{}...", &escaped_content[..25])
                    } else {
                        escaped_content
                    };
                    let len = content.as_bytes().len() + 4;
                    println!("0x{:08x} | {:<10} | \"{}\"", offset, len, display_content);
                }
            }
        }
        println!("\n--- End of Report ---");
    }
}


/// Metadata for a single compiled function or routine.
#[derive(Debug)]
pub struct FunctionInfo {
    pub name: String,
    pub arity: usize,
    pub stack_frame_size: usize,
    // NEW: A list of function names that this function calls.
    pub calls: Vec<String>,
}

// --- Data & Memory Layout Structures ---

#[derive(Debug, Default)]
pub struct DataSegment {
    pub string_literals: HashMap<String, usize>,
    pub float_literals: HashMap<u64, usize>,
    pub buffer: Vec<u8>,
}

impl DataSegment {
    pub fn new() -> Self { Self::default() }

    pub fn add_string(&mut self, content: &str) -> usize {
        if let Some(offset) = self.string_literals.get(content) {
            return *offset;
        }
        let offset = self.buffer.len();
        self.buffer.extend_from_slice(content.as_bytes());
        self.buffer.extend_from_slice(&[0, 0, 0, 0]); // Null terminator
        self.string_literals.insert(content.to_string(), offset);
        offset
    }
}

// --- Symbol Table Structures ---

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolKind {
    Local { offset: usize },
    ManifestConstant { value: i64 },
    Function { arity: usize },
    Routine { arity: usize },
    Literal { location_id: usize },
    Export,
    Import,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self { SymbolTable { scopes: vec![HashMap::new()] } }
    pub fn enter_scope(&mut self) { self.scopes.push(HashMap::new()); }
    pub fn leave_scope(&mut self) { if self.scopes.len() > 1 { self.scopes.pop(); } }

    pub fn declare(&mut self, symbol: Symbol) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().expect("Scope stack is empty.");
        if current_scope.contains_key(&symbol.name) {
            return Err(format!("Symbol '{}' already declared in this scope.", symbol.name));
        }
        current_scope.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}
