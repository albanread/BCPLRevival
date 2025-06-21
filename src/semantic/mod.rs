//! # BCPL Semantic Analyzer
//!
//! This module walks the AST to check for semantic correctness and to build a
//! concrete representation of the program's memory layout. It uses the data
//! structures defined in the `symbol_table` module.

use crate::parser::{
    BinaryOp, BlockItem, Declaration, Expression, LValue, LiteralExpr, Program, Statement,
    TopLevel, UnaryOp,
};
use crate::semantic::stack_frame::StackFrame;

use std::collections::HashMap;

// --- Module Declarations ---
pub mod stack_frame;
pub mod symbol_table;

use symbol_table::{
    DataSegment, FunctionInfo, ProgramRepresentation, SectionRepresentation, Symbol, SymbolKind,
    SymbolTable,
};

// --- Semantic Analyzer ---

/// The main struct for the semantic analysis pass.
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<String>,

    sections: HashMap<String, SectionRepresentation>,

    // State for the *current* section being analyzed.
    current_section_name: Option<String>,
    current_functions: HashMap<String, FunctionInfo>,
    current_data_segment: DataSegment,

    // Replace current_stack_offset with StackFrame
    current_stack_frame: Option<StackFrame>,

    // State for the *current* function within the section.
    current_function_name: Option<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            sections: HashMap::new(),
            current_functions: HashMap::new(),
            current_data_segment: DataSegment::new(),
            current_function_name: None,
            current_section_name: None,
            current_stack_frame: None,
        };
        analyzer.populate_builtins();
        analyzer
    }

    pub fn print_current_stack_frame(&self) {
        if let Some(frame) = &self.current_stack_frame {
            if let Some(function_name) = &self.current_function_name {
                println!("Stack frame for function '{}':", function_name);
            } else {
                println!("Current stack frame:");
            }
            frame.print();
        } else {
            println!("No current stack frame.");
        }
    }

    // Add this helper method to handle GET directives
    fn handle_get_directive(&mut self, filename: &str) {
        // Add to dependencies list
        if let Some(info) = self.current_functions.get_mut("start") {
            info.calls.push("GET".to_string());
        }

        // Standard library headers should be validated
        if filename == "libhdr" {
            // This is valid, libhdr is the standard BCPL header
            return;
        }
        // For other files, we should check if they exist
        // but for now just record it as a dependency
    }

    fn handle_static_decl(&mut self, names: &[(String, LiteralExpr)]) {
        for (name, init_expr) in names {
            // Static variables are section-local
            let symbol = Symbol {
                name: name.clone(),
                kind: SymbolKind::Static {
                    offset: self.current_data_segment.buffer.len(),
                },
            };

            if let Err(e) = self.symbol_table.declare(symbol) {
                self.errors.push(e);
            }

            // Wrap the LiteralExpr in an Expression::Literal
            self.visit_expression(&Expression::Literal(init_expr.clone()));

            // Reserve space in data segment
            self.current_data_segment.buffer.extend_from_slice(&[0; 8]);
        }
    }

    // Add support for Global declarations
    fn handle_global_decl(&mut self, names: &[(String, Option<Expression>)]) {
        for (name, init_expr) in names {
            // Global variables are stored in a special section of memory
            let symbol = Symbol {
                name: name.clone(),
                kind: SymbolKind::Global {
                    offset: self.current_data_segment.buffer.len(),
                },
            };

            if let Err(e) = self.symbol_table.declare(symbol) {
                self.errors.push(e);
            }

            // Check initialization expression if present
            if let Some(expr) = init_expr {
                self.visit_expression(expr);
            }

            // Reserve space in data segment
            self.current_data_segment.buffer.extend_from_slice(&[0; 8]);
        }
    }

    /// The main entry point. Consumes the parser's AST and produces a
    /// `ProgramRepresentation` or a list of errors.
    pub fn analyze(&mut self, program: &Program) -> Result<ProgramRepresentation, Vec<String>> {
        let has_explicit_sections = program
            .iter()
            .any(|item| matches!(item, TopLevel::Section { .. }));

        if !has_explicit_sections {
            self.begin_section("_main");
        }

        for item in program {
            self.visit_toplevel(item);
        }

        self.finalize_current_section();

        if self.errors.is_empty() {
            Ok(ProgramRepresentation {
                sections: std::mem::take(&mut self.sections),
                ast: program.clone(),
            })
        } else {
            Err(self.errors.clone())
        }
    }

    // --- Visitor and Helper Methods ---

    fn populate_builtins(&mut self) {
        let builtins = vec![
            ("writes", 1, false),
            ("writen", 1, false),
            ("wrch", 1, false),
            ("readn", 0, true),
            ("rdch", 0, true),
            ("findinput", 1, true),
            ("findoutput", 1, true),
            ("selectinput", 1, false),
            ("selectoutput", 1, false),
            ("endread", 0, false),
            ("endwrite", 0, false),
        ];

        for (name, arity, is_function) in builtins {
            let kind = if is_function {
                SymbolKind::Function { arity }
            } else {
                SymbolKind::Routine { arity }
            };
            let symbol = Symbol {
                name: name.to_string(),
                kind,
            };
            self.symbol_table.declare(symbol).unwrap();
        }
    }

    fn begin_section(&mut self, name: &str) {
        self.finalize_current_section();
        self.current_section_name = Some(name.to_string());
        self.symbol_table.enter_scope();
    }

    fn finalize_current_section(&mut self) {
        if let Some(name) = self.current_section_name.take() {
            let section_rep = SectionRepresentation {
                name: name.clone(),
                functions: std::mem::take(&mut self.current_functions),
                data_segment: std::mem::take(&mut self.current_data_segment),
            };
            self.sections.insert(name, section_rep);
            self.symbol_table.leave_scope();
        }
    }

    fn visit_toplevel(&mut self, item: &TopLevel) {
        match item {
            TopLevel::Section { name, body } => {
                self.begin_section(name);
                self.visit_statement(body);
            }
            TopLevel::Declaration(decl) => self.visit_declaration(decl),
            TopLevel::Manifest(consts) => {
                for (name, expr) in consts {
                    match self.fold_constant_expression(expr) {
                        Ok(value) => {
                            let symbol = Symbol {
                                name: name.clone(),
                                kind: SymbolKind::ManifestConstant { value },
                            };
                            if let Err(e) = self.symbol_table.declare(symbol) {
                                self.errors.push(e);
                            }
                        }
                        Err(e) => self.errors.push(e),
                    }
                }
            }
            TopLevel::Get(filename) => self.handle_get_directive(filename),
            TopLevel::Global(decls) => self.handle_global_decl(decls),
            TopLevel::Static(decls) => self.handle_static_decl(decls),
        }
    }

    fn visit_block(&mut self, items: &[BlockItem]) {
        self.symbol_table.enter_scope();
        for item in items {
            match item {
                BlockItem::Declaration(decl) => self.visit_declaration(decl),
                BlockItem::Statement(stmt) => self.visit_statement(stmt),
            }
        }
        self.symbol_table.leave_scope();
    }

    fn visit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Let { names, values } => {
                for value in values {
                    self.visit_expression(value);
                }
                for name in names {
                    // Create stack frame if it doesn't exist
                    if self.current_stack_frame.is_none() {
                        self.current_stack_frame = Some(StackFrame::new(8));
                    }

                    // Safely get the next offset from the stack frame
                    let offset: i32 = self
                        .current_stack_frame
                        .as_mut()
                        .expect("Stack frame should exist")
                        .allocate_local(name, 8)
                        .try_into()
                        .expect("Negative offset encountered");

                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Local {
                            offset: offset.try_into().expect("Negative offset encountered")
                        },
                    };


                    if let Err(e) = self.symbol_table.declare(symbol) {
                        self.errors.push(e);
                    }
                }
            }

            Declaration::Routine { name, params, body } => {
                let routine_symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Routine {
                        arity: params.len(),
                    },
                };
                if let Err(e) = self.symbol_table.declare(routine_symbol) {
                    self.errors.push(e);
                }

                let saved_function_name = self.current_function_name.clone();
                self.current_function_name = Some(name.clone());

                self.current_functions.insert(
                    name.clone(),
                    FunctionInfo {
                        name: name.clone(),
                        arity: params.len(),
                        stack_frame_size: 0,
                        calls: Vec::new(),
                    },
                );

                // Save the current stack frame and create a new one for this routine
                let saved_stack_frame = self.current_stack_frame.take();
                self.current_stack_frame = Some(StackFrame::new(8));

                self.symbol_table.enter_scope();

                // Handle parameters
                for param in params {
                    let offset = self
                        .current_stack_frame
                        .as_mut()
                        .expect("Stack frame should exist")
                        .allocate_local(param, 8);

                    let param_symbol = Symbol {
                        name: param.clone(),
                        kind: SymbolKind::Local { offset },
                    };
                    if let Err(e) = self.symbol_table.declare(param_symbol) {
                        self.errors.push(e);
                    }
                }

                self.visit_statement(body);
                println!("\nFinal stack frame for routine '{}':", name);
                self.print_current_stack_frame();

                self.symbol_table.leave_scope();

                // Update function info with stack frame size
                if let Some(info) = self.current_functions.get_mut(name) {
                    info.stack_frame_size = self
                        .current_stack_frame
                        .as_ref()
                        .map(|sf| sf.get_frame_size())
                        .unwrap_or(0);
                }

                // Restore the previous stack frame
                self.current_stack_frame = saved_stack_frame;
                self.current_function_name = saved_function_name;
            }

            Declaration::Function { .. } => { /* TODO */ }
            Declaration::And(decls) => {
                for d in decls {
                    self.visit_declaration(d);
                }
            }
        }
    }

    // MODIFIED: Completed implementation to visit all statement types.
    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::For {
                var,
                from,
                to,
                by,
                body,
            } => {
                self.visit_expression(from);
                self.visit_expression(to);
                if let Some(by_expr) = by {
                    self.visit_expression(by_expr);
                }

                self.symbol_table.enter_scope();

                // Allocate space for loop variable
                let offset = self
                    .current_stack_frame
                    .as_mut()
                    .expect("Stack frame should exist")
                    .allocate_local(var, 8);

                let symbol = Symbol {
                    name: var.clone(),
                    kind: SymbolKind::Local { offset },
                };
                if let Err(e) = self.symbol_table.declare(symbol) {
                    self.errors.push(e);
                }

                self.visit_statement(body);
                self.symbol_table.leave_scope();
            }

            Statement::Block { items } => self.visit_block(items),
            Statement::Assignment { lvalues, rvalues } => {
                for lval in lvalues {
                    self.visit_lvalue(lval);
                }
                for rval in rvalues {
                    self.visit_expression(rval);
                }
            }
            Statement::RoutineCall(expr) => self.visit_expression(expr),
            Statement::If {
                condition,
                then_branch,
            } => {
                self.visit_expression(condition);
                self.visit_statement(then_branch);
            }
            Statement::Unless { condition, body } => {
                self.visit_expression(condition);
                self.visit_statement(body);
            }
            Statement::Test {
                condition,
                then_branch,
                else_branch,
            } => {
                self.visit_expression(condition);
                self.visit_statement(then_branch);
                self.visit_statement(else_branch);
            }
            Statement::While { condition, body } => {
                self.visit_expression(condition);
                self.visit_statement(body);
            }
            Statement::Until { condition, body } => {
                self.visit_expression(condition);
                self.visit_statement(body);
            }

            Statement::Repeat(body) => self.visit_statement(body),
            Statement::RepeatWhile { body, condition } => {
                self.visit_statement(body);
                self.visit_expression(condition);
            }
            Statement::RepeatUntil { body, condition } => {
                self.visit_statement(body);
                self.visit_expression(condition);
            }
            Statement::Switch {
                expr,
                cases,
                default,
            } => {
                self.visit_expression(expr);
                for (_, case_body) in cases {
                    self.visit_statement(case_body);
                }
                if let Some(default_body) = default {
                    self.visit_statement(default_body);
                }
            }
            Statement::Resultis(expr) => self.visit_expression(expr),
            Statement::Goto(_)
            | Statement::Break
            | Statement::Loop
            | Statement::Return
            | Statement::Finish => {
                // No sub-nodes to visit for these.
            }
            Statement::LabeledStatement { statement, .. } => self.visit_statement(statement),
        }
    }

    fn visit_lvalue(&mut self, lval: &LValue) {
        match lval {
            LValue::Name(name) => {
                if self.symbol_table.lookup(name).is_none() {
                    self.errors
                        .push(format!("Cannot assign to undeclared variable '{}'.", name));
                }
            }
            LValue::Indirection(expr) => self.visit_expression(expr),
            LValue::Subscript { base, index, .. } => {
                self.visit_expression(base);
                self.visit_expression(index);
            }
        }
    }

    // MODIFIED: Completed implementation to visit all expression types.
    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal(lit) => {
                match lit {
                    LiteralExpr::String(s) => {
                        self.current_data_segment.add_string(s);
                    }
                    LiteralExpr::FloatNumber(f) => {
                        // Validate float literal and store it in the data segment if needed
                        if !f.is_finite() {
                            self.errors
                                .push(format!("Invalid floating point literal: {}", f));
                        }
                        // Add to data segment if needed
                        self.current_data_segment
                            .buffer
                            .extend_from_slice(&f.to_le_bytes());
                    }
                    _ => { /* Other literals (Number, Char, True, False) don't need special handling */
                    }
                }
            }

            Expression::Variable(name) => {
                if self.symbol_table.lookup(name).is_none() {
                    self.errors
                        .push(format!("Use of undeclared identifier '{}'.", name));
                }
            }
            Expression::UnaryOp { operand, .. } => self.visit_expression(operand),
            Expression::BinaryOp { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::FunctionCall { callee, args } => {
                self.visit_expression(callee);
                for arg in args {
                    self.visit_expression(arg);
                }

                if let Expression::Variable(callee_name) = &**callee {
                    if let Some(current_function_name) = &self.current_function_name {
                        if let Some(function_info) =
                            self.current_functions.get_mut(current_function_name)
                        {
                            if !function_info.calls.contains(callee_name) {
                                function_info.calls.push(callee_name.clone());
                            }
                        }
                    }

                    if let Some(symbol) = self.symbol_table.lookup(callee_name) {
                        match symbol.kind {
                            SymbolKind::Function { arity } | SymbolKind::Routine { arity } => {
                                if args.len() != arity {
                                    self.errors.push(format!(
                                        "Function '{}' expects {} arguments, but got {}.",
                                        callee_name,
                                        arity,
                                        args.len()
                                    ));
                                }
                            }
                            _ => self.errors.push(format!(
                                "'{}' is not a function or routine and cannot be called.",
                                callee_name
                            )),
                        }
                    }
                }
            }
            Expression::Conditional {
                condition,
                true_expr,
                false_expr,
            } => {
                self.visit_expression(condition);
                self.visit_expression(true_expr);
                self.visit_expression(false_expr);
            }
            Expression::Valof(stmt) => self.visit_statement(stmt),
            Expression::Vec(size_expr) => self.visit_expression(size_expr),
            Expression::Table(elements) => {
                for el in elements {
                    self.visit_expression(el);
                }
            }
        }
    }

    fn fold_constant_expression(&self, expr: &Expression) -> Result<i64, String> {
        match expr {
            Expression::Literal(LiteralExpr::Number(n)) => Ok(*n),
            Expression::BinaryOp { left, op, right } => {
                let l = self.fold_constant_expression(left)?;
                let r = self.fold_constant_expression(right)?;
                match op {
                    BinaryOp::Add => Ok(l.wrapping_add(r)),
                    BinaryOp::Sub => Ok(l.wrapping_sub(r)),
                    BinaryOp::Mul => Ok(l.wrapping_mul(r)),
                    BinaryOp::Div => Ok(l.wrapping_div(r)),
                    _ => Err("Unsupported operator in constant expression.".to_string()),
                }
            }
            Expression::Variable(name) => {
                if let Some(symbol) = self.symbol_table.lookup(name) {
                    if let SymbolKind::ManifestConstant { value } = symbol.kind {
                        Ok(value)
                    } else {
                        Err(format!("'{}' is not a manifest constant.", name))
                    }
                } else {
                    Err(format!(
                        "Use of undeclared identifier '{}' in constant expression.",
                        name
                    ))
                }
            }
            _ => Err("This expression form is not valid in a manifest constant.".to_string()),
        }
    }
}
