//! # BCPL AST Printer
//!
//! This module provides a human-readable, indented printer for the
//! Abstract Syntax Tree (AST). It's useful for debugging the parser
//! and for understanding the structure of a BCPL program.

use super::{Program, TopLevel, Declaration, Statement, Expression, LValue, LiteralExpr, UnaryOp, BinaryOp, BlockItem};

// --- Public Functions ---

/// Prints a complete AST (`Program`) to the console.
pub fn print_ast(program: &Program) {
    println!("\n--- Abstract Syntax Tree ---");
    let mut printer = AstPrinter { indent: 0 };
    printer.print_program(program);
    println!("--- End Abstract Syntax Tree ---");
}

// --- AST Printer Implementation ---

struct AstPrinter {
    indent: usize,
    
}

impl AstPrinter {
    fn print_indented(&self, text: &str) {
        println!("{}{}", "| ".repeat(self.indent), text);
    }

    fn indented<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    fn print_program(&mut self, program: &Program) {
        self.print_indented("Program");
        self.indented(|p| {
            for toplevel in program {
                p.print_toplevel(toplevel);
            }
        });
    }

    fn print_toplevel(&mut self, toplevel: &TopLevel) {
        match toplevel {
            TopLevel::Declaration(decl) => self.print_declaration(decl),
            TopLevel::Get(filename) => self.print_indented(&format!("Get(\"{}\")", filename)),
            TopLevel::Manifest(consts) => {
                self.print_indented("Manifest");
                self.indented(|p| {
                    for (name, value_expr) in consts {
                        p.print_indented(&format!("Constant: {}", name));
                        p.indented(|p2| {
                            p2.print_expression(value_expr);
                        });
                    }
                });
            }
            TopLevel::Global(globals) => {
                self.print_indented("Global");
                self.indented(|p| {
                    for (name, offset) in globals {
                        p.print_indented(&format!("{}: {:?}", name, offset));

                    }
                });
            }
            TopLevel::Static(statics) => {
                self.print_indented("Static");
                self.indented(|p| {
                    for (name, value) in statics {
                        p.print_indented(&format!("{} = {:?}", name, value));
                    }
                });
            }
            TopLevel::Section { name, body } => {
                self.print_indented(&format!("Section: \"{}\"", name));
                self.indented(|p| {
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
        }
    }

    fn print_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Let { names, values } => {
                self.print_indented(&format!("LetDecl: {}", names.join(", ")));
                self.indented(|p| for value in values { p.print_expression(value); });
            }
            Declaration::Function { name, params, body } => {
                self.print_indented(&format!("FunctionDecl: {}({})", name, params.join(", ")));
                self.indented(|p| {
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_expression(body));
                });
            }
            Declaration::Routine { name, params, body } => {
                self.print_indented(&format!("RoutineDecl: {}({})", name, params.join(", ")));
                self.indented(|p| {
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
            Declaration::And(decls) => {
                self.print_indented("And");
                self.indented(|p| for d in decls { p.print_declaration(d) });
            }
        }
    }

    fn print_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Block { items } => {
                self.print_indented("Block");
                self.indented(|p| {
                    for item in items {
                        match item {
                            BlockItem::Declaration(decl) => p.print_declaration(decl),
                            BlockItem::Statement(stmt) => p.print_statement(stmt),
                        }
                    }
                });
            }
            Statement::RoutineCall(expr) => {
                self.print_indented("RoutineCall");
                self.indented(|p| p.print_expression(expr));
            }
            Statement::Assignment { lvalues, rvalues } => {
                self.print_indented("Assignment");
                self.indented(|p| {
                    p.print_indented("LValues:");
                    p.indented(|p2| for lv in lvalues { p2.print_lvalue(lv) });
                    p.print_indented("RValues:");
                    p.indented(|p2| for rv in rvalues { p2.print_expression(rv) });
                });
            }
            Statement::If { condition, then_branch } => {
                self.print_indented("If");
                self.indented(|p| {
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Then:");
                    p.indented(|p2| p2.print_statement(then_branch));
                });
            }
            Statement::Unless { condition, body } => {
                self.print_indented("Unless");
                self.indented(|p| {
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
            Statement::Test { condition, then_branch, else_branch } => {
                self.print_indented("Test");
                self.indented(|p| {
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Then:");
                    p.indented(|p2| p2.print_statement(then_branch));
                    p.print_indented("Else:");
                    p.indented(|p2| p2.print_statement(else_branch));
                });
            }
            Statement::While { condition, body } => {
                self.print_indented("While");
                self.indented(|p| {
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
            Statement::Until { condition, body } => {
                self.print_indented("Until");
                self.indented(|p| {
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
            Statement::For { var, from, to, by, body } => {
                self.print_indented(&format!("For(var: {})", var));
                self.indented(|p| {
                    p.print_indented("From:");
                    p.indented(|p2| p2.print_expression(from));
                    p.print_indented("To:");
                    p.indented(|p2| p2.print_expression(to));
                    if let Some(by_expr) = by {
                        p.print_indented("By:");
                        p.indented(|p2| p2.print_expression(by_expr));
                    }
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                });
            }
            Statement::Repeat(body) => {
                self.print_indented("Repeat");
                self.indented(|p| p.print_statement(body));
            }
            Statement::RepeatWhile { body, condition } => {
                self.print_indented("RepeatWhile");
                self.indented(|p| {
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                });
            }
            Statement::RepeatUntil { body, condition } => {
                self.print_indented("RepeatUntil");
                self.indented(|p| {
                    p.print_indented("Body:");
                    p.indented(|p2| p2.print_statement(body));
                    p.print_indented("Condition:");
                    p.indented(|p2| p2.print_expression(condition));
                });
            }
            Statement::Switch { expr, cases, default } => {
                self.print_indented("Switch");
                self.indented(|p| {
                    p.print_indented("Expression:");
                    p.indented(|p2| p2.print_expression(expr));
                    p.print_indented("Cases:");
                    p.indented(|p2| {
                        for (values, body) in cases {
                            let value_str = values.iter().map(|v| format!("{:?}", v)).collect::<Vec<_>>().join(", ");
                            p2.print_indented(&format!("Case: {}", value_str));
                            p2.indented(|p3| p3.print_statement(body));
                        }
                    });
                    if let Some(default_body) = default {
                        p.print_indented("Default:");
                        p.indented(|p2| p2.print_statement(default_body));
                    }
                });
            }
            Statement::Resultis(expr) => {
                self.print_indented("Resultis");
                self.indented(|p| p.print_expression(expr));
            }
            Statement::Break => self.print_indented("Break"),
            Statement::Loop => self.print_indented("Loop"),
            Statement::Goto(label) => self.print_indented(&format!("Goto({})", label)),
            Statement::LabeledStatement { label, statement } => {
                self.print_indented(&format!("Label: {}", label));
                self.indented(|p| p.print_statement(statement));
            }
            Statement::Return => self.print_indented("Return"),
            Statement::Finish => self.print_indented("Finish"),
        }
    }

    fn print_lvalue(&mut self, lvalue: &LValue) {
        match lvalue {
            LValue::Name(name) => self.print_indented(&format!("Name({})", name)),
            LValue::Indirection(expr) => {
                self.print_indented("Indirection");
                self.indented(|p| p.print_expression(expr));
            }
            LValue::Subscript { base, index, op } => {
                self.print_indented(&format!("Subscript({:?})", op));
                self.indented(|p| {
                    p.print_indented("Base:");
                    p.indented(|p2| p2.print_expression(base));
                    p.print_indented("Index:");
                    p.indented(|p2| p2.print_expression(index));
                });
            }
        }
    }

    fn print_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal(lit) => match lit {
                LiteralExpr::Number(n) => self.print_indented(&format!("Literal(Number: {})", n)),
                LiteralExpr::FloatNumber(f) => self.print_indented(&format!("Literal(Float: {})", f)),
                LiteralExpr::String(s) => self.print_indented(&format!("Literal(String: \"{}\")", s)),
                LiteralExpr::Char(c) => self.print_indented(&format!("Literal(Char: '{}')", c)),
                LiteralExpr::True => self.print_indented("Literal(True)"),
                LiteralExpr::False => self.print_indented("Literal(False)"),
            },

            Expression::Variable(name) => self.print_indented(&format!("Variable({})", name)),
            Expression::UnaryOp { op, operand } => {
                self.print_indented(&format!("UnaryOp({:?})", op));
                self.indented(|p| p.print_expression(operand));
            }
            Expression::BinaryOp { left, op, right } => {
                self.print_indented(&format!("BinaryOp({:?})", op));
                self.indented(|p| { p.print_expression(left); p.print_expression(right); });
            }
            // MODIFIED: This now prints a more compact and readable summary for function calls.
            Expression::FunctionCall { callee, args } => {
                let callee_name = match &**callee {
                    Expression::Variable(name) => name.clone(),
                    _ => "[complex callee]".to_string(),
                };

                self.print_indented(&format!("FunctionCall: {}", callee_name));
                self.indented(|p| {
                    p.print_indented("Args:");
                    p.indented(|p2| {
                        if args.is_empty() {
                            p2.print_indented("(none)");
                        } else {
                            for arg in args {
                                p2.print_expression(arg);
                            }
                        }
                    });
                });
            }
            Expression::Conditional { condition, true_expr, false_expr } => {
                self.print_indented("Conditional");
                self.indented(|p| {
                    p.print_indented("If:");
                    p.indented(|p2| p2.print_expression(condition));
                    p.print_indented("Then:");
                    p.indented(|p2| p2.print_expression(true_expr));
                    p.print_indented("Else:");
                    p.indented(|p2| p2.print_expression(false_expr));
                });
            }
            Expression::Vec(size) => {
                self.print_indented("Vec");
                self.indented(|p| p.print_expression(size));
            }
            Expression::Valof(body) => {
                self.print_indented("Valof");
                self.indented(|p| p.print_statement(body));
            }
            Expression::Table(elements) => {
                self.print_indented("Table");
                self.indented(|p| for el in elements { p.print_expression(el); });
            }
        }
    }
}
