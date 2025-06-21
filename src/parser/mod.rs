//! # BCPL Parser
//!
//! This module takes a stream of tokens from the lexer and constructs
//! an Abstract Syntax Tree (AST). The AST is the hierarchical representation
//! of the program's structure, which will be used by the code generator.
//!
//! The parser uses a combination of recursive descent for statements and
//! a Pratt parser for expressions, which elegantly handles operator precedence.
//! It also includes error recovery to report multiple errors in one pass.

use crate::lexer::{Token, TokenType};
use std::fmt;

// --- Module Declarations ---
pub mod parser_print;
mod parser_utility;

// --- Abstract Syntax Tree (AST) Node Definitions ---

/// A complete program is a collection of top-level definitions.
pub type Program = Vec<TopLevel>;

/// Represents a single item within a block, which can be either a
/// declaration or a statement. This preserves the original source order.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

/// Represents a top-level item in a BCPL program.
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Declaration(Declaration),
    Get(String),
    Manifest(Vec<(String, Expression)>),
    Global(Vec<(String, i64)>),
    Static(Vec<(String, LiteralExpr)>),
    // NEW: Represents a SECTION "name" BE <statement> construct.
    Section { name: String, body: Box<Statement> },
}

/// Represents a `LET` or `AND` declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Let {
        names: Vec<String>,
        values: Vec<Expression>,
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Box<Expression>,
    },
    Routine {
        name: String,
        params: Vec<String>,
        body: Box<Statement>,
    },
    And(Vec<Declaration>),
}

/// Represents a command or statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // MODIFIED: A Block is now a single vector of `BlockItem`s to preserve order.
    Block {
        items: Vec<BlockItem>,
    },
    Assignment {
        lvalues: Vec<LValue>,
        rvalues: Vec<Expression>,
    },
    RoutineCall(Expression),
    If {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
    },
    Unless {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    Test {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
        else_branch: Box<Statement>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    Until {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    For {
        var: String,
        from: Box<Expression>,
        to: Box<Expression>,
        by: Option<Box<Expression>>, // BY is optional
        body: Box<Statement>,
    },
    Repeat(Box<Statement>),
    RepeatWhile {
        body: Box<Statement>,
        condition: Box<Expression>,
    },
    RepeatUntil {
        body: Box<Statement>,
        condition: Box<Expression>,
    },
    Switch {
        expr: Box<Expression>,
        cases: Vec<(Vec<LiteralExpr>, Box<Statement>)>, // Multiple constants can share a case
        default: Option<Box<Statement>>,
    },
    Return,
    Finish,
    Resultis(Box<Expression>),
    Goto(String),
    Break,
    Loop,
    LabeledStatement {
        label: String,
        statement: Box<Statement>,
    },
}

/// Represents an expression that yields a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpr),
    Variable(String),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },
    FunctionCall {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Conditional {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
    Valof(Box<Statement>),
    Vec(Box<Expression>),
    Table(Vec<Expression>),
}

/// Represents a value that can be assigned to (left-hand side of `:=`).
#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Name(String),
    Indirection(Box<Expression>),
    Subscript {
        base: Box<Expression>,
        index: Box<Expression>,
        op: BinaryOp,
    },
}


/// Represents a literal value.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Number(i64),
    FloatNumber(f64),
    String(String),
    Char(char),
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
    Negate,
    Address,
    Deref,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Rem,
    FloatAdd, FloatSub, FloatMul, FloatDiv,
    Eq, NotEq, Less, LessEq, Greater, GreaterEq,
    FloatEq, FloatNotEq, FloatLess, FloatLessEq, FloatGreater, FloatGreaterEq,
    And, Or, Eqv, NotEqv,
    ShiftLeft, ShiftRight,
    Subscript,
    StringSub,
    FloatVecSub,
}

/// Represents a syntax error found during parsing.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse Error at {},{}: {}", self.line, self.col, self.message)
    }
}

// --- Pratt Parser Precedence ---

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
    None,
    Assignment,
    Conditional,
    Or,
    And,
    Equality,
    Term,
    Factor,
    Shift,
    Unary,
    Call,
    Subscript,
    Primary,
}

// --- Public Functions ---

/// The main entry point for the parser.
pub fn parse(tokens: &[Token]) -> Result<Program, Vec<ParseError>> {
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();
    if parser.errors.is_empty() {
        Ok(program)
    } else {
        Err(parser.errors)
    }
}

/// A public wrapper for the AST printing function.
pub fn print_ast(program: &Program) {
    parser_print::print_ast(program);
}

// --- Parser Struct and Grammar-Parsing Methods ---
struct Parser<'a> {
    pub(super) tokens: &'a [Token],
    pub(super) current: usize,
    pub(super) errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0, errors: Vec::new() }
    }

    // --- Main Parsing Logic ---

    fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();
        while !self.is_at_end() {
            match self.parse_toplevel() {
                Ok(toplevel) => program.push(toplevel),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }
        program
    }

    /// Skips tokens until a likely recovery point (a new top-level directive) is found.
    fn synchronize(&mut self) {
        while !self.is_at_end() {
            match self.peek().kind {
                TokenType::Let | TokenType::Manifest | TokenType::Get | TokenType::Static | TokenType::Global | TokenType::Section => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Parses a top-level directive (e.g., LET, GET, MANIFEST).
    fn parse_toplevel(&mut self) -> Result<TopLevel, ParseError> {
        match self.peek().kind {
            TokenType::Let => Ok(TopLevel::Declaration(self.parse_declaration_chain()?)),
            TokenType::Manifest => self.parse_manifest(),
            TokenType::Global => self.parse_global(),
            TokenType::Static => self.parse_static(),
            TokenType::Get => self.parse_get(),
            // NEW: Handle SECTION keyword at the top level.
            TokenType::Section => self.parse_section(),
            _ => Err(self.error_at_current("Expected a top-level directive (LET, MANIFEST, GET, SECTION, etc.).")),
        }
    }

    // NEW: Parses a SECTION directive.
    fn parse_section(&mut self) -> Result<TopLevel, ParseError> {
        self.consume(TokenType::Section, "Expect 'SECTION'")?;
        let name = self.consume_string_literal("Expect section name in quotes.")?;
        self.consume(TokenType::Be, "Expect 'BE' after section name.")?;
        let body = self.parse_statement()?;
        Ok(TopLevel::Section { name, body: Box::new(body) })
    }

    fn parse_get(&mut self) -> Result<TopLevel, ParseError> {
        self.consume(TokenType::Get, "Expect 'GET'")?;
        if let TokenType::StringLiteral(filename) = self.peek().kind.clone() {
            self.advance();
            Ok(TopLevel::Get(filename))
        } else {
            Err(self.error_at_current("GET directive requires a string literal filename."))
        }
    }

    fn parse_manifest(&mut self) -> Result<TopLevel, ParseError> {
        self.consume(TokenType::Manifest, "Expect 'MANIFEST'")?;

        let mut constants = Vec::new();
        if self.match_token(TokenType::SectionStart) {
            while !self.check(TokenType::SectionEnd) && !self.is_at_end() {
                let name = self.consume_identifier("Expect constant name.")?;
                self.consume(TokenType::Eq, "Expect '=' after constant name.")?;
                let value_expr = self.parse_expression(Precedence::None)?;
                constants.push((name, value_expr));
                self.match_token(TokenType::Semicolon);
            }
            self.consume(TokenType::SectionEnd, "Expect '$)' or '}' to end MANIFEST block.")?;
        } else {
            loop {
                let name = self.consume_identifier("Expect constant name.")?;
                self.consume(TokenType::Eq, "Expect '=' after constant name.")?;
                let value_expr = self.parse_expression(Precedence::None)?;
                constants.push((name, value_expr));
                if !self.match_token(TokenType::Comma) { break; }
            }
        }
        Ok(TopLevel::Manifest(constants))
    }

    fn parse_global(&mut self) -> Result<TopLevel, ParseError> {
        self.consume(TokenType::Global, "Expect 'GLOBAL'")?;
        self.consume(TokenType::SectionStart, "Expect '$(' after GLOBAL")?;
        let mut globals = Vec::new();
        while !self.check(TokenType::SectionEnd) && !self.is_at_end() {
            let name = self.consume_identifier("Expect global variable name.")?;
            self.consume(TokenType::Colon, "Expect ':' separating global name and offset.")?;
            let offset = self.consume_integer("Expect integer offset for global.")?;
            globals.push((name, offset));

            if !self.check(TokenType::SectionEnd) {
                self.consume(TokenType::Semicolon, "Expect ';' to separate GLOBAL declarations.")?;
            }
        }
        self.consume(TokenType::SectionEnd, "Expect '$)' to end GLOBAL block.")?;
        Ok(TopLevel::Global(globals))
    }

    fn parse_static(&mut self) -> Result<TopLevel, ParseError> {
        self.consume(TokenType::Static, "Expect 'STATIC'")?;
        self.consume(TokenType::SectionStart, "Expect '$(' after STATIC")?;
        let mut statics = Vec::new();
        while !self.check(TokenType::SectionEnd) && !self.is_at_end() {
            let (name, value) = self.parse_name_equals_literal("static variable")?;
            statics.push((name, value));
            if !self.check(TokenType::SectionEnd) {
                self.consume(TokenType::Semicolon, "Expect ';' to separate STATIC declarations.")?;
            }
        }
        self.consume(TokenType::SectionEnd, "Expect '$)' to end STATIC block.")?;
        Ok(TopLevel::Static(statics))
    }

    fn parse_name_equals_literal(&mut self, item_kind: &str) -> Result<(String, LiteralExpr), ParseError> {
        let name = self.consume_identifier(&format!("Expect {} name.", item_kind))?;
        self.consume(TokenType::Eq, &format!("Expect '=' after {} name.", item_kind))?;
        let literal_expr = match self.parse_expression(Precedence::None)? {
            Expression::Literal(lit) => lit,
            _ => return Err(self.error_at_current(&format!("{} value must be a literal.", item_kind))),
        };
        Ok((name, literal_expr))
    }

    fn parse_declaration_chain(&mut self) -> Result<Declaration, ParseError> {
        let mut decls = vec![self.parse_declaration()?];
        while self.match_token(TokenType::And) {
            decls.push(self.parse_declaration()?);
        }

        if decls.len() == 1 {
            Ok(decls.pop().unwrap())
        } else {
            Ok(Declaration::And(decls))
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        self.consume(TokenType::Let, "Expect 'LET' to begin a declaration.")?;
        let name = self.consume_identifier("Expect identifier for declaration name.")?;

        if self.match_token(TokenType::LParen) {
            let params = self.parse_parameter_list()?;
            self.consume(TokenType::RParen, "Expect ')' after parameters.")?;

            if self.match_token(TokenType::Be) {
                let body = self.parse_statement()?;
                Ok(Declaration::Routine { name, params, body: Box::new(body) })
            } else {
                self.consume(TokenType::Eq, "Expect '=' for function body or 'BE' for routine.")?;
                let body = self.parse_expression(Precedence::None)?;
                Ok(Declaration::Function { name, params, body: Box::new(body) })
            }
        } else {
            let mut names = vec![name];
            while self.match_token(TokenType::Comma) {
                names.push(self.consume_identifier("Expect identifier in declaration list.")?);
            }

            self.consume(TokenType::Eq, "Expect '=' after variable name(s).")?;
            let mut values = vec![self.parse_expression(Precedence::None)?];
            while self.match_token(TokenType::Comma) {
                values.push(self.parse_expression(Precedence::None)?);
            }

            if names.len() != values.len() {
                return Err(self.error_at_current("Mismatched number of names and values in declaration."));
            }
            Ok(Declaration::Let { names, values })
        }
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<String>, ParseError> {
        let mut params = Vec::new();
        if !self.check(TokenType::RParen) {
            loop {
                params.push(self.consume_identifier("Expected parameter name")?);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        Ok(params)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if let TokenType::Identifier(_) = self.peek().kind {
            if let TokenType::Colon = self.peek_next().kind {
                let label = self.consume_identifier("Expect label name.")?;
                self.consume(TokenType::Colon, "Expect ':' after label.")?;
                let statement = self.parse_statement()?;
                return Ok(Statement::LabeledStatement { label, statement: Box::new(statement) });
            }
        }

        let mut statement = self.parse_primary_statement()?;

        if self.match_token(TokenType::Repeat) {
            statement = Statement::Repeat(Box::new(statement));
        } else if self.match_token(TokenType::RepeatUntil) {
            let condition = self.parse_expression(Precedence::None)?;
            statement = Statement::RepeatUntil { body: Box::new(statement), condition: Box::new(condition) };
        } else if self.match_token(TokenType::RepeatWhile) {
            let condition = self.parse_expression(Precedence::None)?;
            statement = Statement::RepeatWhile { body: Box::new(statement), condition: Box::new(condition) };
        }

        Ok(statement)
    }

    fn parse_primary_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek().kind {
            TokenType::If => self.parse_if_statement(),
            TokenType::Unless => self.parse_unless_statement(),
            TokenType::Test => self.parse_test_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Until => self.parse_until_statement(),
            TokenType::For => self.parse_for_statement(),
            TokenType::Switchon => self.parse_switchon_statement(),
            TokenType::Goto => self.parse_goto_statement(),
            TokenType::Resultis => self.parse_resultis_statement(),
            TokenType::Return => { self.advance(); Ok(Statement::Return) },
            TokenType::Break => { self.advance(); Ok(Statement::Break) },
            TokenType::Loop => { self.advance(); Ok(Statement::Loop) },
            TokenType::Finish => { self.advance(); Ok(Statement::Finish) },
            TokenType::SectionStart => self.parse_block_statement(),
            _ => self.parse_assignment_or_call_statement(),
        }
    }

    fn parse_assignment_or_call_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::None)?;

        let mut expressions = vec![expr];
        while self.match_token(TokenType::Comma) {
            expressions.push(self.parse_expression(Precedence::None)?);
        }

        if self.match_token(TokenType::Assign) {
            let mut rvalues = vec![self.parse_expression(Precedence::None)?];
            while self.match_token(TokenType::Comma) {
                rvalues.push(self.parse_expression(Precedence::None)?);
            }

            if expressions.len() != rvalues.len() {
                return Err(self.error_at_current("Mismatched number of lvalues and rvalues in assignment."));
            }

            let lvalues = expressions.into_iter()
                .map(|e| self.expression_to_lvalue(e))
                .collect::<Result<Vec<_>,_>>()?;

            Ok(Statement::Assignment { lvalues, rvalues })

        } else if expressions.len() == 1 {
            Ok(Statement::RoutineCall(expressions.pop().unwrap()))
        } else {
            Err(self.error_at_current("Expected ':=' for multi-assignment or a single routine call."))
        }
    }

    fn parse_goto_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Goto, "Expect 'GOTO'")?;
        let label = self.consume_identifier("Expect label name for GOTO.")?;
        Ok(Statement::Goto(label))
    }

    fn parse_resultis_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Resultis, "Expected 'RESULTIS'")?;
        let value = self.parse_expression(Precedence::None)?;
        Ok(Statement::Resultis(Box::new(value)))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::If, "Expect 'IF'")?;
        let condition = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Then, "Expect 'THEN' after IF condition.")?;
        let then_branch = self.parse_statement()?;
        Ok(Statement::If { condition: Box::new(condition), then_branch: Box::new(then_branch) })
    }

    fn parse_unless_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Unless, "Expect 'UNLESS'")?;
        let condition = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Do, "Expect 'DO' after UNLESS condition.")?;
        let body = self.parse_statement()?;
        Ok(Statement::Unless { condition: Box::new(condition), body: Box::new(body) })
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::For, "Expected 'FOR'")?;
        let var = self.consume_identifier("Expected loop variable name.")?;
        self.consume(TokenType::Eq, "Expected '=' after loop variable.")?;
        let from = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::To, "Expected 'TO' in FOR loop.")?;
        let to = self.parse_expression(Precedence::None)?;
        let by = if self.match_token(TokenType::By) {
            Some(Box::new(self.parse_expression(Precedence::None)?))
        } else {
            None
        };
        self.consume(TokenType::Do, "Expected 'DO' in FOR loop.")?;
        let body = self.parse_statement()?;
        Ok(Statement::For { var, from: Box::new(from), to: Box::new(to), by, body: Box::new(body) })
    }

    fn parse_test_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Test, "Expect 'TEST'")?;
        let condition = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Then, "Expect 'THEN' after TEST condition.")?;
        let then_branch = self.parse_statement()?;
        self.consume(TokenType::Or, "Expect 'OR' for the else branch of a TEST statement.")?;
        let else_branch = self.parse_statement()?;
        Ok(Statement::Test {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::While, "Expected 'WHILE'")?;
        let condition = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Do, "Expected 'DO' after WHILE condition.")?;
        let body = self.parse_statement()?;
        Ok(Statement::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_until_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Until, "Expected 'UNTIL'")?;
        let condition = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Do, "Expected 'DO' after UNTIL condition.")?;
        let body = self.parse_statement()?;
        Ok(Statement::Until {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_switchon_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::Switchon, "Expected 'SWITCHON'")?;
        let expr = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::Into, "Expected 'INTO' after SWITCHON expression.")?;
        self.consume(TokenType::SectionStart, "Expected '$(' to begin SWITCHON block.")?;

        let mut cases = Vec::new();
        let mut default = None;

        while !self.check(TokenType::SectionEnd) && !self.is_at_end() {
            if self.match_token(TokenType::Default) {
                self.consume(TokenType::Colon, "Expected ':' after DEFAULT.")?;
                default = Some(Box::new(self.parse_statement()?));
                break;
            } else if self.match_token(TokenType::Case) {
                let mut values = vec![];
                loop {
                    values.push(match self.parse_expression(Precedence::None)? {
                        Expression::Literal(lit) => lit,
                        _ => return Err(self.error_at_current("CASE labels must be literals.")),
                    });
                    if !self.match_token(TokenType::Comma) { break; }
                }
                self.consume(TokenType::Colon, "Expected ':' after CASE labels.")?;
                let body = self.parse_statement()?;
                cases.push((values, Box::new(body)));
            } else {
                return Err(self.error_at_current("Expected 'CASE' or 'DEFAULT' in SWITCHON block."));
            }
        }

        self.consume(TokenType::SectionEnd, "Expected '$)' to end SWITCHON block.")?;
        Ok(Statement::Switch { expr: Box::new(expr), cases, default })
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::SectionStart, "Expect '$(' or '{' to start a block.")?;
        let mut items = Vec::new();

        while !self.check(TokenType::SectionEnd) && !self.is_at_end() {
            let item = if self.check(TokenType::Let) {
                BlockItem::Declaration(self.parse_declaration_chain()?)
            } else {
                BlockItem::Statement(self.parse_statement()?)
            };
            items.push(item);
        }

        self.consume(TokenType::SectionEnd, "Expect '$)' or '}' to end a block.")?;
        Ok(Statement::Block { items })
    }

    // --- Expression Parsing (Pratt Parser) ---

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_expr = self.parse_prefix()?;

        while precedence < self.get_precedence(self.peek()) {
            left_expr = self.parse_infix(left_expr)?;
        }

        Ok(left_expr)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        match self.peek().kind.clone() {
            TokenType::Number(_) | TokenType::FloatNumber(_) | TokenType::StringLiteral(_)
            | TokenType::CharLiteral(_) | TokenType::True | TokenType::False => self.parse_literal(),
            TokenType::Identifier(name) => { self.advance(); Ok(Expression::Variable(name))},
            TokenType::Minus | TokenType::At | TokenType::LogicalNot | TokenType::Indirection => self.parse_unary(),
            TokenType::LParen => self.parse_grouping(),
            TokenType::Vec => self.parse_vec_expression(),
            TokenType::Valof => self.parse_valof_expression(),
            TokenType::Table => self.parse_table_expression(),
            _ => Err(self.error_at_current("Expected an expression.")),
        }
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, ParseError> {
        match self.peek().kind {
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash | TokenType::Rem
            | TokenType::Eq | TokenType::NotEq | TokenType::Less | TokenType::LessEq
            | TokenType::Greater | TokenType::GreaterEq | TokenType::LogicalAnd | TokenType::LogicalOr
            | TokenType::Eqv | TokenType::Neqv | TokenType::ShiftLeft | TokenType::ShiftRight
            | TokenType::FloatPlus | TokenType::FloatMinus | TokenType::FloatStar | TokenType::FloatSlash
            | TokenType::FloatEq | TokenType::FloatNotEq | TokenType::FloatLess | TokenType::FloatLessEq
            | TokenType::FloatGreater | TokenType::FloatGreaterEq
            => self.parse_binary(left),

            TokenType::Indirection | TokenType::StringSub | TokenType::FloatVecSub
            => self.parse_subscript(left),

            TokenType::Conditional => self.parse_conditional(left),

            TokenType::LParen => self.parse_call(left),

            _ => Err(self.error_at_current("Expected an infix operator or function call.")),
        }
    }

    fn parse_valof_expression(&mut self) -> Result<Expression, ParseError> {
        self.consume(TokenType::Valof, "Expected 'VALOF'")?;
        let body = self.parse_statement()?;
        Ok(Expression::Valof(Box::new(body)))
    }

    fn parse_vec_expression(&mut self) -> Result<Expression, ParseError> {
        self.consume(TokenType::Vec, "Expect 'VEC'.")?;
        let size_expr = self.parse_expression(Precedence::Primary)?;
        Ok(Expression::Vec(Box::new(size_expr)))
    }

    fn parse_table_expression(&mut self) -> Result<Expression, ParseError> {
        self.consume(TokenType::Table, "Expect 'TABLE'.")?;
        self.consume(TokenType::SectionStart, "Expect '$[' or equivalent after TABLE.")?;

        let mut elements = Vec::new();
        if !self.check(TokenType::SectionEnd) {
            loop {
                elements.push(self.parse_expression(Precedence::None)?);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::SectionEnd, "Expect '$]' or equivalent to end TABLE.")?;
        Ok(Expression::Table(elements))
    }

    fn parse_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.advance();
        let literal = match &token.kind {
            TokenType::Number(n) => LiteralExpr::Number(*n),
            TokenType::FloatNumber(f) => LiteralExpr::FloatNumber(*f),
            TokenType::StringLiteral(s) => LiteralExpr::String(s.clone()),
            TokenType::CharLiteral(c) => LiteralExpr::Char(*c),
            TokenType::True => LiteralExpr::True,
            TokenType::False => LiteralExpr::False,
            _ => return Err(self.error_at_token(token, "Expected a literal value.")),
        };
        Ok(Expression::Literal(literal))
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        let op_token = self.advance();
        let op = match op_token.kind {
            TokenType::Minus => UnaryOp::Negate,
            TokenType::At => UnaryOp::Address,
            TokenType::Indirection => UnaryOp::Deref,
            TokenType::LogicalNot => UnaryOp::Not,
            _ => return Err(self.error_at_token(op_token, "Invalid unary operator.")),
        };
        let operand = self.parse_expression(Precedence::Unary)?;
        Ok(Expression::UnaryOp { op, operand: Box::new(operand) })
    }

    fn parse_conditional(&mut self, condition: Expression) -> Result<Expression, ParseError> {
        self.consume(TokenType::Conditional, "Expect '->'.")?;
        let true_expr = self.parse_expression(Precedence::Conditional)?;
        self.consume(TokenType::Comma, "Expect ',' in conditional expression.")?;
        let false_expr = self.parse_expression(Precedence::Conditional)?;
        Ok(Expression::Conditional {
            condition: Box::new(condition),
            true_expr: Box::new(true_expr),
            false_expr: Box::new(false_expr),
        })
    }

    fn parse_binary(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let op_token = self.advance();
        let precedence = self.get_precedence(op_token);
        let op = self.get_binary_op(op_token)?;
        let right = self.parse_expression(precedence)?;
        Ok(Expression::BinaryOp { left: Box::new(left), op, right: Box::new(right) })
    }

    fn parse_subscript(&mut self, base: Expression) -> Result<Expression, ParseError> {
        let op_token = self.advance();
        let op = self.get_binary_op(op_token)?;
        let index = self.parse_expression(Precedence::Subscript)?;
        Ok(Expression::BinaryOp { left: Box::new(base), op, right: Box::new(index) })
    }

    fn parse_grouping(&mut self) -> Result<Expression, ParseError> {
        self.consume(TokenType::LParen, "Expect '(' for grouping.")?;
        let expr = self.parse_expression(Precedence::None)?;
        self.consume(TokenType::RParen, "Expect ')' after expression.")?;
        Ok(expr)
    }

    fn parse_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        self.consume(TokenType::LParen, "Expect '(' for function call.")?;
        let mut args = Vec::new();
        if !self.check(TokenType::RParen) {
            loop {
                args.push(self.parse_expression(Precedence::None)?);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RParen, "Expect ')' after arguments.")?;
        Ok(Expression::FunctionCall { callee: Box::new(callee), args })
    }
}
