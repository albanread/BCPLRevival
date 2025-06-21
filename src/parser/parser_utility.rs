use super::{Parser, ParseError, Expression, LValue, Precedence, BinaryOp, UnaryOp};
use crate::lexer::{Token, TokenType};

// --- Parser Helper Methods ---

impl<'a> Parser<'a> {
    /// Converts an `Expression` into a valid `LValue` if possible.
    /// This is used to validate the left-hand side of an assignment.
    pub(super) fn expression_to_lvalue(&self, expr: Expression) -> Result<LValue, ParseError> {
        match expr {
            Expression::Variable(name) => Ok(LValue::Name(name)),
            Expression::UnaryOp { op: UnaryOp::Deref, operand } => Ok(LValue::Indirection(operand)),
            Expression::BinaryOp { left, op, right } => {
                match op {
                    BinaryOp::Subscript | BinaryOp::StringSub | BinaryOp::FloatVecSub => {
                        Ok(LValue::Subscript { base: left, index: right, op })
                    }
                    _ => Err(self.error_at_current("This expression cannot be assigned to."))
                }
            }
            _ => Err(self.error_at_current("Invalid assignment target. Must be a variable, indirection, or subscript.")),
        }
    }

    /// Checks if the current token matches the given `TokenType`. If it does,
    /// it consumes the token and returns true. Otherwise, it returns false.
    pub(super) fn match_token(&mut self, kind: TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consumes the current token if it matches the expected `TokenType`.
    /// If it matches, the token is returned. If not, an error is generated.
    pub(super) fn consume(&mut self, kind: TokenType, message: &str) -> Result<&'a Token, ParseError> {
        if self.check(kind.clone()) {
            Ok(self.advance())
        } else {
            Err(self.error_at_current(&format!("{} (found {:?})", message, self.peek().kind)))
        }
    }

    /// Consumes the current token if it's an identifier, returning its name.
    /// Otherwise, it returns an error.
    pub(super) fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        let token = self.peek();
        if let TokenType::Identifier(name) = &token.kind {
            self.advance();
            Ok(name.clone())
        } else {
            Err(self.error_at_token(token, message))
        }
    }

    /// Consumes the current token if it's a number, returning its value.
    /// Otherwise, it returns an error.
    pub(super) fn consume_integer(&mut self, message: &str) -> Result<i64, ParseError> {
        let token = self.peek();
        if let TokenType::Number(val) = token.kind {
            self.advance();
            Ok(val)
        } else {
            Err(self.error_at_token(token, message))
        }
    }

    // NEW: Consumes the current token if it's a string literal, returning its content.
    // Otherwise, it returns an error.
    pub(super) fn consume_string_literal(&mut self, message: &str) -> Result<String, ParseError> {
        let token = self.peek();
        if let TokenType::StringLiteral(val) = &token.kind {
            self.advance();
            Ok(val.clone())
        } else {
            Err(self.error_at_token(token, message))
        }
    }


    /// Checks if the current token is of the given `TokenType` without consuming it.
    pub(super) fn check(&self, kind: TokenType) -> bool {
        !self.is_at_end() && self.peek().kind == kind
    }

    /// Returns the next token without consuming it.
    pub(super) fn peek_next(&self) -> &'a Token {
        if self.current + 1 >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1] // EndOfFile
        } else {
            &self.tokens[self.current + 1]
        }
    }

    /// Consumes and returns the current token, advancing the parser's position.
    pub(super) fn advance(&mut self) -> &'a Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    /// Checks if the parser has reached the end of the token stream.
    pub(super) fn is_at_end(&self) -> bool {
        self.peek().kind == TokenType::EndOfFile
    }

    /// Returns the current token without consuming it.
    pub(super) fn peek(&self) -> &'a Token {
        &self.tokens[self.current]
    }

    /// Returns the most recently consumed token.
    pub(super) fn previous(&self) -> &'a Token {
        &self.tokens[self.current - 1]
    }

    /// Creates a `ParseError` at the location of the current token.
    pub(super) fn error_at_current(&self, message: &str) -> ParseError {
        self.error_at_token(self.peek(), message)
    }

    /// Creates a `ParseError` at the location of a given token.
    pub(super) fn error_at_token(&self, token: &Token, message: &str) -> ParseError {
        let full_message = if token.kind == TokenType::EndOfFile {
            format!("{} at end of file", message)
        } else {
            format!("{} near '{}'", message, token.lexeme)
        };
        ParseError {
            message: full_message,
            line: token.line,
            col: token.col,
        }
    }

    /// Returns the precedence level for a given token in the Pratt parser.
    pub(super) fn get_precedence(&self, token: &Token) -> Precedence {
        match token.kind {
            TokenType::LParen => Precedence::Call,
            TokenType::Conditional => Precedence::Conditional,
            TokenType::Or | TokenType::Eqv | TokenType::Neqv => Precedence::Or,
            TokenType::LogicalAnd => Precedence::And,
            TokenType::Eq | TokenType::NotEq | TokenType::Less | TokenType::Greater | TokenType::LessEq | TokenType::GreaterEq
            | TokenType::FloatEq | TokenType::FloatNotEq | TokenType::FloatLess | TokenType::FloatLessEq | TokenType::FloatGreater | TokenType::FloatGreaterEq
            => Precedence::Equality,
            TokenType::Plus | TokenType::Minus | TokenType::FloatPlus | TokenType::FloatMinus => Precedence::Term,
            TokenType::Star | TokenType::Slash | TokenType::Rem | TokenType::FloatStar | TokenType::FloatSlash => Precedence::Factor,
            TokenType::ShiftLeft | TokenType::ShiftRight => Precedence::Shift,
            TokenType::Indirection | TokenType::StringSub | TokenType::FloatVecSub => Precedence::Subscript,
            _ => Precedence::None,
        }
    }

    /// Converts a token into its corresponding `BinaryOp`.
    pub(super) fn get_binary_op(&self, token: &Token) -> Result<BinaryOp, ParseError> {
        Ok(match token.kind {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Star => BinaryOp::Mul,
            TokenType::Slash => BinaryOp::Div,
            TokenType::Rem => BinaryOp::Rem,
            TokenType::Eq => BinaryOp::Eq,
            TokenType::NotEq => BinaryOp::NotEq,
            TokenType::Less => BinaryOp::Less,
            TokenType::LessEq => BinaryOp::LessEq,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::GreaterEq => BinaryOp::GreaterEq,
            TokenType::LogicalAnd => BinaryOp::And,
            TokenType::LogicalOr => BinaryOp::Or,
            TokenType::Eqv => BinaryOp::Eqv,
            TokenType::Neqv => BinaryOp::NotEqv,
            TokenType::ShiftLeft => BinaryOp::ShiftLeft,
            TokenType::ShiftRight => BinaryOp::ShiftRight,
            TokenType::FloatPlus => BinaryOp::FloatAdd,
            TokenType::FloatMinus => BinaryOp::FloatSub,
            TokenType::FloatStar => BinaryOp::FloatMul,
            TokenType::FloatSlash => BinaryOp::FloatDiv,
            TokenType::FloatEq => BinaryOp::FloatEq,
            TokenType::FloatNotEq => BinaryOp::FloatNotEq,
            TokenType::FloatLess => BinaryOp::FloatLess,
            TokenType::FloatLessEq => BinaryOp::FloatLessEq,
            TokenType::FloatGreater => BinaryOp::FloatGreater,
            TokenType::FloatGreaterEq => BinaryOp::FloatGreaterEq,
            TokenType::Indirection => BinaryOp::Subscript,
            TokenType::StringSub => BinaryOp::StringSub,
            TokenType::FloatVecSub => BinaryOp::FloatVecSub,
            _ => return Err(self.error_at_token(token, "Invalid binary operator.")),
        })
    }
}
