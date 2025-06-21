//! # BCPL Lexer
//!
//! This module is responsible for the lexical analysis of BCPL source code.
//! It takes a raw string of source code and transforms it into a flat sequence of tokens.
//! This token stream is then ready to be consumed by the parser.
//!
//! The design handles:
//! - Standard BCPL syntax from the reference manuals.
//! - Modern extensions for characters, strings, and floating-point numbers.
//! - Detailed location tracking (line and column) for high-quality error messages.
//! - Recognition of all keywords, operators, and literal formats.

use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;

// --- Public API Structures ---

/// Represents a single lexical token.
/// It contains the type of the token and its location in the source file.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub col: usize,
}

/// Enumerates all possible kinds of tokens in the BCPL language.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // --- Delimiters and Brackets ---
    LParen,        // (
    RParen,        // )
    SectionStart,  // $( or {
    SectionEnd,    // $) or }
    Comma,         // ,
    Colon,         // :
    Semicolon,     // ;

    // --- Operators ---
    Assign,        // :=

    // Addressing & Subscripting
    At,            // @ (Address-of)
    Indirection,   // ! (Vector indirection)
    StringSub,     // % (Character indirection)
    FloatVecSub,   // .% (Float vector indirection)

    // Arithmetic
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    FloatPlus,     // +.
    FloatMinus,    // -.
    FloatStar,     // *.
    FloatSlash,    // /.

    // Relational
    Eq,            // =
    NotEq,         // ~=
    Less,          // <
    LessEq,        // <=
    Greater,       // >
    GreaterEq,     // >=
    FloatEq,       // =.
    FloatNotEq,    // ~=.
    FloatLess,     // <.
    FloatLessEq,   // <=.
    FloatGreater,  // >.
    FloatGreaterEq,// >=.

    // Logical
    LogicalNot,    // ~
    LogicalAnd,    // &
    LogicalOr,     // |

    // Shift
    ShiftLeft,     // <<
    ShiftRight,    // >>

    // Conditional
    Conditional,   // ->

    // --- Literals (with associated data) ---
    Identifier(String),
    Number(i64),
    FloatNumber(f64),
    StringLiteral(String),
    CharLiteral(char),

    // --- Keywords ---
    And, Be, Break, By, Case, Default, Do, Endcase, Eqv, False, Finish, For,
    Get, Global, Goto, If, Into, Let, Loop, Manifest, Neqv, Or, Rem, Repeat,
    RepeatUntil, RepeatWhile, Resultis, Return, Static, Switchon, Table, Test,
    Then, To, True, Unless, Until, Valof, Vec, While,

    // NEW: Added Section keyword
    Section,

    // --- Special ---
    EndOfFile,
}

/// Represents an error encountered during lexical analysis.
#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

// --- Main Public Functions ---

/// Scans a BCPL source string and returns a vector of tokens or a lexer error.
pub fn scan(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source);
    lexer.scan_tokens()
}

/// Prints a vector of tokens in a human-readable, formatted table for debugging.
pub fn print_tokens(tokens: &[Token]) {
    println!("\n--- Token Stream ---");
    println!("{:<5} {:<5} {:<20} {}", "Line", "Col", "Type", "Lexeme / Value");
    println!("{}", "-".repeat(60));

    for token in tokens {
        // Format the token kind to be more readable, especially for literals.
        let kind_str = match &token.kind {
            TokenType::Identifier(s) => format!("Identifier({})", s),
            TokenType::Number(n) => format!("Number({})", n),
            TokenType::FloatNumber(f) => format!("FloatNumber({})", f),
            TokenType::StringLiteral(s) => format!("String(\"{}\")", s.replace('\n', "\\n")),
            TokenType::CharLiteral(c) => format!("Char('{}')", c),
            // For all other simple tokens, just use the debug representation.
            _ => format!("{:?}", token.kind),
        };

        println!(
            "{:<5} {:<5} {:<20} '{}'",
            token.line, token.col, kind_str, token.lexeme
        );
    }
    println!("--- End Token Stream ---\n");
}


// --- Internal Lexer Implementation ---

/// The Lexer struct holds the state needed for tokenization.
struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    keywords: HashMap<&'static str, TokenType>,

    // Positional tracking
    start_pos: usize,
    current_pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer instance.
    fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.chars().peekable(),
            keywords: Self::create_keywords_map(),
            start_pos: 0,
            current_pos: 0,
            line: 1,
            col: 1,
        }
    }

    /// The main loop that consumes characters and produces tokens.
    fn scan_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start_pos = self.current_pos;
            if let Some(token) = self.scan_token()? {
                tokens.push(token);
            }
        }

        tokens.push(Token {
            kind: TokenType::EndOfFile,
            lexeme: String::from(""),
            line: self.line,
            col: self.col,
        });

        Ok(tokens)
    }

    /// Scans a single token based on the current character.
    fn scan_token(&mut self) -> Result<Option<Token>, LexerError> {
        let (start_col, start_line) = (self.col, self.line);

        let c = self.advance();

        let token_type = match c {
            // Single-character tokens
            '(' => Some(TokenType::LParen),
            ')' => Some(TokenType::RParen),
            ',' => Some(TokenType::Comma),
            ';' => Some(TokenType::Semicolon),
            '@' => Some(TokenType::At),
            '!' => Some(TokenType::Indirection),
            '%' => Some(TokenType::StringSub),
            '&' => Some(TokenType::LogicalAnd),
            '|' => Some(TokenType::LogicalOr),

            // Operators that might be two characters
            ':' => if self.match_char('=') { Some(TokenType::Assign) } else { Some(TokenType::Colon) },
            '+' => if self.match_char('.') { Some(TokenType::FloatPlus) } else { Some(TokenType::Plus) },
            '*' => if self.match_char('.') { Some(TokenType::FloatStar) } else { Some(TokenType::Star) },

            '/' => self.handle_slash(),
            '-' => if self.match_char('>') { Some(TokenType::Conditional) } else if self.match_char('.') { Some(TokenType::FloatMinus) } else { Some(TokenType::Minus) },
            '<' => if self.match_char('<') { Some(TokenType::ShiftLeft) } else if self.match_char('=') { if self.match_char('.') { Some(TokenType::FloatLessEq) } else { Some(TokenType::LessEq) }} else if self.match_char('.') { Some(TokenType::FloatLess) } else { Some(TokenType::Less) },
            '>' => if self.match_char('>') { Some(TokenType::ShiftRight) } else if self.match_char('=') { if self.match_char('.') { Some(TokenType::FloatGreaterEq) } else { Some(TokenType::GreaterEq) }} else if self.match_char('.') { Some(TokenType::FloatGreater) } else { Some(TokenType::Greater) },
            '=' => if self.match_char('.') { Some(TokenType::FloatEq) } else { Some(TokenType::Eq) },
            '~' => if self.match_char('=') { if self.match_char('.') { Some(TokenType::FloatNotEq) } else { Some(TokenType::NotEq) }} else { Some(TokenType::LogicalNot) },

            // Section brackets
            '$' => if self.match_char('(') { Some(TokenType::SectionStart) } else if self.match_char(')') { Some(TokenType::SectionEnd) } else { return Err(self.error("Invalid character after '$'")) },
            '{' => Some(TokenType::SectionStart),
            '}' => Some(TokenType::SectionEnd),

            // Floating point indirection
            '.' => if self.match_char('%') { Some(TokenType::FloatVecSub) } else { return Err(self.error("'.' is not a valid operator unless part of a float literal or '.%'")) },

            // Literals
            '\'' => Some(self.char_literal()?),
            '"' => Some(self.string_literal()?),

            // Numbers
            '#' => Some(self.based_number()?),
            c if c.is_ascii_digit() => Some(self.number()?),

            // Identifiers and Keywords
            c if c.is_ascii_alphabetic() || c == '_' => Some(self.identifier()),

            // Whitespace
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                self.col = 1;
                None
            }

            // Unrecognized character
            _ => return Err(self.error(&format!("Unexpected character: {}", c))),
        };

        if let Some(kind) = token_type {
            let lexeme = self.source[self.start_pos..self.current_pos].to_string();
            Ok(Some(Token { kind, lexeme, line: start_line, col: start_col }))
        } else {
            Ok(None)
        }
    }

    /// Handles tokens starting with '/', which could be a comment or an operator.
    fn handle_slash(&mut self) -> Option<TokenType> {
        if self.match_char('/') {
            // A line comment goes to the end of the line.
            while self.peek() != '\n' && !self.is_at_end() {
                self.advance();
            }
            None // No token is emitted for a comment
        } else if self.match_char('*') {
            // A block comment.
            self.block_comment();
            None
        } else if self.match_char('.') {
            Some(TokenType::FloatSlash)
        } else {
            Some(TokenType::Slash)
        }
    }

    /// Consumes a block comment `/* ... */`.
    fn block_comment(&mut self) {
        while !(self.peek() == '*' && self.peek_next() == '/') && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.col = 1;
            }
            self.advance();
        }

        if !self.is_at_end() {
            self.advance(); // Consume '*'
            self.advance(); // Consume '/'
        }
    }

    /// Parses an identifier or a keyword.
    fn identifier(&mut self) -> TokenType {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        let text = &self.source[self.start_pos..self.current_pos];
        self.keywords.get(text.to_uppercase().as_str()).cloned().unwrap_or_else(|| TokenType::Identifier(text.to_string()))
    }

    /// Parses a number literal (decimal or float).
    fn number(&mut self) -> Result<TokenType, LexerError> {
        let mut is_float = false;
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for a fractional part.
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance(); // Consume the "."
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        // Look for an exponent part.
        if self.peek().to_ascii_lowercase() == 'e' {
            is_float = true;
            self.advance(); // Consume 'e' or 'E'
            if self.peek() == '+' || self.peek() == '-' {
                self.advance();
            }
            if !self.peek().is_ascii_digit() {
                return Err(self.error("Malformed float exponent"));
            }
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let text = &self.source[self.start_pos..self.current_pos];
        if is_float {
            text.parse::<f64>()
                .map(TokenType::FloatNumber)
                .map_err(|_| self.error("Invalid floating point number"))
        } else {
            text.parse::<i64>()
                .map(TokenType::Number)
                .map_err(|_| self.error("Invalid integer number"))
        }
    }

    /// Parses a number literal with a specific base (#).
    fn based_number(&mut self) -> Result<TokenType, LexerError> {
        let radix = if self.peek().to_ascii_lowercase() == 'x' {
            self.advance(); // consume 'x' or 'X'
            16
        } else {
            8
        };

        self.start_pos = self.current_pos; // Start parsing after '#' or '#X'
        while self.peek().is_digit(radix) {
            self.advance();
        }

        let text = &self.source[self.start_pos..self.current_pos];
        if text.is_empty() {
            return Err(self.error("Number missing after base specifier"));
        }

        i64::from_str_radix(text, radix)
            .map(TokenType::Number)
            .map_err(|_| self.error("Invalid number literal for the given base"))
    }

    /// Parses a string literal.
    fn string_literal(&mut self) -> Result<TokenType, LexerError> {
        let mut value = String::new();
        while self.peek() != '"' && !self.is_at_end() {
            let c = self.peek();
            if c == '\n' {
                self.line += 1;
            }

            if c == '*' { // Escape sequence
                self.advance(); // consume '*'
                let escaped = match self.advance() {
                    'n' | 'N' => '\n',
                    't' | 'T' => '\t',
                    's' | 'S' => ' ',
                    'b' | 'B' => '\x08', // Backspace
                    'p' | 'P' => '\x0C', // Form feed / new page
                    'c' | 'C' => '\r',
                    '"' => '"',
                    '*' => '*',
                    c => return Err(self.error(&format!("Invalid escape sequence: *{}", c))),
                };
                value.push(escaped);
            } else {
                value.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Err(self.error("Unterminated string."));
        }

        self.advance(); // The closing quote.
        Ok(TokenType::StringLiteral(value))
    }

    /// Parses a character literal.
    fn char_literal(&mut self) -> Result<TokenType, LexerError> {
        let character = if self.peek() == '*' {
            self.advance(); // consume '*'
            match self.advance() {
                'n' | 'N' => '\n',
                't' | 'T' => '\t',
                c => return Err(self.error(&format!("Invalid character escape sequence: *{}", c))),
            }
        } else {
            self.advance()
        };

        if self.peek() != '\'' {
            return Err(self.error("Unterminated character literal. Must be a single character."));
        }
        self.advance(); // consume closing '

        Ok(TokenType::CharLiteral(character))
    }

    // --- Helper Methods ---

    /// Checks if we've consumed all characters.
    fn is_at_end(&self) -> bool {
        self.current_pos >= self.source.len()
    }

    /// Consumes and returns the next character, advancing position counters.
    fn advance(&mut self) -> char {
        self.current_pos += 1;
        self.col += 1;
        self.chars.next().unwrap_or('\0')
    }

    /// Checks if the next character matches the expected one. If so, consumes it.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }
        self.advance();
        true
    }

    /// Returns the next character without consuming it.
    fn peek(&mut self) -> char {
        self.chars.peek().cloned().unwrap_or('\0')
    }

    /// Returns the character after the next one without consuming anything.
    fn peek_next(&self) -> char {
        let mut clone = self.chars.clone();
        clone.next(); // Skip the first peek
        clone.next().unwrap_or('\0')
    }

    /// Creates a LexerError with the current position.
    fn error(&self, message: &str) -> LexerError {
        LexerError {
            message: message.to_string(),
            line: self.line,
            col: self.col,
        }
    }

    /// Initializes the keyword map.
    fn create_keywords_map() -> HashMap<&'static str, TokenType> {
        let mut map = HashMap::new();
        map.insert("AND", TokenType::And);
        map.insert("BE", TokenType::Be);
        map.insert("BREAK", TokenType::Break);
        map.insert("BY", TokenType::By);
        map.insert("CASE", TokenType::Case);
        map.insert("DEFAULT", TokenType::Default);
        map.insert("DO", TokenType::Do);
        map.insert("ENDCASE", TokenType::Endcase);
        map.insert("EQV", TokenType::Eqv);
        map.insert("FALSE", TokenType::False);
        map.insert("FINISH", TokenType::Finish);
        map.insert("FOR", TokenType::For);
        map.insert("GET", TokenType::Get);
        map.insert("GLOBAL", TokenType::Global);
        map.insert("GOTO", TokenType::Goto);
        map.insert("IF", TokenType::If);
        map.insert("INTO", TokenType::Into);
        map.insert("LET", TokenType::Let);
        map.insert("LOOP", TokenType::Loop);
        map.insert("MANIFEST", TokenType::Manifest);
        map.insert("NEQV", TokenType::Neqv);
        map.insert("OR", TokenType::Or);
        map.insert("REM", TokenType::Rem);
        map.insert("REPEAT", TokenType::Repeat);
        map.insert("REPEATUNTIL", TokenType::RepeatUntil);
        map.insert("REPEATWHILE", TokenType::RepeatWhile);
        map.insert("RESULTIS", TokenType::Resultis);
        map.insert("RETURN", TokenType::Return);
        map.insert("SECTION", TokenType::Section); // NEW
        map.insert("STATIC", TokenType::Static);
        map.insert("SWITCHON", TokenType::Switchon);
        map.insert("TABLE", TokenType::Table);
        map.insert("TEST", TokenType::Test);
        map.insert("THEN", TokenType::Then);
        map.insert("TO", TokenType::To);
        map.insert("TRUE", TokenType::True);
        map.insert("UNLESS", TokenType::Unless);
        map.insert("UNTIL", TokenType::Until);
        map.insert("VALOF", TokenType::Valof);
        map.insert("VEC", TokenType::Vec);
        map.insert("WHILE", TokenType::While);
        map
    }
}
