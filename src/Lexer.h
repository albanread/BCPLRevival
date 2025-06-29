#ifndef LEXER_H
#define LEXER_H

#include <string>
#include <vector>
#include <cstdint>

// Defines all the possible token types in the BCPL language.
enum class TokenType {
    // End of File
    Eof,

    // Identifiers and Literals
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,

    // Keywords
    KwLet, KwAnd, KwBe, KwVec,
    KwIf, KwThen, KwUnless, KwTest, KwOr,
    KwWhile, KwDo, KwUntil, KwRepeat, KwRepeatWhile, KwRepeatUntil,
    KwFor, KwTo, KwBy,
    KwSwitchon, KwInto, KwCase, KwDefault, KwEndcase,
    KwGoto, KwReturn, KwResultis,
    KwBreak, KwLoop,
    KwValof,
    KwManifest, KwStatic, KwGlobal,
    KwTrue, KwFalse,

    // Operators
    OpAssign,       // :=
    OpPlus,         // +
    OpMinus,        // -
    OpMultiply,     // *
    OpDivide,       // /
    OpRemainder,    // REM
    OpEq,           // =
    OpNe,           // ~=
    OpLt,           // <
    OpGt,           // >
    OpLe,           // <=
    OpGe,           // >=
    OpLogAnd,       // &
    OpLogOr,        // |
    OpLogNot,       // ~
    OpLogEqv,       // EQV
    OpLogNeqv,      // NEQV
    OpLshift,       // <<
    OpRshift,       // >>
    OpAt,           // @ (Address of)
    OpBang,         // ! (Indirection / Vector subscript)
    OpConditional,  // ->

    // Floating Point Operators
    OpFloatPlus,    // +.
    OpFloatMinus,   // -.
    OpFloatMultiply,// *.
    OpFloatDivide,  // /.
    OpFloatEq,      // =.
    OpFloatNe,      // ~=.
    OpFloatLt,      // <.
    OpFloatGt,      // >.
    OpFloatLe,      // <=.
    OpFloatGe,      // >=.
    OpFloatVecSub,  // .%

    // Character Operator
    OpCharSub,      // %
    
    // Delimiters
    LParen,         // (
    RParen,         // )
    LBrace,         // {  (alternate for $( )
    RBrace,         // }  (alternate for $) )
    LSection,       // $(
    RSection,       // $)
    Comma,          // ,
    Colon,          // :
    Semicolon,      // ;

    // Others
    Illegal         // Represents an unrecognized token
};

// Represents a single token scanned from the source code.
struct Token {
    TokenType type;
    std::string text;      // The actual text of the token (e.g., "myvar", "123")
    int64_t int_val;       // For integer literals
    double float_val;      // For floating-point literals
    uint32_t line;         // Line number for error reporting
    uint32_t col;          // Column number for error reporting
};

/**
 * @class Lexer
 * @brief A singleton class that performs lexical analysis on BCPL source code.
 *
 * The Lexer scans the input string and converts it into a sequence of tokens
 * according to the BCPL language specification. It handles identifiers,
 * keywords, literals (integer, float, string, char), operators, and comments.
 */
class Lexer {
public:
    // --- Singleton Access ---
    static Lexer& getInstance() {
        static Lexer instance;
        return instance;
    }

    // --- Deleted Constructors to enforce singleton pattern ---
    Lexer(const Lexer&) = delete;
    Lexer& operator=(const Lexer&) = delete;

    /**
     * @brief Initializes the lexer with a new source code string.
     * @param source The BCPL source code to be tokenized.
     */
    void init(const std::string& source);

    /**
     * @brief Scans and returns the next token from the source code.
     * @return The next Token.
     */
    Token getNextToken();

private:
    Lexer() = default;

    void skipWhitespace();
    void skipComments();
    Token identifierOrKeyword();
    Token number();
    Token stringLiteral();
    Token charLiteral();
    Token operatorOrDelimiter();
    char peek() const;
    char peekNext() const;
    char advance();

    std::string source_code;
    size_t pos = 0;
    uint32_t current_line = 1;
    uint32_t current_col = 1;
};

#endif // LEXER_H

