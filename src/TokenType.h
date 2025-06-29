#ifndef TOKEN_TYPE_H
#define TOKEN_TYPE_H

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

#endif // TOKEN_TYPE_H
