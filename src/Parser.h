#ifndef PARSER_H
#define PARSER_H

#include "Lexer.h"
#include "AST.h"
#include <memory>
#include <vector>

/**
 * @class Parser
 * @brief A singleton class that constructs an Abstract Syntax Tree (AST) from a token stream.
 *
 * This is a recursive descent parser. It processes the token stream from the Lexer
 * and builds a hierarchical representation of the source code according to the
 * BCPL grammar. The final output is a complete AST, ready for the code generator.
 */
class Parser {
public:
    // --- Singleton Access ---
    static Parser& getInstance() {
        static Parser instance;
        return instance;
    }

    // --- Deleted Constructors to enforce singleton pattern ---
    Parser(const Parser&) = delete;
    Parser& operator=(const Parser&) = delete;

    /**
     * @brief Parses a complete BCPL source file.
     * @param source The BCPL source code as a string.
     * @return A unique_ptr to the root Program node of the generated AST.
     * @throws std::runtime_error on a syntax error.
     */
    ProgramPtr parse(const std::string& source);

private:
    Parser() : lexer(Lexer::getInstance()) {}

    // --- Parser State ---
    Lexer& lexer;
    Token currentToken;
    Token peekToken;

    // --- Utility Methods ---
    void advanceTokens();
    void expect(TokenType type, const std::string& message);

    // --- Parsing Methods for different grammar rules ---
    
    // Declarations
    DeclPtr parseDeclaration();
    DeclPtr parseLetDeclaration();
    DeclPtr parseGlobalDeclaration();
    DeclPtr parseManifestDeclaration();
    DeclPtr parseFunctionOrRoutineDeclaration(const std::string& name);
    // Other declarations like MANIFEST, STATIC, GLOBAL would go here.

    // Statements
    StmtPtr parseStatement();
    StmtPtr parseCompoundStatement();
    StmtPtr parseIfStatement();
    StmtPtr parseWhileStatement();
    StmtPtr parseForStatement();
    StmtPtr parseSwitchonStatement();
    StmtPtr parseGotoStatement();
    StmtPtr parseReturnStatement();
    StmtPtr parseRepeatStatement();
    StmtPtr parseResultisStatement();
    StmtPtr parseLabeledStatement(const std::string& label_name);
    StmtPtr parseExpressionStatement(); // For routine calls and assignments
    Assignment* parseAssignment(ExprPtr first_lhs);
    
    // Expressions (using Pratt parsing or precedence climbing)
    ExprPtr parseExpression(int precedence = 0);
    ExprPtr parsePrimaryExpression();
    ExprPtr parseIdentifierExpression();
    ExprPtr parseParenExpression();
    ExprPtr parseValofExpression();
    ExprPtr parseVectorConstructor();
    ExprPtr parseUnary();
    ExprPtr parseBinaryRHS(int expr_prec, ExprPtr lhs);
    ExprPtr parseFunctionCall(ExprPtr function_name);

    int getTokenPrecedence();
};

#endif // PARSER_H

