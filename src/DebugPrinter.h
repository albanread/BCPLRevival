#ifndef DEBUG_PRINTER_H
#define DEBUG_PRINTER_H

#include "AST.h"
#include "Lexer.h"
#include <string>
#include <iostream>

/**
 * @class DebugPrinter
 * @brief A singleton utility class for printing compiler intermediate representations.
 *
 * This class provides methods to generate human-readable representations of the
 * token stream from the Lexer and the Abstract Syntax Tree from the Parser.
 * It is an essential tool for debugging the compiler's front-end.
 */
class DebugPrinter {
public:
    // --- Singleton Access ---
    static DebugPrinter& getInstance() {
        static DebugPrinter instance;
        return instance;
    }

    // --- Deleted Constructors ---
    DebugPrinter(const DebugPrinter&) = delete;
    DebugPrinter& operator=(const DebugPrinter&) = delete;

    /**
     * @brief Prints all tokens from a given source string.
     * @param source The BCPL source code.
     */
    void printTokens(const std::string& source);

    /**
     * @brief Prints the Abstract Syntax Tree in a structured, indented format.
     * @param ast The root of the AST to print.
     */
    void printAST(const ProgramPtr& ast);

private:
    DebugPrinter() = default;

    // --- AST Visitor Methods ---
    void visit(Node* node, int indent = 0);

    // Helper to print with indentation
    void indent(int level);

    // Specific visitors for each node type
    void visit(Program* node, int indent);
    void visit(FunctionDeclaration* node, int indent);
    void visit(LetDeclaration* node, int indent);
    void visit(NumberLiteral* node, int indent);
    void visit(FloatLiteral* node, int indent);
    void visit(StringLiteral* node, int indent);
    void visit(CharLiteral* node, int indent);
    void visit(VariableAccess* node, int indent);
    void visit(UnaryOp* node, int indent);
    void visit(BinaryOp* node, int indent);
    void visit(FunctionCall* node, int indent);
    void visit(ConditionalExpression* node, int indent);
    void visit(Valof* node, int indent);
    void visit(Assignment* node, int indent);
    void visit(RoutineCall* node, int indent);
    void visit(CompoundStatement* node, int indent);
    void visit(IfStatement* node, int indent);
    void visit(TestStatement* node, int indent);
    void visit(WhileStatement* node, int indent);
    void visit(ForStatement* node, int indent);
    void visit(GotoStatement* node, int indent);
    void visit(LabeledStatement* node, int indent);
    void visit(ReturnStatement* node, int indent);
    void visit(FinishStatement* node, int indent);
    void visit(ResultisStatement* node, int indent);
};

#endif // DEBUG_PRINTER_H

