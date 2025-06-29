#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include "AST.h"
#include <memory>

/**
 * @class Optimizer
 * @brief A singleton class that performs optimizations on the Abstract Syntax Tree.
 *
 * This class implements an AST-to-AST transformation pass. It walks the input AST
 * and returns a new, functionally equivalent, but more efficient AST. This initial
 * implementation focuses on Constant Folding.
 */
class Optimizer {
public:
    // --- Singleton Access ---
    static Optimizer& getInstance() {
        static Optimizer instance;
        return instance;
    }

    // --- Deleted Constructors ---
    Optimizer(const Optimizer&) = delete;
    Optimizer& operator=(const Optimizer&) = delete;

    /**
     * @brief The main entry point for the optimization pass.
     * @param ast A unique_ptr to the root of the AST to be optimized.
     * @return A unique_ptr to the root of the newly created, optimized AST.
     */
    ProgramPtr optimize(ProgramPtr ast);

private:
    Optimizer() = default;

    // --- Visitor methods that return new, optimized nodes ---
    // Each method takes a node from the old tree and returns a
    // unique_ptr to a node for the new tree.

    // Generic dispatchers
    ExprPtr visit(Expression* node);
    StmtPtr visit(Statement* node);
    DeclPtr visit(Declaration* node);

    // Expression visitors
    ExprPtr visit(NumberLiteral* node);
    ExprPtr visit(FloatLiteral* node);
    ExprPtr visit(StringLiteral* node);
    ExprPtr visit(CharLiteral* node);
    ExprPtr visit(VariableAccess* node);
    ExprPtr visit(UnaryOp* node);
    ExprPtr visit(BinaryOp* node);
    ExprPtr visit(FunctionCall* node);
    ExprPtr visit(ConditionalExpression* node);
    ExprPtr visit(Valof* node);
    
    // Statement visitors
    StmtPtr visit(Assignment* node);
    StmtPtr visit(RoutineCall* node);
    StmtPtr visit(CompoundStatement* node);
    StmtPtr visit(IfStatement* node);
    StmtPtr visit(TestStatement* node);
    StmtPtr visit(WhileStatement* node);
    StmtPtr visit(ForStatement* node);
    StmtPtr visit(GotoStatement* node);
    StmtPtr visit(LabeledStatement* node);
    StmtPtr visit(ReturnStatement* node);
    StmtPtr visit(FinishStatement* node);
    StmtPtr visit(ResultisStatement* node);
    
    // Declaration visitors
    DeclPtr visit(LetDeclaration* node);
    DeclPtr visit(FunctionDeclaration* node);
    
    // Top-level visitor
    ProgramPtr visit(Program* node);
};

#endif // OPTIMIZER_H

