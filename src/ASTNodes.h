#ifndef AST_NODES_H
#define AST_NODES_H

#include "AST.h"

// --- Program Node ---
class Program : public Node {
public:
    Program(std::vector<DeclPtr> decls) : declarations(std::move(decls)) {}
    std::vector<DeclPtr> declarations;
};

// --- Declaration Nodes ---
class Declaration : public Node {};

class LetDeclaration : public Declaration {
public:
    struct VarInit {
        std::string name;
        ExprPtr init;
    };
    LetDeclaration(std::vector<VarInit> inits) : initializers(std::move(inits)) {}
    std::vector<VarInit> initializers;
};

class GlobalDeclaration : public Declaration {
public:
    struct Global {
        std::string name;
        int size;
    };
    GlobalDeclaration(std::vector<Global> globals) : globals(std::move(globals)) {}
    std::vector<Global> globals;
};

class ManifestDeclaration : public Declaration {
public:
    struct Manifest {
        std::string name;
        int value;
    };
    ManifestDeclaration(std::vector<Manifest> manifests) : manifests(std::move(manifests)) {}
    std::vector<Manifest> manifests;
};

class FunctionDeclaration : public Declaration {
public:
    FunctionDeclaration(std::string name, std::vector<std::string> params, ExprPtr body_expr, StmtPtr body_stmt)
        : name(std::move(name)), params(std::move(params)), body_expr(std::move(body_expr)), body_stmt(std::move(body_stmt)) {}
    std::string name;
    std::vector<std::string> params;
    ExprPtr body_expr;
    StmtPtr body_stmt;
};

// --- Expression Nodes ---
class Expression : public Node {};

// Represents a numeric literal (integer).
class NumberLiteral : public Expression {
public:
    NumberLiteral(int64_t val) : value(val) {}
    int64_t value;
};

// Represents a floating-point literal.
class FloatLiteral : public Expression {
public:
    FloatLiteral(double val) : value(val) {}
    double value;
};

// Represents a string literal.
class StringLiteral : public Expression {
public:
    StringLiteral(std::string val) : value(std::move(val)) {}
    std::string value;
};

// Represents a character literal.
class CharLiteral : public Expression {
public:
    CharLiteral(int64_t val) : value(val) {}
    int64_t value;
};

// Represents accessing a variable by its name.
class VariableAccess : public Expression {
public:
    VariableAccess(std::string name) : name(std::move(name)) {}
    std::string name;
};

// Represents a unary operation (e.g., @E, ~E, !E).
class UnaryOp : public Expression {
public:
    UnaryOp(TokenType op, ExprPtr rhs) : op(op), rhs(std::move(rhs)) {}
    TokenType op;
    ExprPtr rhs;
};

// Represents a binary operation (e.g., E1 + E2, E1 = E2).
class BinaryOp : public Expression {
public:
    BinaryOp(TokenType op, ExprPtr left, ExprPtr right)
        : op(op), left(std::move(left)), right(std::move(right)) {}
    TokenType op;
    ExprPtr left;
    ExprPtr right;
};

// Represents a function call.
class FunctionCall : public Expression {
public:
    FunctionCall(ExprPtr func, std::vector<ExprPtr> args)
        : function(std::move(func)), arguments(std::move(args)) {}
    ExprPtr function;
    std::vector<ExprPtr> arguments;
};

// Represents a conditional expression (E1 -> E2, E3).
class ConditionalExpression : public Expression {
public:
    ConditionalExpression(ExprPtr cond, ExprPtr true_expr, ExprPtr false_expr)
        : condition(std::move(cond)),
          trueExpr(std::move(true_expr)),
          falseExpr(std::move(false_expr)) {}
    ExprPtr condition;
    ExprPtr trueExpr;
    ExprPtr falseExpr;
};

// Represents a table constructor.
class TableConstructor : public Expression {};

// Represents a vector constructor.
class VectorConstructor : public Expression {
public:
    explicit VectorConstructor(ExprPtr size) : size(std::move(size)) {}
    ExprPtr size;
};

// Represents a VALOF block.
class Valof : public Expression {
public:
    explicit Valof(StmtPtr body) : body(std::move(body)) {}
    StmtPtr body;
};

// Represents a dereference expression (e.g., !P).
class DereferenceExpr : public Expression {
public:
    explicit DereferenceExpr(ExprPtr ptr) : pointer(std::move(ptr)) {}
    ExprPtr pointer;
};

// Represents a vector access expression (e.g., V!I).
class VectorAccess : public Expression {
public:
    ExprPtr vector;
    ExprPtr index;

    VectorAccess(ExprPtr vector, ExprPtr index)
        : vector(std::move(vector)), index(std::move(index)) {}
};

class StringAccess : public Expression {
public:
    ExprPtr string;
    ExprPtr index;

    StringAccess(ExprPtr string, ExprPtr index)
        : string(std::move(string)), index(std::move(index)) {}
};

// --- Statement Nodes ---
class Statement : public Node {};

class CompoundStatement : public Statement {
public:
    CompoundStatement(std::vector<StmtPtr> stmts) : statements(std::move(stmts)) {}
    std::vector<StmtPtr> statements;
};

class IfStatement : public Statement {
public:
    IfStatement(ExprPtr cond, StmtPtr then_stmt) : condition(std::move(cond)), then_statement(std::move(then_stmt)) {}
    ExprPtr condition;
    StmtPtr then_statement;
};

class TestStatement : public Statement {
public:
    TestStatement(ExprPtr cond, StmtPtr then_stmt, StmtPtr else_stmt)
        : condition(std::move(cond)), then_statement(std::move(then_stmt)), else_statement(std::move(else_stmt)) {}
    ExprPtr condition;
    StmtPtr then_statement;
    StmtPtr else_statement;
};

class WhileStatement : public Statement {
public:
    WhileStatement(ExprPtr cond, StmtPtr body) : condition(std::move(cond)), body(std::move(body)) {}
    ExprPtr condition;
    StmtPtr body;
};

class ForStatement : public Statement {
public:
    ForStatement(std::string var, ExprPtr from, ExprPtr to, ExprPtr by, StmtPtr body)
        : var_name(std::move(var)), from_expr(std::move(from)), to_expr(std::move(to)), by_expr(std::move(by)), body(std::move(body)) {}
    std::string var_name;
    ExprPtr from_expr;
    ExprPtr to_expr;
    ExprPtr by_expr;
    StmtPtr body;
};

class GotoStatement : public Statement {
public:
    GotoStatement(ExprPtr label) : label(std::move(label)) {}
    ExprPtr label;
};

class ReturnStatement : public Statement {};

class ResultisStatement : public Statement {
public:
    ResultisStatement(ExprPtr val) : value(std::move(val)) {}
    ExprPtr value;
};

class LabeledStatement : public Statement {
public:
    LabeledStatement(std::string name, StmtPtr stmt) : name(std::move(name)), statement(std::move(stmt)) {}
    std::string name;
    StmtPtr statement;
};

class Assignment : public Statement {
public:
    Assignment(std::vector<ExprPtr> lhs, std::vector<ExprPtr> rhs) : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    std::vector<ExprPtr> lhs;
    std::vector<ExprPtr> rhs;
};

class RoutineCall : public Statement {
public:
    RoutineCall(ExprPtr call) : call_expression(std::move(call)) {}
    ExprPtr call_expression;
};

class RepeatStatement : public Statement {
public:
    RepeatStatement(StmtPtr body, ExprPtr cond) : body(std::move(body)), condition(std::move(cond)) {}
    StmtPtr body;
    ExprPtr condition;
};

class LoopStatement : public Statement {};

class BreakStatement : public Statement {};

class EndcaseStatement : public Statement {};

class FinishStatement : public Statement {};

class SwitchonStatement : public Statement {
public:
    struct SwitchCase {
        int value;
        std::string label; // Label for the case's code block
        StmtPtr statement;
    };
    SwitchonStatement(ExprPtr expr, std::vector<SwitchCase> cases, StmtPtr default_case)
        : expression(std::move(expr)), cases(std::move(cases)), default_case(std::move(default_case)) {}
    ExprPtr expression;
    std::vector<SwitchCase> cases;
    StmtPtr default_case; // Can be nullptr
};

#endif // AST_NODES_H