#ifndef AST_H
#define AST_H

#include <string>
#include <vector>
#include <memory>
#include "Lexer.h" // For TokenType

// --- Forward Declarations ---
class Expression;
class Statement;
class Declaration;

// --- Base Node Class ---
// The base class for all nodes in the Abstract Syntax Tree.
class Node {
public:
    virtual ~Node() = default;
};

// --- Expression Nodes ---
class Expression : public Node {};

using ExprPtr = std::unique_ptr<Expression>;

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

// --- Statement Nodes ---
class Statement : public Node {};

using StmtPtr = std::unique_ptr<Statement>;

// --- New Statement Node Definitions ---
// Represents a SWITCHON statement.
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

// Represents a BREAK statement.
class BreakStatement : public Statement {};

// Represents a LOOP statement.
class LoopStatement : public Statement {};

// Represents a REPEAT UNTIL loop.
class RepeatStatement : public Statement {
public:
    RepeatStatement(StmtPtr body, ExprPtr cond)
        : body(std::move(body)), condition(std::move(cond)) {}
    StmtPtr body;
    ExprPtr condition;
};

// Represents an ENDCASE statement.
class EndcaseStatement : public Statement {};

// --- New Expression Node Definitions ---
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
    VectorAccess(ExprPtr vec, ExprPtr idx) : vector(std::move(vec)), index(std::move(idx)) {}
    ExprPtr vector;
    ExprPtr index;
};

// Represents a character access expression (e.g., S%I).
class CharacterAccess : public Expression {
public:
    CharacterAccess(ExprPtr str, ExprPtr idx) : string(std::move(str)), index(std::move(idx)) {}
    ExprPtr string;
    ExprPtr index;
};

// Represents an assignment (e.g., V!i := E).
class Assignment : public Statement {
public:
    Assignment(std::vector<ExprPtr> lhs, std::vector<ExprPtr> rhs)
        : lhs(std::move(lhs)), rhs(std::move(rhs)) {}
    std::vector<ExprPtr> lhs;
    std::vector<ExprPtr> rhs;
};

// Represents a call to a routine (which doesn't return a value).
class RoutineCall : public Statement {
public:
    explicit RoutineCall(ExprPtr call_expr) : call_expression(std::move(call_expr)) {}
    ExprPtr call_expression;
};

// Represents a block of statements, e.g., $( C1; C2 $)
class CompoundStatement : public Statement {
public:
    explicit CompoundStatement(std::vector<StmtPtr> stmts) : statements(std::move(stmts)) {}
    std::vector<StmtPtr> statements;
};


// Represents an IF/UNLESS command.
class IfStatement : public Statement {
public:
    IfStatement(ExprPtr cond, StmtPtr then_stmt)
        : condition(std::move(cond)), then_statement(std::move(then_stmt)) {}
    ExprPtr condition;
    StmtPtr then_statement;
};

// Represents a TEST command.
class TestStatement : public Statement {
public:
    TestStatement(ExprPtr cond, StmtPtr then_stmt, StmtPtr else_stmt)
        : condition(std::move(cond)),
          then_statement(std::move(then_stmt)),
          else_statement(std::move(else_stmt)) {}
    ExprPtr condition;
    StmtPtr then_statement;
    StmtPtr else_statement;
};

// Represents a WHILE/UNTIL loop.
class WhileStatement : public Statement {
public:
    WhileStatement(ExprPtr cond, StmtPtr body)
        : condition(std::move(cond)), body(std::move(body)) {}
    ExprPtr condition;
    StmtPtr body;
};

// Represents a FOR loop.
class ForStatement : public Statement {
public:
    ForStatement(std::string var, ExprPtr from, ExprPtr to, ExprPtr by, StmtPtr body)
        : var_name(std::move(var)), from_expr(std::move(from)),
          to_expr(std::move(to)), by_expr(std::move(by)), body(std::move(body)) {}
    std::string var_name;
    ExprPtr from_expr;
    ExprPtr to_expr;
    ExprPtr by_expr; // Can be nullptr for default BY 1
    StmtPtr body;
};

// Represents a GOTO statement.
class GotoStatement : public Statement {
public:
    explicit GotoStatement(ExprPtr label_expr) : label(std::move(label_expr)) {}
    ExprPtr label;
};

// Represents a labeled statement.
class LabeledStatement : public Statement {
public:
    LabeledStatement(std::string name, StmtPtr stmt) : name(std::move(name)), statement(std::move(stmt)) {}
    std::string name;
    StmtPtr statement;
};

// Represents a RETURN statement.
class ReturnStatement : public Statement {};

// Represents a FINISH statement.
class FinishStatement : public Statement {};

// Represents a RESULTIS statement.
class ResultisStatement : public Statement {
public:
    explicit ResultisStatement(ExprPtr val) : value(std::move(val)) {}
    ExprPtr value;
};

// --- Declaration Nodes ---
class Declaration : public Statement {}; // Declarations can appear in statement lists
using DeclPtr = std::unique_ptr<Declaration>;

class GetDirective : public Declaration {
public:
    explicit GetDirective(std::string file) : filename(std::move(file)) {}
    std::string filename;
};

// Represents a LET declaration for variables.
class LetDeclaration : public Declaration {
public:
    struct VarInit {
        std::string name;
        ExprPtr init; // Can be nullptr
    };
    explicit LetDeclaration(std::vector<VarInit> inits) : initializers(std::move(inits)) {}
    std::vector<VarInit> initializers;
};

class GlobalDeclaration : public Declaration {
public:
    struct Global {
        std::string name;
        int size;
    };
    explicit GlobalDeclaration(std::vector<Global> globals) : globals(std::move(globals)) {}
    std::vector<Global> globals;
};

class ManifestDeclaration : public Declaration {
public:
    struct Manifest {
        std::string name;
        int value;
    };
    explicit ManifestDeclaration(std::vector<Manifest> manifests) : manifests(std::move(manifests)) {}
    std::vector<Manifest> manifests;
};

// Represents a function or routine declaration.
class FunctionDeclaration : public Declaration {
public:
    FunctionDeclaration(std::string name, std::vector<std::string> params, ExprPtr body_expr, StmtPtr body_stmt)
        : name(std::move(name)), params(std::move(params)),
          body_expr(std::move(body_expr)), body_stmt(std::move(body_stmt)) {}
    std::string name;
    std::vector<std::string> params;
    ExprPtr body_expr; // For LET F() = E
    StmtPtr body_stmt; // For LET R() BE C
};


// --- Program Node ---
// The root of the entire AST.
class Program : public Node {
public:
    explicit Program(std::vector<DeclPtr> decls) : declarations(std::move(decls)) {}
    std::vector<DeclPtr> declarations;
};

using ProgramPtr = std::unique_ptr<Program>;

#endif // AST_H