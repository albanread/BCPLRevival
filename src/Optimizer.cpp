#include "Optimizer.h"
#include <vector>
#include <memory>

// --- Main Entry Point ---

ProgramPtr Optimizer::optimize(ProgramPtr ast) {
    return visit(ast.get());
}

// --- Visitor Dispatchers ---

ExprPtr Optimizer::visit(Expression* node) {
    if (auto* n = dynamic_cast<NumberLiteral*>(node))   return visit(n);
    if (auto* n = dynamic_cast<FloatLiteral*>(node))    return visit(n);
    if (auto* n = dynamic_cast<StringLiteral*>(node))   return visit(n);
    if (auto* n = dynamic_cast<CharLiteral*>(node))     return visit(n);
    if (auto* n = dynamic_cast<VariableAccess*>(node))  return visit(n);
    if (auto* n = dynamic_cast<UnaryOp*>(node))         return visit(n);
    if (auto* n = dynamic_cast<BinaryOp*>(node))        return visit(n);
    if (auto* n = dynamic_cast<FunctionCall*>(node))    return visit(n);
    if (auto* n = dynamic_cast<ConditionalExpression*>(node)) return visit(n);
    if (auto* n = dynamic_cast<Valof*>(node))           return visit(n);
    throw std::runtime_error("Optimizer: Unsupported Expression node.");
}

StmtPtr Optimizer::visit(Statement* node) {
    if (auto* n = dynamic_cast<Assignment*>(node))         return visit(n);
    if (auto* n = dynamic_cast<RoutineCall*>(node))        return visit(n);
    if (auto* n = dynamic_cast<CompoundStatement*>(node))  return visit(n);
    if (auto* n = dynamic_cast<IfStatement*>(node))          return visit(n);
    if (auto* n = dynamic_cast<TestStatement*>(node))        return visit(n);
    if (auto* n = dynamic_cast<WhileStatement*>(node))       return visit(n);
    if (auto* n = dynamic_cast<ForStatement*>(node))         return visit(n);
    if (auto* n = dynamic_cast<GotoStatement*>(node))        return visit(n);
    if (auto* n = dynamic_cast<LabeledStatement*>(node))     return visit(n);
    if (auto* n = dynamic_cast<ReturnStatement*>(node))      return visit(n);
    if (auto* n = dynamic_cast<FinishStatement*>(node))      return visit(n);
    if (auto* n = dynamic_cast<ResultisStatement*>(node))    return visit(n);
    if (auto* n = dynamic_cast<Declaration*>(node))          return visit(dynamic_cast<Declaration*>(node));
    throw std::runtime_error("Optimizer: Unsupported Statement node.");
}

DeclPtr Optimizer::visit(Declaration* node) {
    if (auto* n = dynamic_cast<LetDeclaration*>(node))       return visit(n);
    if (auto* n = dynamic_cast<FunctionDeclaration*>(node))  return visit(n);
    throw std::runtime_error("Optimizer: Unsupported Declaration node.");
}


// --- Expression Visitors ---

// Literals and variables are leaves; just create new copies as they cannot be optimized further.
ExprPtr Optimizer::visit(NumberLiteral* node) { return std::make_unique<NumberLiteral>(*node); }
ExprPtr Optimizer::visit(FloatLiteral* node) { return std::make_unique<FloatLiteral>(*node); }
ExprPtr Optimizer::visit(StringLiteral* node) { return std::make_unique<StringLiteral>(*node); }
ExprPtr Optimizer::visit(CharLiteral* node) { return std::make_unique<CharLiteral>(*node); }
ExprPtr Optimizer::visit(VariableAccess* node) { return std::make_unique<VariableAccess>(*node); }

ExprPtr Optimizer::visit(UnaryOp* node) {
    auto new_rhs = visit(node->rhs.get());
    return std::make_unique<UnaryOp>(node->op, std::move(new_rhs));
}

ExprPtr Optimizer::visit(BinaryOp* node) {
    // Recursively optimize the left and right children first.
    auto new_left = visit(node->left.get());
    auto new_right = visit(node->right.get());

    // --- INTEGER CONSTANT FOLDING ---
    if (auto* left_lit = dynamic_cast<NumberLiteral*>(new_left.get())) {
        if (auto* right_lit = dynamic_cast<NumberLiteral*>(new_right.get())) {
            int64_t l = left_lit->value;
            int64_t r = right_lit->value;
            switch (node->op) {
                case TokenType::OpPlus:     return std::make_unique<NumberLiteral>(l + r);
                case TokenType::OpMinus:    return std::make_unique<NumberLiteral>(l - r);
                case TokenType::OpMultiply: return std::make_unique<NumberLiteral>(l * r);
                case TokenType::OpDivide:   if (r != 0) return std::make_unique<NumberLiteral>(l / r); break;
                case TokenType::OpRemainder:if (r != 0) return std::make_unique<NumberLiteral>(l % r); break;
                case TokenType::OpLshift:   return std::make_unique<NumberLiteral>(l << r);
                case TokenType::OpRshift:   return std::make_unique<NumberLiteral>(l >> r);
                case TokenType::OpLogAnd:   return std::make_unique<NumberLiteral>(l & r);
                case TokenType::OpLogOr:    return std::make_unique<NumberLiteral>(l | r);
                case TokenType::OpLogEqv:   return std::make_unique<NumberLiteral>(~(l ^ r));
                case TokenType::OpLogNeqv:  return std::make_unique<NumberLiteral>(l ^ r);
                case TokenType::OpEq:       return std::make_unique<NumberLiteral>(l == r ? -1 : 0);
                case TokenType::OpNe:       return std::make_unique<NumberLiteral>(l != r ? -1 : 0);
                case TokenType::OpLt:       return std::make_unique<NumberLiteral>(l < r ? -1 : 0);
                case TokenType::OpLe:       return std::make_unique<NumberLiteral>(l <= r ? -1 : 0);
                case TokenType::OpGt:       return std::make_unique<NumberLiteral>(l > r ? -1 : 0);
                case TokenType::OpGe:       return std::make_unique<NumberLiteral>(l >= r ? -1 : 0);
                default: break;
            }
        }
    }
    
    // --- FLOAT CONSTANT FOLDING ---
    if (auto* left_lit_f = dynamic_cast<FloatLiteral*>(new_left.get())) {
        if (auto* right_lit_f = dynamic_cast<FloatLiteral*>(new_right.get())) {
            double l = left_lit_f->value;
            double r = right_lit_f->value;
             switch (node->op) {
                case TokenType::OpFloatPlus:     return std::make_unique<FloatLiteral>(l + r);
                case TokenType::OpFloatMinus:    return std::make_unique<FloatLiteral>(l - r);
                case TokenType::OpFloatMultiply: return std::make_unique<FloatLiteral>(l * r);
                case TokenType::OpFloatDivide:   if (r != 0.0) return std::make_unique<FloatLiteral>(l / r); break;
                case TokenType::OpFloatEq:       return std::make_unique<NumberLiteral>(l == r ? -1 : 0);
                case TokenType::OpFloatNe:       return std::make_unique<NumberLiteral>(l != r ? -1 : 0);
                case TokenType::OpFloatLt:       return std::make_unique<NumberLiteral>(l < r ? -1 : 0);
                case TokenType::OpFloatLe:       return std::make_unique<NumberLiteral>(l <= r ? -1 : 0);
                case TokenType::OpFloatGt:       return std::make_unique<NumberLiteral>(l > r ? -1 : 0);
                case TokenType::OpFloatGe:       return std::make_unique<NumberLiteral>(l >= r ? -1 : 0);
                default: break;
            }
        }
    }

    // If folding was not possible, return a new BinaryOp with the optimized children.
    return std::make_unique<BinaryOp>(node->op, std::move(new_left), std::move(new_right));
}

ExprPtr Optimizer::visit(FunctionCall* node) {
    auto new_func = visit(node->function.get());
    std::vector<ExprPtr> new_args;
    for (const auto& arg : node->arguments) {
        new_args.push_back(visit(arg.get()));
    }
    return std::make_unique<FunctionCall>(std::move(new_func), std::move(new_args));
}

ExprPtr Optimizer::visit(ConditionalExpression* node) {
    auto new_cond = visit(node->condition.get());
    auto new_true = visit(node->trueExpr.get());
    auto new_false = visit(node->falseExpr.get());
    return std::make_unique<ConditionalExpression>(std::move(new_cond), std::move(new_true), std::move(new_false));
}

ExprPtr Optimizer::visit(Valof* node) {
    auto new_body = visit(node->body.get());
    return std::make_unique<Valof>(std::move(new_body));
}


// --- Statement and Declaration Visitors ---

ProgramPtr Optimizer::visit(Program* node) {
    std::vector<DeclPtr> new_decls;
    for (const auto& decl : node->declarations) {
        new_decls.push_back(visit(decl.get()));
    }
    return std::make_unique<Program>(std::move(new_decls));
}

DeclPtr Optimizer::visit(FunctionDeclaration* node) {
    StmtPtr new_body_stmt = nullptr;
    ExprPtr new_body_expr = nullptr;
    if (node->body_stmt) new_body_stmt = visit(node->body_stmt.get());
    if (node->body_expr) new_body_expr = visit(node->body_expr.get());
    
    return std::make_unique<FunctionDeclaration>(
        node->name, node->params, std::move(new_body_expr), std::move(new_body_stmt)
    );
}

DeclPtr Optimizer::visit(LetDeclaration* node) {
    std::vector<LetDeclaration::VarInit> new_inits;
    for (const auto& init : node->initializers) {
        new_inits.push_back({init.name, init.init ? visit(init.init.get()) : nullptr});
    }
    return std::make_unique<LetDeclaration>(std::move(new_inits));
}

StmtPtr Optimizer::visit(CompoundStatement* node) {
    std::vector<StmtPtr> new_stmts;
    for (const auto& stmt : node->statements) {
        new_stmts.push_back(visit(stmt.get()));
    }
    return std::make_unique<CompoundStatement>(std::move(new_stmts));
}

StmtPtr Optimizer::visit(Assignment* node) {
    std::vector<ExprPtr> new_lhs, new_rhs;
    for (const auto& expr : node->lhs) new_lhs.push_back(visit(expr.get()));
    for (const auto& expr : node->rhs) new_rhs.push_back(visit(expr.get()));
    return std::make_unique<Assignment>(std::move(new_lhs), std::move(new_rhs));
}

StmtPtr Optimizer::visit(IfStatement* node) {
    auto new_cond = visit(node->condition.get());
    auto new_then = visit(node->then_statement.get());
    return std::make_unique<IfStatement>(std::move(new_cond), std::move(new_then));
}

StmtPtr Optimizer::visit(TestStatement* node) {
    auto new_cond = visit(node->condition.get());
    auto new_then = visit(node->then_statement.get());
    auto new_else = node->else_statement ? visit(node->else_statement.get()) : nullptr;
    return std::make_unique<TestStatement>(std::move(new_cond), std::move(new_then), std::move(new_else));
}

StmtPtr Optimizer::visit(WhileStatement* node) {
    auto new_cond = visit(node->condition.get());
    auto new_body = visit(node->body.get());
    return std::make_unique<WhileStatement>(std::move(new_cond), std::move(new_body));
}

StmtPtr Optimizer::visit(ForStatement* node) {
    auto new_from = visit(node->from_expr.get());
    auto new_to = visit(node->to_expr.get());
    auto new_by = node->by_expr ? visit(node->by_expr.get()) : nullptr;
    auto new_body = visit(node->body.get());
    return std::make_unique<ForStatement>(node->var_name, std::move(new_from), std::move(new_to), std::move(new_by), std::move(new_body));
}

StmtPtr Optimizer::visit(GotoStatement* node) {
    auto new_label = visit(node->label.get());
    return std::make_unique<GotoStatement>(std::move(new_label));
}

StmtPtr Optimizer::visit(ResultisStatement* node) {
    auto new_value = visit(node->value.get());
    return std::make_unique<ResultisStatement>(std::move(new_value));
}

StmtPtr Optimizer::visit(ReturnStatement* node) { return std::make_unique<ReturnStatement>(*node); }
StmtPtr Optimizer::visit(FinishStatement* node) { return std::make_unique<FinishStatement>(*node); }

StmtPtr Optimizer::visit(RoutineCall* node) {
    return std::make_unique<RoutineCall>(visit(node->call_expression.get()));
}

StmtPtr Optimizer::visit(LabeledStatement* node) {
    return std::make_unique<LabeledStatement>(node->name, visit(node->statement.get()));
}

