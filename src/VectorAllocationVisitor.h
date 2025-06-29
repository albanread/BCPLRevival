#ifndef VECTOR_ALLOCATION_VISITOR_H
#define VECTOR_ALLOCATION_VISITOR_H

#include "AST.h"
#include <vector>

class VectorAllocationVisitor {
public:
    std::vector<const VectorConstructor*> allocations;

    void visit(const Node* node) {
        if (auto funcDecl = dynamic_cast<const FunctionDeclaration*>(node)) {
            if (funcDecl->body_expr) {
                visit(funcDecl->body_expr.get());
            } else if (funcDecl->body_stmt) {
                visit(funcDecl->body_stmt.get());
            }
        } else if (auto valof = dynamic_cast<const Valof*>(node)) {
            visit(valof->body.get());
        } else if (auto compound = dynamic_cast<const CompoundStatement*>(node)) {
            for (const auto& stmt : compound->statements) {
                visit(stmt.get());
            }
        } else if (auto let = dynamic_cast<const LetDeclaration*>(node)) {
            for (const auto& init : let->initializers) {
                if (init.init) {
                    visit(init.init.get());
                }
            }
        } else if (auto vec = dynamic_cast<const VectorConstructor*>(node)) {
            allocations.push_back(vec);
        }
    }
};

#endif // VECTOR_ALLOCATION_VISITOR_H
