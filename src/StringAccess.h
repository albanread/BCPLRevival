#ifndef STRING_ACCESS_H
#define STRING_ACCESS_H

#include "AST.h"

class StringAccess : public Expression {
public:
    ExprPtr string;
    ExprPtr index;

    StringAccess(ExprPtr string, ExprPtr index)
        : string(std::move(string)), index(std::move(index)) {}
};

#endif // STRING_ACCESS_H
