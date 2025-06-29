#include "DebugPrinter.h"
#include <iomanip> // For std::setw
#include <map>

// Helper to convert TokenType to a string for printing.
std::string tokenTypeToString(TokenType type) {
    // A comprehensive map for all token types.
    static const std::map<TokenType, std::string> typeMap = {
        {TokenType::Eof, "EOF"},
        {TokenType::Identifier, "Identifier"},
        {TokenType::IntegerLiteral, "IntLiteral"},
        {TokenType::FloatLiteral, "FloatLiteral"},
        {TokenType::StringLiteral, "StringLiteral"},
        {TokenType::CharLiteral, "CharLiteral"},
        {TokenType::KwLet, "LET"}, {TokenType::KwAnd, "AND"}, {TokenType::KwBe, "BE"},
        {TokenType::KwVec, "VEC"}, {TokenType::KwIf, "IF"}, {TokenType::KwThen, "THEN"},
        {TokenType::KwUnless, "UNLESS"}, {TokenType::KwTest, "TEST"}, {TokenType::KwOr, "OR"},
        {TokenType::KwWhile, "WHILE"}, {TokenType::KwDo, "DO"}, {TokenType::KwUntil, "UNTIL"},
        {TokenType::KwRepeat, "REPEAT"}, {TokenType::KwRepeatWhile, "REPEATWHILE"},
        {TokenType::KwRepeatUntil, "REPEATUNTIL"}, {TokenType::KwFor, "FOR"},
        {TokenType::KwTo, "TO"}, {TokenType::KwBy, "BY"},
        {TokenType::KwSwitchon, "SWITCHON"}, {TokenType::KwInto, "INTO"},
        {TokenType::KwCase, "CASE"}, {TokenType::KwDefault, "DEFAULT"},
        {TokenType::KwEndcase, "ENDCASE"}, {TokenType::KwGoto, "GOTO"},
        {TokenType::KwReturn, "RETURN"},
        {TokenType::KwResultis, "RESULTIS"}, {TokenType::KwBreak, "BREAK"},
        {TokenType::KwLoop, "LOOP"}, {TokenType::KwValof, "VALOF"},
        {TokenType::KwManifest, "MANIFEST"}, {TokenType::KwStatic, "STATIC"},
        {TokenType::KwGlobal, "GLOBAL"}, {TokenType::KwTrue, "TRUE"}, {TokenType::KwFalse, "FALSE"},
        {TokenType::OpAssign, "Op ':='"}, {TokenType::OpPlus, "Op '+'"},
        {TokenType::OpMinus, "Op '-'"}, {TokenType::OpMultiply, "Op '*'"},
        {TokenType::OpDivide, "Op '/'"}, {TokenType::OpRemainder, "Op 'REM'"},
        {TokenType::OpEq, "Op '='"}, {TokenType::OpNe, "Op '~='"}, {TokenType::OpLt, "Op '<'"},
        {TokenType::OpGt, "Op '>'"}, {TokenType::OpLe, "Op '<='"}, {TokenType::OpGe, "Op '>='"},
        {TokenType::OpLogAnd, "Op '&'"}, {TokenType::OpLogOr, "Op '|'"},
        {TokenType::OpLogNot, "Op '~'"}, {TokenType::OpLogEqv, "Op 'EQV'"},
        {TokenType::OpLogNeqv, "Op 'NEQV'"}, {TokenType::OpLshift, "Op '<<'"},
        {TokenType::OpRshift, "Op '>>'"}, {TokenType::OpAt, "Op '@'"},
        {TokenType::OpBang, "Op '!'"}, {TokenType::OpConditional, "Op '->'"},
        {TokenType::OpFloatPlus, "Op '+.'"}, {TokenType::OpFloatMinus, "Op '-.'"},
        {TokenType::OpFloatMultiply, "Op '*.'"}, {TokenType::OpFloatDivide, "Op '/.'"},
        {TokenType::OpFloatEq, "Op '=.'"}, {TokenType::OpFloatNe, "Op '~=.'"},
        {TokenType::OpFloatLt, "Op '<.'"}, {TokenType::OpFloatGt, "Op '>.'"},
        {TokenType::OpFloatLe, "Op '<=.'"}, {TokenType::OpFloatGe, "Op '>=.'"},
        {TokenType::OpFloatVecSub, "Op '.%'"}, {TokenType::OpCharSub, "Op '%'"},
        {TokenType::LParen, "LParen '('"}, {TokenType::RParen, "RParen ')'"},
        {TokenType::LBrace, "LBrace '{'"}, {TokenType::RBrace, "RBrace '}'"},
        {TokenType::LSection, "LSection '$('"}, {TokenType::RSection, "RSection '$)'"},
        {TokenType::Comma, "Comma ','"}, {TokenType::Colon, "Colon ':'"},
        {TokenType::Semicolon, "Semicolon ';'"}, {TokenType::Illegal, "Illegal"}
    };
    if (typeMap.count(type)) {
        return typeMap.at(type);
    }
    return "UnknownToken";
}

void DebugPrinter::printTokens(const std::string& source) {
    Lexer& lexer = Lexer::getInstance();
    lexer.init(source);

    std::cout << "\n--- TOKEN STREAM ---" << std::endl;
    std::cout << std::left << std::setw(8) << "Line"
              << std::setw(8) << "Col"
              << std::setw(20) << "Type"
              << "Text" << std::endl;
    std::cout << "-----------------------------------------------------" << std::endl;

    Token token;
    do {
        token = lexer.getNextToken();
        std::cout << std::left << std::setw(8) << token.line
                  << std::setw(8) << token.col
                  << std::setw(20) << tokenTypeToString(token.type)
                  << "'" << token.text << "'" << std::endl;
    } while (token.type != TokenType::Eof);
    std::cout << "-----------------------------------------------------\n" << std::endl;
}

void DebugPrinter::printAST(const ProgramPtr& ast) {
    std::cout << "\n--- ABSTRACT SYNTAX TREE ---" << std::endl;
    visit(ast.get(), 0);
    std::cout << "---------------------------\n" << std::endl;
}

void DebugPrinter::indent(int level) {
    for (int i = 0; i < level; ++i) {
        std::cout << "|  ";
    }
}

// --- Visitor Dispatcher ---
void DebugPrinter::visit(Node* node, int indent_level) {
    if (!node) return;
    if (auto* n = dynamic_cast<Program*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<FunctionDeclaration*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<LetDeclaration*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<NumberLiteral*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<FloatLiteral*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<StringLiteral*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<CharLiteral*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<VariableAccess*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<UnaryOp*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<BinaryOp*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<FunctionCall*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<ConditionalExpression*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<Valof*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<Assignment*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<RoutineCall*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<CompoundStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<IfStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<TestStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<WhileStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<ForStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<GotoStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<LabeledStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<ReturnStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<FinishStatement*>(node)) visit(n, indent_level);
    else if (auto* n = dynamic_cast<ResultisStatement*>(node)) visit(n, indent_level);
    else std::cout << "Unknown AST Node" << std::endl;
}

// --- Specific Visitors ---
void DebugPrinter::visit(Program* node, int i) {
    indent(i); std::cout << "Program" << std::endl;
    for (const auto& decl : node->declarations) visit(decl.get(), i + 1);
}

void DebugPrinter::visit(FunctionDeclaration* node, int i) {
    indent(i); std::cout << (node->body_expr ? "FunctionDecl " : "RoutineDecl ") << node->name << "(";
    for (size_t j = 0; j < node->params.size(); ++j) std::cout << node->params[j] << (j == node->params.size() - 1 ? "" : ", ");
    std::cout << ")" << std::endl;
    if(node->body_expr) visit(node->body_expr.get(), i + 1);
    if(node->body_stmt) visit(node->body_stmt.get(), i + 1);
}

void DebugPrinter::visit(LetDeclaration* node, int i) {
    indent(i); std::cout << "LetDecl" << std::endl;
    for (const auto& var : node->initializers) {
        indent(i + 1); std::cout << "Var " << var.name << std::endl;
        if(var.init) visit(var.init.get(), i + 2);
    }
}

void DebugPrinter::visit(CompoundStatement* node, int i) {
    indent(i); std::cout << "CompoundStatement" << std::endl;
    for (const auto& stmt : node->statements) visit(stmt.get(), i + 1);
}

void DebugPrinter::visit(Assignment* node, int i) {
    indent(i); std::cout << "Assignment" << std::endl;
    indent(i + 1); std::cout << "LHS:" << std::endl;
    for(const auto& lhs : node->lhs) visit(lhs.get(), i + 2);
    indent(i + 1); std::cout << "RHS:" << std::endl;
    for(const auto& rhs : node->rhs) visit(rhs.get(), i + 2);
}

void DebugPrinter::visit(RoutineCall* node, int i) {
    indent(i); std::cout << "RoutineCall" << std::endl;
    visit(node->call_expression.get(), i + 1);
}

void DebugPrinter::visit(IfStatement* node, int i) {
    indent(i); std::cout << "IfStatement" << std::endl;
    indent(i + 1); std::cout << "Condition:" << std::endl;
    visit(node->condition.get(), i + 2);
    indent(i + 1); std::cout << "Then:" << std::endl;
    visit(node->then_statement.get(), i + 2);
}

void DebugPrinter::visit(TestStatement* node, int i) {
    indent(i); std::cout << "TestStatement" << std::endl;
    indent(i + 1); std::cout << "Condition:" << std::endl;
    visit(node->condition.get(), i + 2);
    indent(i + 1); std::cout << "Then:" << std::endl;
    visit(node->then_statement.get(), i + 2);
    if (node->else_statement) {
        indent(i + 1); std::cout << "Else:" << std::endl;
        visit(node->else_statement.get(), i + 2);
    }
}

void DebugPrinter::visit(WhileStatement* node, int i) {
    indent(i); std::cout << "WhileStatement" << std::endl;
    indent(i + 1); std::cout << "Condition:" << std::endl;
    visit(node->condition.get(), i + 2);
    indent(i + 1); std::cout << "Body:" << std::endl;
    visit(node->body.get(), i + 2);
}

void DebugPrinter::visit(ForStatement* node, int i) {
    indent(i); std::cout << "ForStatement (Var: " << node->var_name << ")" << std::endl;
    indent(i + 1); std::cout << "From:" << std::endl;
    visit(node->from_expr.get(), i + 2);
    indent(i + 1); std::cout << "To:" << std::endl;
    visit(node->to_expr.get(), i + 2);
    if (node->by_expr) {
        indent(i + 1); std::cout << "By:" << std::endl;
        visit(node->by_expr.get(), i + 2);
    }
    indent(i + 1); std::cout << "Body:" << std::endl;
    visit(node->body.get(), i + 2);
}

void DebugPrinter::visit(GotoStatement* node, int i) {
    indent(i); std::cout << "GotoStatement" << std::endl;
    indent(i + 1); std::cout << "Label:" << std::endl;
    visit(node->label.get(), i + 2);
}

void DebugPrinter::visit(LabeledStatement* node, int i) {
    indent(i); std::cout << "Label: " << node->name << std::endl;
    visit(node->statement.get(), i);
}

void DebugPrinter::visit(ReturnStatement* node, int i) {
    indent(i); std::cout << "ReturnStatement" << std::endl;
}

void DebugPrinter::visit(FinishStatement* node, int i) {
    indent(i); std::cout << "FinishStatement" << std::endl;
}

void DebugPrinter::visit(ResultisStatement* node, int i) {
    indent(i); std::cout << "ResultisStatement" << std::endl;
    indent(i + 1); std::cout << "Value:" << std::endl;
    visit(node->value.get(), i + 2);
}

void DebugPrinter::visit(Valof* node, int i) {
    indent(i); std::cout << "Valof" << std::endl;
    indent(i + 1); std::cout << "Body:" << std::endl;
    visit(node->body.get(), i + 2);
}

void DebugPrinter::visit(ConditionalExpression* node, int i) {
    indent(i); std::cout << "ConditionalExpression" << std::endl;
    indent(i + 1); std::cout << "Condition:" << std::endl;
    visit(node->condition.get(), i + 2);
    indent(i + 1); std::cout << "True-Expr:" << std::endl;
    visit(node->trueExpr.get(), i + 2);
    indent(i + 1); std::cout << "False-Expr:" << std::endl;
    visit(node->falseExpr.get(), i + 2);
}

void DebugPrinter::visit(BinaryOp* node, int i) {
    indent(i); std::cout << "BinaryOp: " << tokenTypeToString(node->op) << std::endl;
    visit(node->left.get(), i + 1);
    visit(node->right.get(), i + 1);
}

void DebugPrinter::visit(UnaryOp* node, int i) {
    indent(i); std::cout << "UnaryOp: " << tokenTypeToString(node->op) << std::endl;
    visit(node->rhs.get(), i + 1);
}

void DebugPrinter::visit(NumberLiteral* node, int i) {
    indent(i); std::cout << "IntLiteral: " << node->value << std::endl;
}

void DebugPrinter::visit(FloatLiteral* node, int i) {
    indent(i); std::cout << "FloatLiteral: " << node->value << std::endl;
}

void DebugPrinter::visit(StringLiteral* node, int i) {
    indent(i); std::cout << "StringLiteral: \"" << node->value << "\"" << std::endl;
}

void DebugPrinter::visit(CharLiteral* node, int i) {
    indent(i); std::cout << "CharLiteral: '" << (char)node->value << "'" << std::endl;
}

void DebugPrinter::visit(VariableAccess* node, int i) {
    indent(i); std::cout << "Variable: " << node->name << std::endl;
}

void DebugPrinter::visit(FunctionCall* node, int i) {
    indent(i); std::cout << "FunctionCall" << std::endl;
    indent(i + 1); std::cout << "Function:" << std::endl;
    visit(node->function.get(), i + 2);
    if (!node->arguments.empty()) {
        indent(i + 1); std::cout << "Arguments:" << std::endl;
        for (const auto& arg : node->arguments) {
            visit(arg.get(), i + 2);
        }
    }
}

