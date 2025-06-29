#include "Lexer.h"
#include <unordered_map>
#include <cctype>
#include <stdexcept>

// A map to associate BCPL keywords with their corresponding token types.
const std::unordered_map<std::string, TokenType> keywords = {
    {"LET", TokenType::KwLet}, {"AND", TokenType::KwAnd}, {"BE", TokenType::KwBe},
    {"VEC", TokenType::KwVec}, {"IF", TokenType::KwIf}, {"THEN", TokenType::KwThen},
    {"UNLESS", TokenType::KwUnless}, {"TEST", TokenType::KwTest}, {"OR", TokenType::KwOr},
    {"WHILE", TokenType::KwWhile}, {"DO", TokenType::KwDo}, {"UNTIL", TokenType::KwUntil},
    {"REPEAT", TokenType::KwRepeat}, {"REPEATWHILE", TokenType::KwRepeatWhile},
    {"REPEATUNTIL", TokenType::KwRepeatUntil}, {"FOR", TokenType::KwFor},
    {"TO", TokenType::KwTo}, {"BY", TokenType::KwBy}, {"SWITCHON", TokenType::KwSwitchon},
    {"INTO", TokenType::KwInto}, {"CASE", TokenType::KwCase}, {"DEFAULT", TokenType::KwDefault},
    {"ENDCASE", TokenType::KwEndcase}, {"GOTO", TokenType::KwGoto},
    {"RETURN", TokenType::KwReturn},
    {"RESULTIS", TokenType::KwResultis}, {"BREAK", TokenType::KwBreak},
    {"LOOP", TokenType::KwLoop}, {"VALOF", TokenType::KwValof},
    {"MANIFEST", TokenType::KwManifest}, {"STATIC", TokenType::KwStatic},
    {"GLOBAL", TokenType::KwGlobal}, {"TRUE", TokenType::KwTrue}, {"FALSE", TokenType::KwFalse},
    // Operators that are parsed as identifiers
    {"REM", TokenType::OpRemainder}, {"EQV", TokenType::OpLogEqv}, {"NEQV", TokenType::OpLogNeqv}
};


void Lexer::init(const std::string& source) {
    source_code = source;
    pos = 0;
    current_line = 1;
    current_col = 1;
}

char Lexer::peek() const {
    if (pos >= source_code.length()) return '\0';
    return source_code[pos];
}

char Lexer::peekNext() const {
    if (pos + 1 >= source_code.length()) return '\0';
    return source_code[pos + 1];
}

char Lexer::advance() {
    if (pos >= source_code.length()) return '\0';
    char current_char = source_code[pos];
    pos++;
    if (current_char == '\n') {
        current_line++;
        current_col = 1;
    } else {
        current_col++;
    }
    return current_char;
}

void Lexer::skipWhitespace() {
    while (isspace(peek())) {
        advance();
    }
}

void Lexer::skipComments() {
    while (true) {
        if (peek() == '/' && peekNext() == '/') {
            // Single line comment
            while (peek() != '\n' && peek() != '\0') {
                advance();
            }
        } else if (peek() == '/' && peekNext() == '*') {
            // Multi-line comment
            advance(); // Consume '/'
            advance(); // Consume '*'
            while (peek() != '\0' && (peek() != '*' || peekNext() != '/')) {
                advance();
            }
            if (peek() != '\0') {
                advance(); // Consume '*'
                advance(); // Consume '/'
            }
        } else {
            break; // Not a comment
        }
        skipWhitespace(); // Skip any whitespace after the comment
    }
}

Token Lexer::identifierOrKeyword() {
    std::string text;
    uint32_t start_col = current_col;
    while (isalnum(peek()) || peek() == '_') {
        text += advance();
    }

    // Check if the identifier is a keyword
    auto it = keywords.find(text);
    if (it != keywords.end()) {
        return {it->second, text, 0, 0.0, current_line, start_col};
    }

    return {TokenType::Identifier, text, 0, 0.0, current_line, start_col};
}

Token Lexer::number() {
    std::string text;
    uint32_t start_col = current_col;
    bool is_float = false;
    int base = 10;

    if (peek() == '#') {
        text += advance();
        if (toupper(peek()) == 'X') {
            text += advance();
            base = 16;
        } else {
            base = 8;
        }
    }

    while (isalnum(peek()) || peek() == '.') {
        char c = peek();
        if (c == '.') {
            if (is_float) break; // Can't have two decimal points
            is_float = true;
            text += advance();
        } else if (toupper(c) == 'E' && is_float) {
            // Scientific notation part
            text += advance();
            if (peek() == '+' || peek() == '-') {
                text += advance();
            }
        } else if ((base == 10 && isdigit(c)) ||
                   (base == 8 && c >= '0' && c <= '7') ||
                   (base == 16 && isxdigit(c))) {
            text += advance();
        } else {
            break;
        }
    }
    
    // Determine if it was a float literal based on presence of '.' or 'e'/'E'
    is_float = (text.find('.') != std::string::npos || 
                text.find('e') != std::string::npos ||
                text.find('E') != std::string::npos);

    if (is_float) {
        return {TokenType::FloatLiteral, text, 0, std::stod(text), current_line, start_col};
    } else {
        std::string num_part = text;
        if (base != 10) num_part = text.substr(1);
        if (base == 16) num_part = text.substr(2);
        return {TokenType::IntegerLiteral, text, std::stoll(num_part, nullptr, base), 0.0, current_line, start_col};
    }
}

Token Lexer::stringLiteral() {
    std::string text;
    uint32_t start_col = current_col;
    advance(); // Consume opening "
    while (peek() != '"' && peek() != '\0') {
        if (peek() == '*') { // Handle escape sequences
            advance(); // consume '*'
            char escaped = advance();
            switch (tolower(escaped)) {
                case 'n': text += '\n'; break;
                case 't': text += '\t'; break;
                case 's': text += ' '; break;
                case 'b': text += '\b'; break;
                case 'p': text += '\f'; break;
                case 'c': text += '\r'; break;
                case '"': text += '"'; break;
                case '*': text += '*'; break;
                default: text += escaped; // As per spec, unknown escapes are the char itself
            }
        } else {
            text += advance();
        }
    }
    advance(); // Consume closing "
    return {TokenType::StringLiteral, text, 0, 0.0, current_line, start_col};
}

Token Lexer::charLiteral() {
    uint32_t start_col = current_col;
    advance(); // Consume opening '
    int64_t val = (int64_t)advance(); // Read the character
    if (peek() != '\'') {
      // Handle error, for now we assume valid syntax
    }
    advance(); // Consume closing '
    return {TokenType::CharLiteral, std::string(1, (char)val), val, 0.0, current_line, start_col};
}

Token Lexer::operatorOrDelimiter() {
    uint32_t start_col = current_col;
    char c = advance();
    switch (c) {
        case '(': return {TokenType::LParen, "(", 0, 0.0, current_line, start_col};
        case ')': return {TokenType::RParen, ")", 0, 0.0, current_line, start_col};
        case '{': return {TokenType::LBrace, "{", 0, 0.0, current_line, start_col};
        case '}': return {TokenType::RBrace, "}", 0, 0.0, current_line, start_col};
        case ',': return {TokenType::Comma, ",", 0, 0.0, current_line, start_col};
        case ';': return {TokenType::Semicolon, ";", 0, 0.0, current_line, start_col};
        case '!': return {TokenType::OpBang, "!", 0, 0.0, current_line, start_col};
        case '@': return {TokenType::OpAt, "@", 0, 0.0, current_line, start_col};
        case '&': return {TokenType::OpLogAnd, "&", 0, 0.0, current_line, start_col};
        case '|': return {TokenType::OpLogOr, "|", 0, 0.0, current_line, start_col};
        case '%': return {TokenType::OpCharSub, "%", 0, 0.0, current_line, start_col};

        case '+':
            if (peek() == '.') { advance(); return {TokenType::OpFloatPlus, "+.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpPlus, "+", 0, 0.0, current_line, start_col};
        case '*':
            if (peek() == '.') { advance(); return {TokenType::OpFloatMultiply, "*.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpMultiply, "*", 0, 0.0, current_line, start_col};
        case '/':
            if (peek() == '.') { advance(); return {TokenType::OpFloatDivide, "/.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpDivide, "/", 0, 0.0, current_line, start_col};

        case '-':
            if (peek() == '>') { advance(); return {TokenType::OpConditional, "->", 0, 0.0, current_line, start_col}; }
            if (peek() == '.') { advance(); return {TokenType::OpFloatMinus, "-.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpMinus, "-", 0, 0.0, current_line, start_col};

        case ':':
            if (peek() == '=') { advance(); return {TokenType::OpAssign, ":=", 0, 0.0, current_line, start_col}; }
            return {TokenType::Colon, ":", 0, 0.0, current_line, start_col};
            
        case '~':
            if (peek() == '=') {
                advance();
                if (peek() == '.') { advance(); return {TokenType::OpFloatNe, "~=.", 0, 0.0, current_line, start_col}; }
                return {TokenType::OpNe, "~=", 0, 0.0, current_line, start_col};
            }
            return {TokenType::OpLogNot, "~", 0, 0.0, current_line, start_col};
            
        case '=':
            if (peek() == '.') { advance(); return {TokenType::OpFloatEq, "=.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpEq, "=", 0, 0.0, current_line, start_col};
        
        case '<':
            if (peek() == '=') {
                advance();
                if (peek() == '.') { advance(); return {TokenType::OpFloatLe, "<=.", 0, 0.0, current_line, start_col}; }
                return {TokenType::OpLe, "<=", 0, 0.0, current_line, start_col};
            }
            if (peek() == '<') { advance(); return {TokenType::OpLshift, "<<", 0, 0.0, current_line, start_col}; }
            if (peek() == '.') { advance(); return {TokenType::OpFloatLt, "<.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpLt, "<", 0, 0.0, current_line, start_col};
            
        case '>':
            if (peek() == '=') {
                advance();
                if (peek() == '.') { advance(); return {TokenType::OpFloatGe, ">=.", 0, 0.0, current_line, start_col}; }
                return {TokenType::OpGe, ">=", 0, 0.0, current_line, start_col};
            }
            if (peek() == '>') { advance(); return {TokenType::OpRshift, ">>", 0, 0.0, current_line, start_col}; }
            if (peek() == '.') { advance(); return {TokenType::OpFloatGt, ">.", 0, 0.0, current_line, start_col}; }
            return {TokenType::OpGt, ">", 0, 0.0, current_line, start_col};

        case '.':
            if (peek() == '%') { advance(); return {TokenType::OpFloatVecSub, ".%", 0, 0.0, current_line, start_col}; }
            // Fallthrough for '.' that might start a number like .5
            // This is handled by the number() function logic, so we shouldn't get here
            // unless it's an invalid use of '.'
            return {TokenType::Illegal, ".", 0, 0.0, current_line, start_col};

        case '$':
            if (peek() == '(') { advance(); return {TokenType::LSection, "$(", 0, 0.0, current_line, start_col}; }
            if (peek() == ')') { advance(); return {TokenType::RSection, "$)", 0, 0.0, current_line, start_col}; }
            return {TokenType::Illegal, "$", 0, 0.0, current_line, start_col};

        default:
            return {TokenType::Illegal, std::string(1, c), 0, 0.0, current_line, start_col};
    }
}


Token Lexer::getNextToken() {
    skipWhitespace();
    skipComments();

    if (pos >= source_code.length()) {
        return {TokenType::Eof, "", 0, 0.0, current_line, current_col};
    }

    char current_char = peek();

    if (isalpha(current_char) || current_char == '_') {
        return identifierOrKeyword();
    }
    if (isdigit(current_char) || (current_char == '.' && isdigit(peekNext())) || current_char == '#') {
        return number();
    }
    if (current_char == '"') {
        return stringLiteral();
    }
    if (current_char == '\'') {
        return charLiteral();
    }
    
    return operatorOrDelimiter();
}

