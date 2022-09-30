#pragma once

#include <cstdint>
#include <iostream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include <src/lexical/location.hpp>

enum class SingleType {
    PLUS, MINUS, ASTERISK, BACKSLASH, PERCENT,
    DEQUAL, 
    EQUAL, 
    LPAREN, RPAREN, LBRACE, RBRACE, LCURLY, RCURLY, LARROW, RARROW,
    COMMA, SEMICOLON,

    IF, ELIF, ELSE, OFFER, EXERT, NIX,

    END
};
std::ostream& operator<<(std::ostream&, SingleType type);

struct NamespaceType {
    std::vector<std::string> nameSpace;
};
std::ostream& operator<<(std::ostream&, const NamespaceType&);

struct IdentifierType {
    std::string value;
};
std::ostream& operator<<(std::ostream&, const IdentifierType&);

struct ErrorType {
    std::string value;
};
std::ostream& operator<<(std::ostream&, const ErrorType&);

struct IntegerType {
    uint64_t value;
};
std::ostream& operator<<(std::ostream&, const IntegerType&);

struct FloatingType {
    bool large;
    double value;
};
std::ostream& operator<<(std::ostream&, const FloatingType&);

struct BooleanType {
    bool value;
};
std::ostream& operator<<(std::ostream&, const BooleanType&);

struct StringLiteralType {
    std::string literal;
    std::string translated;
};
std::ostream& operator<<(std::ostream&, const StringLiteralType&);

using TokenType = std::variant<
    SingleType, NamespaceType, IdentifierType, IntegerType, 
    FloatingType, BooleanType, StringLiteralType,
    ErrorType
>;

enum class TokenTypes {
    OPERATOR, EQUAL, LPAREN, RPAREN, LBRACE, RBRACE,
    LCURLY, RCURLY, SEMICOLON, END,
    IDENTIFIER, INTEGER, ERROR, KEYWORD
};

bool operator==(const TokenType&, TokenTypes);
inline bool operator==(TokenTypes types, const TokenType& type) { return type == types; }

struct Token {
    // TODO: Other info all tokens should have like location.
    SpanLocation location;
    TokenType type;

    friend std::ostream& operator<<(std::ostream&, const Token&);
};
