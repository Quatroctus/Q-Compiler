#include <src/lexical/lexer.hpp>

#include <algorithm>
#include <iostream>
#include <optional>

#include <src/util/string.hpp>

#include <lib/patterns.hpp>

Token yylex(SpanLocation&);

Lexer::Lexer(const char* filepath) 
    : Lexer{std::string{filepath}} {}

Lexer::Lexer(const std::string_view filepath)
    : Lexer{std::string{filepath}} {}

Lexer::Lexer(const std::string& filepath) : loc{filepath, 1, 1, 1, 1} {
    this->scan_begin(filepath);
}

Lexer::~Lexer() {
    this->scan_end();
}

Token Lexer::nextToken() {
    if (!this->toks.empty()) {
        Token tok = this->toks.front();
        this->toks.pop_front();
        return tok;
    }
    using namespace mpark::patterns;
    static std::optional<Token> preservedToken{};
    if (preservedToken) {
        Token tok = *preservedToken;
        preservedToken.reset();
        return tok;
    }
    Token tok{};
    std::string error{};
    std::optional<Location> cached{};
    while (match((tok = yylex(this->loc)).type)(
        pattern(as<ErrorType>(_)) = [] { return true; },
        pattern(_) = [] { return false; }
    )) {
        ErrorType& err = std::get<ErrorType>(tok.type);
        if (!cached) cached = tok.location.left;
        error += err.value;
    }
    if (cached) {
        preservedToken = tok;
        return Token{{*cached, this->loc.left}, ErrorType{error}};
    } else {
        return tok;
    }
}

void Lexer::store(Token&& tok) {
    this->toks.push_front(std::move(tok));
}

void Lexer::store(std::vector<Token>& tokens) {
    this->toks.insert(this->toks.begin(), std::make_move_iterator(tokens.begin()), std::make_move_iterator(tokens.end()));
    tokens.clear();
}
