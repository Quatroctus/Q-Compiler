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
    using namespace mpark::patterns;
    static std::optional<Token> preservedToken{};

    if (this->storage) {
        auto storage = *this->storage;
        if (storage.size() == 1) return storage.back();
        auto tok = storage.back();
        storage.pop_back();
        return std::move(tok);
    } else {
        if (!this->toks.empty()) {
            auto tok = this->toks.front();
            this->toks.pop_front();
            return tok;
        }
        if (preservedToken) {
            auto tok = *preservedToken;
            preservedToken.reset();
            return tok;
        }
        Token tok{};
        std::string error{};
        std::optional<Location> cached{};
        while (match((tok = yylex(this->loc)).type)(
            pattern(as<ErrorType>(arg)) = [&](ErrorType& err) {
                if (!cached) cached = tok.location.left;
                error += err.value;
                return true;
            },
            pattern(_) = [] { return false; }
        ));
        if (cached) {
            preservedToken = tok;
            return Token{{*cached, this->loc.left}, ErrorType{error}};
        }
        return tok;
    }
}

void Lexer::store(Token&& tok) {
    if (this->storage) {
        auto storage = *this->storage;
        storage.push_back(std::move(tok));
    } else {
        this->toks.push_front(std::move(tok));
    }
}

void Lexer::store(std::vector<Token>& tokens) {
    if (this->storage) {
        auto storage = *this->storage;
        storage.insert(storage.end(), std::make_move_iterator(tokens.rbegin()), std::make_move_iterator(tokens.rend()));
    } else {
        this->toks.insert(this->toks.begin(), std::make_move_iterator(tokens.begin()), std::make_move_iterator(tokens.end()));
    }
    tokens.clear();
}
