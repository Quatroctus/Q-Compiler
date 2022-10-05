#pragma once

#include <algorithm>
#include <deque>
#include <optional>
#include <string>
#include <string_view>

#include <src/lexical/location.hpp>
#include <src/lexical/token.hpp>

class Lexer {
public:
    Lexer(const char* filepath);
    Lexer(const std::string_view filepath);
    Lexer(const std::string& filepath);
    ~Lexer();

    Token nextToken();
    void store(Token&&);
    void store(std::vector<Token>&);

    inline void primeStorage(std::vector<Token>&& storage) {
        this->storage = std::move(storage);
        (*this->storage).push_back(Token{storage.back().location, SingleType::END});
        std::reverse((*this->storage).begin(), (*this->storage).end());
    }
    inline void wipeStorage() { this->storage.reset(); }

    inline static Token MakeInteger(const std::string& integer, uint32_t base, const SpanLocation& loc) {
        return Token{loc, IntegerType{std::stoull(integer, nullptr, base)}};
    }
    
    inline static Token MakeFloating(const std::string& floating, bool large, const SpanLocation& loc) {
        double d = std::stod(floating);
        if (!large) {
            float f = std::stof(floating);
            if (static_cast<double>(f) != d) {
                // TODO: Warning Maybe error.
            }
        }
        return Token{loc, FloatingType{large, d}};
    }

    inline static Token MakeBoolean(bool value, const SpanLocation& loc) {
        return Token{loc, BooleanType{value}};
    }

    inline static Token MakeStringLiteral(std::string&& sl, const SpanLocation& loc) {
        std::string translated{sl};
        // TODO: Parse escape codes \n \r etc.
        return Token{loc, StringLiteralType{sl, translated}};
    }
    

    inline static Token MakeIF(const SpanLocation& loc) {
        return Token{loc, SingleType::IF};
    }

    inline static Token MakeELIF(const SpanLocation& loc) {
        return Token{loc, SingleType::ELIF};
    }

    inline static Token MakeELSE(const SpanLocation& loc) {
        return Token{loc, SingleType::ELSE};
    }

    inline static Token MakeOFFER(const SpanLocation& loc) {
        return Token{loc, SingleType::OFFER};
    }

    inline static Token MakeEXERT(const SpanLocation& loc) {
        return Token{loc, SingleType::EXERT};
    }
    
    inline static Token MakeNIX(const SpanLocation& loc) {
        return Token{loc, SingleType::NIX};
    }


    inline static Token MakeDEQUAL(const SpanLocation& loc) {
        return Token{loc, SingleType::DEQUAL};
    }
    
    inline static Token MakeEQUAL(const SpanLocation& loc) {
        return Token{loc, SingleType::EQUAL};
    }
    
    inline static Token MakePLUS(const SpanLocation& loc) {
        return Token{loc, SingleType::PLUS};
    }
    
    inline static Token MakeMINUS(const SpanLocation& loc) {
        return Token{loc, SingleType::MINUS};
    }
    
    inline static Token MakeASTERISK(const SpanLocation& loc) {
        return Token{loc, SingleType::ASTERISK};
    }
    
    inline static Token MakeBACKSLASH(const SpanLocation& loc) {
        return Token{loc, SingleType::BACKSLASH};
    }
    
    inline static Token MakePERCENT(const SpanLocation& loc) {
        return Token{loc, SingleType::PERCENT};
    }
    

    inline static Token MakeLPAREN(const SpanLocation& loc) {
        return Token{loc, SingleType::LPAREN};
    }
    
    inline static Token MakeRPAREN(const SpanLocation& loc) {
        return Token{loc, SingleType::RPAREN};
    }
    
    inline static Token MakeLCURLY(const SpanLocation& loc) {
        return Token{loc, SingleType::LCURLY};
    }
    
    inline static Token MakeRCURLY(const SpanLocation& loc) {
        return Token{loc, SingleType::RCURLY};
    }
    
    inline static Token MakeLBRACE(const SpanLocation& loc) {
        return Token{loc, SingleType::LBRACE};
    }
    
    inline static Token MakeRBRACE(const SpanLocation& loc) {
        return Token{loc, SingleType::RBRACE};
    }

    inline static Token MakeCOMMA(const SpanLocation& loc) {
        return Token{loc, SingleType::COMMA};
    }

    inline static Token MakeSEMICOLON(const SpanLocation& loc) {
        return Token{loc, SingleType::SEMICOLON};
    }

    inline static Token MakeNamespace(std::vector<std::string>&& nameSpace, const SpanLocation& loc) {
        return Token{loc, NamespaceType{std::move(nameSpace)}};
    }

    inline static Token MakeIdentifier(std::string&& identifier, const SpanLocation& loc) {
        return Token{loc, IdentifierType{std::move(identifier)}};
    }


    inline static Token MakeEND(const SpanLocation& loc) {
        return Token{loc, SingleType::END};
    }

    inline static Token MakeError(std::string&& err, const SpanLocation& loc) {
        return Token{loc, ErrorType{std::move(err)}};
    }

private:
    SpanLocation loc;
    std::optional<std::vector<Token>> storage{};
    std::deque<Token> toks;

    void scan_begin(const std::string&);
    void scan_end();

};
