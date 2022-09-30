#ifdef __linux__
#define FMT_HEADER_ONLY
#include <fmt/format.h>
namespace std {
    using fmt::v9::format;
};
#else
#include <format>
#endif
#include <stdexcept>

#include <src/lexical/token.hpp>
#include <src/util/string.hpp>

#include <lib/patterns.hpp>

std::ostream& operator<<(std::ostream& out, SingleType type) {
    switch (type) {
        case SingleType::PLUS: return out << "PLUS";
        case SingleType::MINUS: return out << "MINUS";
        case SingleType::ASTERISK: return out << "ASTERISK";
        case SingleType::BACKSLASH: return out << "BACKSLASH";
        case SingleType::PERCENT: return out << "PERCENT";
        case SingleType::EQUAL: return out << "EQUAL";
        case SingleType::LPAREN: return out << "LPAREN";
        case SingleType::RPAREN: return out << "RPAREN";
        case SingleType::LCURLY: return out << "LCURLY";
        case SingleType::RCURLY: return out << "RCURLY";
        case SingleType::LBRACE: return out << "LBRACE";
        case SingleType::RBRACE: return out << "RBRACE";
        case SingleType::SEMICOLON: return out << "SEMICOLON";
        case SingleType::OFFER: return out << "OFFER";
        case SingleType::EXERT: return out << "EXERT";
        case SingleType::NIX: return out << "NIX";
        case SingleType::END: return out << "EOF";
    }
    throw std::runtime_error{std::format("Unknown SingleType {}", static_cast<uint32_t>(type))};
}

std::ostream& operator<<(std::ostream& out, const NamespaceType& type) {
    return out << "NAMESPACE{ value: \"" << Util::Join(type.nameSpace, "::") << "\" }";
}

std::ostream& operator<<(std::ostream& out, const IdentifierType& type) {
    return out << "IDENTIFIER{ value: \"" << type.value.data() << "\" }";
}

std::ostream& operator<<(std::ostream& out, const ErrorType& type) {
    return out << "ERROR{ value: \"" << type.value.data() << "\" }";
}

std::ostream& operator<<(std::ostream& out, const IntegerType& type) {
    return out << "INTEGER{ value: " << type.value << " }";
}

std::ostream& operator<<(std::ostream& out, const FloatingType& type) {
    //return out << std::format("FLOATING{ large: {}, value: {} }", (type.large ? "true" : "false"), type.value);
    return out;
}

std::ostream& operator<<(std::ostream& out, const BooleanType& type) {
    return out << "BOOLEAN{ value: " << (type.value ? "true" : "false") << " }";
}

std::ostream& operator<<(std::ostream& out, const StringLiteralType& sl) {
    return out << "StringLiteral{ value: \"" << sl.literal << "\" }";
}

bool operator==(const TokenType& type, TokenTypes types) {
    using namespace mpark::patterns;
    return match(type)(
        pattern(as<SingleType>(arg)) = [types] (SingleType type) {
            return match(type, types)(
                pattern(anyof(SingleType::PLUS, SingleType::MINUS, SingleType::ASTERISK, SingleType::BACKSLASH, SingleType::PERCENT), TokenTypes::OPERATOR) = [] { return true; },
                pattern(SingleType::EQUAL, TokenTypes::EQUAL) = [] { return true; },
                pattern(SingleType::LPAREN, TokenTypes::LPAREN) = [] { return true; },
                pattern(SingleType::RPAREN, TokenTypes::RPAREN) = [] { return true; },
                pattern(SingleType::LBRACE, TokenTypes::LBRACE) = [] { return true; },
                pattern(SingleType::RBRACE, TokenTypes::RBRACE) = [] { return true; },
                pattern(SingleType::LCURLY, TokenTypes::LCURLY) = [] { return true; },
                pattern(SingleType::RCURLY, TokenTypes::RCURLY) = [] { return true; },
                pattern(SingleType::SEMICOLON, TokenTypes::SEMICOLON) = [] { return true; },
                pattern(SingleType::END, TokenTypes::END) = [] { return true; },
                pattern(_, _) = [] { return false; }
            );
            //return types == TokenTypes::OPERATOR;
        },
        pattern(as<IdentifierType>(_)) = [types] { return types == TokenTypes::IDENTIFIER; },
        pattern(as<IntegerType>(_)) = [types] { return types == TokenTypes::INTEGER; },
        pattern(as<ErrorType>(_)) = [types] { return types == TokenTypes::ERROR; },
        pattern(_) = [] { return false; }
    );
}

std::ostream& operator<<(std::ostream& out, const Token& tok) {
    using namespace mpark::patterns;
    out << tok.location << "\n    ";
    match(tok.type)(
        pattern(as<SingleType>(arg)) = [&out] (auto type) { out << type; },
        pattern(as<NamespaceType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<IdentifierType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<IntegerType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<FloatingType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<BooleanType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<StringLiteralType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(as<ErrorType>(arg)) = [&out] (const auto& type) { out << type; },
        pattern(_) = [&out] { out << "Unknown"; }
    );
    return out;
}