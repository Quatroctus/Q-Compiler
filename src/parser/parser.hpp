#pragma once

#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <stack>
#include <vector>

#include <src/lexical/lexer.hpp>
#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/name.hpp>
#include <src/parser/ast/operator_type.hpp>
#include <src/parser/ast/type.hpp>
#include <src/parser/ast/qfunction.hpp>
#include <src/parser/ast/qvariable.hpp>

#include <lib/patterns.hpp>

// TODO: Think about how to actually handle errors.
struct ParseError {
    Token tok;
    std::vector<TokenTypes> expected;
};

class Parser {
public:

    // TODO: Structures.
    void parse(Lexer& lexer, std::unordered_map<Name, QVariable>& variables, std::unordered_map<Name, QFunction>& functions);

    // Parses through the global state of a single .q file.
    inline void parseTest(Lexer& lexer) {
        while (!this->expect<SingleType::END>(lexer)) {
            std::cerr << Util::Join(Name::AsStringVec(this->parseNameList(lexer)), " ");
            auto eq = this->expect<SingleType::EQUAL>(lexer);
            if (eq) {
                std::cerr << " = ";
            } else std::cerr << "\nERROR: Expected EQUAL\n";
            auto varDeclType = this->parseVarDeclType(lexer);
            if (!varDeclType) std::cerr << "\nERROR: Expected Variable Type.";
            else {
                for (const auto&[arg, type] : (*varDeclType).first) {
                    std::cerr << arg << " " << type << " ";
                }
                std::cerr << (*varDeclType).second;
            }
            std::vector<Token> expressionTokens;
            auto rawExpressions = this->collectExpressions(lexer, expressionTokens);
            auto expressions = this->parseExpressions(rawExpressions, expressionTokens);
            if (expressions.empty()) std::cerr << " exprStack empty";
            if (!this->expect<SingleType::SEMICOLON>(lexer)) std::cerr << " FUCK\n";
            std::cerr << "\n";
        }
    }
    // void parseGlobal(Lexer&, std::map<std::string, FunctionDef>&, std::map<std::string, VariableDef>&);

private:
    std::vector<ParseError> errors;

    template<typename... TokenTypes, std::enable_if_t<std::tuple_size_v<std::tuple<TokenTypes...>> >= 2, bool> = true>
    std::optional<std::pair<Token, std::variant<TokenTypes...>>> expect(Lexer& lexer) {
        auto tok = lexer.nextToken();
        using namespace mpark::patterns;
        return match(tok.type)(
            pattern(anyof(as<TokenTypes>(arg)...)) = [&tok] (std::variant<TokenTypes...> type) { return std::make_pair(std::move(tok), std::move(type)); },
            pattern(_) = [&tok, &lexer] { lexer.store(std::move(tok)); return std::nullopt; }
        );
    }

    template<typename TokenType>
    std::optional<std::pair<Token, TokenType>> expect(Lexer& lexer) {
        auto tok = lexer.nextToken();
        using namespace mpark::patterns;
        return match(tok.type)(
            pattern(as<TokenType>(arg)) = [&tok] (TokenType& type) -> std::optional<std::pair<Token, TokenType>> { return std::make_pair(tok, type); },
            pattern(_) = [&tok, &lexer] () -> std::optional<std::pair<Token, TokenType>> { lexer.store(std::move(tok)); return std::nullopt; }
        );
    }

    template<SingleType... types>
    std::optional<std::pair<Token, SingleType>> expect(Lexer& lexer) {
        auto tok = lexer.nextToken();
        using namespace mpark::patterns;
        return match(tok.type)(
            pattern(as<SingleType>(arg)) = [&tok] (SingleType sType) {
                WHEN(IF_IS(sType, anyof(types...))) () -> std::optional<std::pair<Token, SingleType>> { return std::make_pair(std::move(tok), sType); }; },
            pattern(_) = [&tok, &lexer] () -> std::optional<std::pair<Token, SingleType>> { lexer.store(std::move(tok)); return std::nullopt; }
        );
    }

    std::vector<Token> collectName(Lexer&);
    std::vector<Token> collectType(Lexer&);
    enum struct LiteralType { IDENTIFIER, INTEGER, FLOATING, STRING, BOOL };
    struct Index;
    struct FNCall;
    struct Constructor;

    using ExpressionPiece = std::pair<size_t, std::variant<LiteralType, OperatorType, Index*, FNCall*, Constructor*>>; // Single part of an expression. 
    using RawExpression = std::vector<ExpressionPiece>; // One whole expression.
    struct ExpressionOrConstructor { // One whole expression or a constructor expression.
        std::variant<
            RawExpression,
            std::vector<ExpressionOrConstructor>
        > value;
        template<typename T>
        ExpressionOrConstructor(T&& t) : value{std::forward<T>(t)} {}
    };
    // using CollectedExpressions = std::pair<std::vector<Token>, std::vector<ExpressionOrConstructor>>; // A series of expressions, constructors or a mix of both.
    using CollectedExpressions = std::vector<ExpressionOrConstructor>;
    struct Index {
        CollectedExpressions indices;
    };
    struct FNCall {
        CollectedExpressions arguments;
    };
    struct Constructor {
        CollectedExpressions arguments;
    };

    CollectedExpressions collectExpressions(Lexer&, std::vector<Token>&);

    template<typename Iter>
    Name parseName(Iter begin, Iter end) {
        using namespace mpark::patterns;
        if (IF_IS(begin[0].type, as<NamespaceType>(_))) {
            return Name{
                std::forward<const std::vector<std::string>>(std::get<NamespaceType>(begin[0].type).nameSpace),
                std::forward<const std::string>(std::get<IdentifierType>(begin[1].type).value)
            };
        } else {
            return Name{
                std::vector<std::string>{},
                std::forward<const std::string>(std::get<IdentifierType>(begin[0].type).value)
            };
        }
    }
    Name parseName(const std::vector<Token>&);
    Name parseName(std::vector<Token>&&);
    template<typename Iter>
    Type parseType(Iter begin, Iter end) {
        using namespace mpark::patterns;
        constexpr auto isLBrace = [](const auto& target) {
            return IF_IS(target.type, as<SingleType>(SingleType::LBRACE));
        };
        auto it = std::find_if(begin, end, isLBrace);
        std::optional<Type> type = MakeType(parseName(begin, end));
        if (it != end) { // Array
            auto iter = it;
            while (iter != end) {
                iter = std::next(iter);
                if (IF_IS((*iter).type, as<SingleType>(SingleType::RBRACE))) {
                    type = MakeArrayType(std::move(*type), std::monostate{});
                    continue;
                } // TODO: Parse Expressions.
                // TODO: Name hard to parse, because we won't know how much to advance `iter`.
                //      Make shift solution would be to std::find_if to the next '['???
                auto name = parseName(iter, end);
                type = MakeArrayType(std::move(*type), name);
                iter = std::find_if(iter, end, isLBrace);
            }
        }
        return *type;
    }
    inline Type parseType(const std::vector<Token>&);
    inline Type parseType(std::vector<Token>&&);

    std::shared_ptr<ExpressionNode> parseExpression(ExpressionOrConstructor&, std::vector<Token>&);
    std::vector<std::shared_ptr<ExpressionNode>> parseExpressions(CollectedExpressions&, std::vector<Token>&);
    //std::shared_ptr<ExpressionNode> parseExpression(RawExpression&&);

    inline std::optional<Name> tryParseName(Lexer& lexer) {
        auto tokens = this->collectName(lexer);
        return tokens.empty() ? std::nullopt : std::make_optional(this->parseName(std::move(tokens)));
    }
    inline std::optional<Type> tryParseType(Lexer& lexer) {
        auto tokens = this->collectType(lexer);
        return tokens.empty() ? std::nullopt : std::make_optional(this->parseType(std::move(tokens)));
    }
    inline std::vector<std::shared_ptr<ExpressionNode>> tryParseExpressions(Lexer& lexer) {
        std::vector<Token> tokens;
        auto raw = this->collectExpressions(lexer, tokens);
        if (raw.empty() || tokens.empty()) return std::vector<std::shared_ptr<ExpressionNode>>{};
        return this->parseExpressions(raw, tokens);
    }
    std::vector<Name> parseNameList(Lexer&);
    using VarDeclType = std::pair<std::vector<std::pair<Name, Type>>, Type>;
    std::optional<VarDeclType> parseVarDeclType(Lexer&);

    template<SingleType... SingleTypes, typename... TokenTypes>
    std::vector<Token> collectUntil(Lexer& lexer) {
        using namespace mpark::patterns;
        std::vector<Token> tokens;
        do {
            tokens.push_back(lexer.nextToken());
        } while (!IF_IS(tokens.back().type, anyof(as<SingleType>(anyof(SingleType::END, SingleTypes...)), as<TokenTypes>(_)...)));
        return tokens;
    }

    // template<size_t arraySize>
    // std::vector<Token> tokenizeUntil(Lexer& lexer, std::array<TokenTypes, arraySize> targets) {
    //     using namespace mpark::patterns;
    //     std::vector<Token> toks;
    //     do {
    //         toks.push_back(lexer.nextToken());
    //     } while (
    //         std::find_if(
    //             std::begin(targets), std::end(targets), [&toks] (TokenTypes type) {
    //                 return toks.back().type == type;
    //             }
    //         ) == std::end(targets) && match(toks.back().type)(
    //             pattern(as<SingleType>(SingleType::END)) = [] { return false; },
    //             pattern(_) = [] { return true; }
    //         )
    //     );
    //     return toks;
    // }

};
