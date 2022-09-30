#include <src/parser/parser.hpp>

#include <algorithm>
#include <cassert>

#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/binary_operator_node.hpp>
#include <src/parser/ast/boolean_node.hpp>
#include <src/parser/ast/constructor_node.hpp>
#include <src/parser/ast/identifier_node.hpp>
#include <src/parser/ast/index_node.hpp>
#include <src/parser/ast/integer_node.hpp>
#include <src/parser/ast/floating_node.hpp>
#include <src/parser/ast/fncall_node.hpp>
#include <src/parser/ast/string_node.hpp>
#include <src/parser/ast/unary_operator_node.hpp>

#include <lib/patterns.hpp>

#define AS(type, value) static_cast<type>(value)

// void Parser::parseGlobal(Lexer& lexer, std::map<std::string, FunctionDef>& globalFunctions, std::map<std::string, VariableDef>& globalVariables) {

// }

void Parser::parse(Lexer& lexer, std::unordered_map<Name, QVariable>& variables, std::unordered_map<Name, QFunction>& functions) {
    using namespace mpark::patterns;
    while (!this->expect<SingleType::END>(lexer)) {
        auto names = this->parseNameList(lexer);
        if (names.empty()) {
            // ERROR: Expected at least one name.
        }
        auto eq = this->expect<SingleType::EQUAL>(lexer);
        if (!eq) {
            // ERROR: Expected an EQUAL.
        }
        auto typing = this->parseVarDeclType(lexer);
        if (!typing) {
            // ERROR: Expected parameter list and return type or variable type.
        }
        if ((*typing).first.empty()) { // Variable
            std::cerr << "Parser::parse Variable\n";
            // TODO: Handle the two types of expression equivalents.
            //    expression, expression, ..., expression;
            //    {expression, expression, ..., expression}, ..., {expression, expression, ..., expression};
            std::vector<Token> exprTokens;
            auto rawExpressions = this->collectExpressions(lexer, exprTokens);
            bool allConstructors = std::accumulate(rawExpressions.cbegin(), rawExpressions.cend(), true, [](bool b, const ExpressionOrConstructor& e){return b && e.value.index() == 1;});
            auto expressions = this->parseExpressions(rawExpressions, exprTokens);
            // TODO: Change this to be all convertible to typing
            bool allMatchTyping = std::accumulate(expressions.begin(), expressions.end(), true, [&](bool b, std::shared_ptr<ExpressionNode> e){return b && e->getType() == (*typing).second;});
            if (!this->expect<SingleType::SEMICOLON>(lexer)) {
                // ERROR: Expected Semicolon got ...
                std::cerr << "Expected Semicolon got " << lexer.nextToken();
            }
            if (
                names.size() == expressions.size() &&
                (allConstructors || allMatchTyping)
            ) {
                auto nameIt = names.begin();
                auto exprIt = expressions.begin();
                for (; nameIt != names.end(); nameIt++, exprIt++) {
                    if (std::dynamic_pointer_cast<ConstructorNode>(*exprIt) || expressions.size() == 1) {
                        variables.emplace(*nameIt, QVariable{*nameIt, (*typing).second, *exprIt});
                    } else {
                        variables.emplace(*nameIt, QVariable{*nameIt, (*typing).second, std::make_shared<ConstructorNode>((*typing).second, std::vector{*exprIt})});
                    }
                }
            } else {
                std::shared_ptr<ExpressionNode> value;
                if (expressions.size() > 1 || (rawExpressions[0].value.index() == 0 && expressions[0]->getType() != (*typing).second)) {
                    value = std::make_shared<ConstructorNode>((*typing).second, std::move(expressions));
                } else {
                    value = expressions[0];
                }
                for (const auto& name : names) {
                    if (variables.contains(name)) {
                        std::cerr << "Name Collision on " << name << "\n";
                        assert(false);
                    }
                    variables.emplace(name, QVariable{name, (*typing).second, value});
                }
            }
        } else { // Function
            std::cerr << "Parser::parse Function\n";
            if (names.size() > 1) {
                // WARNING: Extra names in function assignment.
            }
            // TODO: Handle the two types of function expression equivalents.
            //    expression, expression, ..., expression;
            //    {
            //         name_list = type expression|expressioncsl|braced_expressioncsl;
            //         expression;
            //         offer expression;
            //         exert expression;
            //    }
            // TODO: Expand ArrayType parameters.
            std::vector<Token> tokens;
            auto rawExpressions = this->collectExpressions(lexer, tokens);
            if (rawExpressions.empty() || tokens.empty()) {
                // Try to parse a code block.

            } else {
                if (!this->expect<SingleType::SEMICOLON>(lexer)) {
                    // ERROR: Expected Semicolon but got ...
                }
                auto expressions = this->parseExpressions(rawExpressions, tokens);
                std::shared_ptr<ExpressionNode> value;
                auto exprtype = expressions[0]->getType();
                if (expressions.size() > 1 || (rawExpressions[0].value.index() == 0 && !(*typing).second.isConvertible(exprtype))) {
                    value = std::make_shared<ConstructorNode>((*typing).second, std::move(expressions));
                } else {
                    value = expressions[0];
                }
                functions.emplace(names[0], QFunction{names[0], std::move((*typing).first), std::move((*typing).second), value});
            }
        }
    }
}

std::vector<Token> Parser::collectName(Lexer& lexer) {
    std::vector<Token> tokens{};
    auto nsOpt = this->expect<NamespaceType>(lexer);
    auto nOpt = this->expect<IdentifierType>(lexer);
    if (nsOpt) tokens.push_back(std::move((*nsOpt).first));
    if (nOpt) tokens.push_back(std::move((*nOpt).first));
    return tokens;
}

std::vector<Token> Parser::collectType(Lexer& lexer) {
    auto tokens = this->collectName(lexer);
    if (tokens.empty()) {
        auto nix = this->expect<SingleType::NIX>(lexer);
        if (nix) return {(*nix).first};
        return {};
    }
    auto lbOpt = this->expect<SingleType::LBRACE>(lexer);
    if (lbOpt) {
        tokens.push_back(std::move((*lbOpt).first));

        // TODO: Collect tokens for an expression.
        // TODO: Rewrite to support empty braces as array type.
        // this->collectExpresion(lexer);
        auto lengthName = this->collectName(lexer);
        if (lengthName.empty()) {
            lexer.store(tokens);
            return {};
        }
        tokens.insert(tokens.end(), std::make_move_iterator(lengthName.begin()), std::make_move_iterator(lengthName.end()));

        auto rbOpt = this->expect<SingleType::RBRACE>(lexer);
        if (!rbOpt) {
            lexer.store(tokens);
            return {};
        }
        tokens.push_back(std::move((*rbOpt).first));
    }

    return tokens;
}

uint32_t Precedence(OperatorType type) {
    switch (type) {
        case OperatorType::ADD: case OperatorType::SUB:
            return 2;
        case OperatorType::MUL: case OperatorType::DIV: case OperatorType::MOD:
            return 3;
        case OperatorType::NEG:
            return 12;
        case OperatorType::LPAREN: case OperatorType::RPAREN:
            return 1;
    }
    return 0;
}

#define OPERATORS SingleType::PLUS, SingleType::MINUS, SingleType::ASTERISK, SingleType::BACKSLASH, SingleType::PERCENT, \
                  SingleType::LPAREN, SingleType::RPAREN
Parser::CollectedExpressions Parser::collectExpressions(Lexer& lexer, std::vector<Token>& tokens) {
    using namespace mpark::patterns;
    CollectedExpressions csExpressions;
    do {
        auto lcurly = this->expect<SingleType::LCURLY>(lexer);
        if (lcurly) {
            // constructor.
            tokens.push_back(std::move((*lcurly).first));
            auto expressions = this->collectExpressions(lexer, tokens);
            auto rcurly = this->expect<SingleType::RCURLY>(lexer);
            if (!rcurly) {
                lexer.store(tokens);
                return csExpressions;
            }
            tokens.push_back(std::move((*rcurly).first));
            csExpressions.push_back(ExpressionOrConstructor{std::move(expressions)});
        } else {
            // parse an expression.
            bool lastWasOp = true;
            RawExpression expression{};
            std::vector<std::pair<size_t, OperatorType>> storage;
            while (true) {
                OperatorType opType{OperatorType::LPAREN};
                auto op = this->expect<OPERATORS>(lexer);
                if (op) {
                    switch ((*op).second) {
                        case SingleType::LPAREN:
                            storage.emplace_back(tokens.size(), OperatorType::LPAREN);
                            tokens.push_back(std::move((*op).first));
                            break;
                        case SingleType::RPAREN:
                            if (storage.empty()) {
                                // ERROR: Unmatched parenthesis.
                            }
                            if (storage.back().second == OperatorType::LPAREN) {
                                // ERROR: Empty parenthesis expression.
                            }
                            while (storage.back().second != OperatorType::LPAREN) {
                                expression.push_back(std::move(storage.back()));
                                storage.pop_back();
                            }
                            storage.pop_back();
                            tokens.push_back(std::move((*op).first));
                            break;
                        case SingleType::MINUS:
                            opType = OperatorType::NEG;
                            goto handleOperator;
                        default:
                            if (lastWasOp) {
                                // ERROR: Invalid unary operator.
                            }
                            opType = AS(OperatorType, (*op).second);
                        handleOperator:
                            if (storage.empty() || Precedence(opType) > Precedence(storage.back().second)) {
                                storage.emplace_back(tokens.size(), opType);
                                tokens.push_back(std::move((*op).first));
                            } else {
                                while (Precedence(opType) <= Precedence(storage.back().second)) {
                                    expression.push_back(std::move(storage.back()));
                                    storage.pop_back();
                                }
                                expression.push_back({tokens.size(), opType});
                                tokens.push_back((*op).first);
                            }
                            break;
                    }
                    lastWasOp = true;
                } else {
                    if (!lastWasOp) {
                        while (!storage.empty()) {
                            if (storage.back().second == OperatorType::LPAREN) {
                                // ERROR: Unmatched Parenthesis.
                            }
                            expression.push_back(std::move(storage.back()));
                            storage.pop_back();
                        }
                        if (!csExpressions.empty()) assert(false);
                        csExpressions.push_back(ExpressionOrConstructor{std::move(expression)});
                        break;
                    }
                    auto literal = lexer.nextToken();
                    bool matched = match(literal.type)(
                        pattern(as<IntegerType>(_)) = [&] {
                            expression.emplace_back(tokens.size(), LiteralType::INTEGER);
                            tokens.push_back(std::move(literal));
                            return true;
                        },
                        pattern(as<FloatingType>(_)) = [&] {
                            expression.emplace_back(tokens.size(), LiteralType::FLOATING);
                            tokens.push_back(std::move(literal));
                            return true;
                        },
                        pattern(as<BooleanType>(_)) = [&] {
                            expression.emplace_back(tokens.size(), LiteralType::BOOL);
                            tokens.push_back(std::move(literal));
                            return true;
                        },
                        pattern(as<StringLiteralType>(_)) = [&] {
                            expression.emplace_back(tokens.size(), LiteralType::STRING);
                            tokens.push_back(std::move(literal));
                            return true;
                        },
                        pattern(_) = [&] { lexer.store(std::move(literal)); return false; }
                    );
                    if (!matched) {
                        auto name = this->collectName(lexer);
                        if (!name.empty()) {
                            expression.emplace_back(tokens.size(), LiteralType::IDENTIFIER);
                            tokens.insert(tokens.end(), std::make_move_iterator(name.begin()), std::make_move_iterator(name.end()));
                            
                            bool loop = true;
                            while (loop) {
                                size_t stored = tokens.size();
                                auto token = lexer.nextToken();
                                bool noConstructor = false;
                                match(token.type)(
                                    pattern(as<SingleType>(SingleType::LBRACE)) = [&] {
                                        tokens.push_back(std::move(token));
                                        auto expressions = this->collectExpressions(lexer, tokens);
                                        auto rbrace = this->expect<SingleType::RBRACE>(lexer);
                                        if (!rbrace) {
                                            // ERROR: Unmatched Square Braces.
                                        }
                                        tokens.push_back(std::move((*rbrace).first));
                                        expression.emplace_back(stored, new Index{std::move(expressions)});
                                        noConstructor = true;
                                    },
                                    pattern(as<SingleType>(SingleType::LPAREN)) = [&] {
                                        tokens.push_back(std::move(token));
                                        auto expressions = this->collectExpressions(lexer, tokens);
                                        auto rparen = this->expect<SingleType::RPAREN>(lexer);
                                        if (!rparen){
                                            // ERROR: Unmatched Parenthesis.
                                        }
                                        tokens.push_back(std::move((*rparen).first));
                                        expression.emplace_back(stored, new FNCall{std::move(expressions)});
                                        noConstructor = true;
                                    },
                                    pattern(as<SingleType>(SingleType::LCURLY)) = [&] {
                                        if (!noConstructor) {
                                            // ERROR: Cannot invoke constructor on function return result or index result.
                                            lexer.store(std::move(token));
                                            loop = false;
                                            return;
                                        }
                                        tokens.push_back(std::move(token));
                                        auto expressions = this->collectExpressions(lexer, tokens);
                                        auto rcurly = this->expect<SingleType::RCURLY>(lexer);
                                        if (!rcurly) {
                                            // ERROR: Unmatched Curly Braces.
                                        }
                                        tokens.push_back(std::move((*rcurly).first));
                                        expression.emplace_back(stored, new Constructor{std::move(expressions)});
                                        //loop = false; // TODO: This is not correct. We should be able to invoke or index a constructed type. We shouldn't be able to construct from a function call or index though.
                                    },
                                    pattern(_) = [&] {
                                        lexer.store(std::move(token));
                                        loop = false;
                                    }
                                );
                            }
                        } else {
                            // BAIL and release tokens.
                            lexer.store(tokens);
                            return CollectedExpressions{};
                        }
                    }
                    lastWasOp = false;
                }
            }
        }
    } while ([&]() -> bool {
        auto comma = this->expect<SingleType::COMMA>(lexer);
        if (comma) {
            tokens.push_back(std::move((*comma).first));
            return true;
        }
        return false;
    }());

    return csExpressions;
}

Name Parser::parseName(const std::vector<Token>& tokens) {
    return this->parseName(tokens.begin(), tokens.end());
}

Name Parser::parseName(std::vector<Token>&& tokens) {
    return this->parseName(
        std::make_move_iterator(tokens.begin()), std::make_move_iterator(tokens.end())
    );
}

Type Parser::parseType(const std::vector<Token>& tokens) {
    return this->parseType(tokens.begin(), tokens.end());
    // using namespace mpark::patterns;
    // if (IF_IS(tokens.back().type, as<SingleType>(SingleType::RBRACE))) {
    //     auto it = std::find_if(
    //         tokens.begin(), tokens.end(), [] (auto&& target) {
    //             return IF_IS(target.type, as<SingleType>(SingleType::LBRACE));
    //         }
    //     );
    //     return MakeArrayType(
    //         parseName(std::vector<Token>{tokens.begin(), it}),  
    //         parseName(std::vector<Token>{std::next(it), tokens.end() - 1})
    //     );
    // }
    // return MakeType(parseName(tokens));
}

Type Parser::parseType(std::vector<Token>&& tokens) {
    auto result = this->parseType(
        std::make_move_iterator(tokens.begin()),
        std::make_move_iterator(tokens.end())
    );
    tokens.clear();
    return result;
    // using namespace mpark::patterns;
    // if (IF_IS(tokens.back().type, as<SingleType>(SingleType::RBRACE))) {
    //     auto it = std::make_move_iterator(std::find_if(
    //         tokens.begin(), tokens.end(), [] (auto&& target) {
    //             return IF_IS(target.type, as<SingleType>(SingleType::LBRACE));
    //         }
    //     ));
    //     return MakeArrayType(
    //         parseName(std::vector<Token>{std::make_move_iterator(tokens.begin()), it}),  
    //         parseName(std::vector<Token>{std::next(it), std::make_move_iterator(tokens.end() - 1)})
    //     );
    // }
    // return MakeType(parseName(std::move(tokens)));
}

std::shared_ptr<ExpressionNode> Parser::parseExpression(ExpressionOrConstructor& expression, std::vector<Token>& tokens) {
    using namespace mpark::patterns;
    if (expression.value.index() == 0) {
        RawExpression& expr = std::get<RawExpression>(expression.value);
        if (expr.empty()) {
            std::cerr << "RawExpression is Empty\n";
            return nullptr;
        }
        std::vector<std::shared_ptr<ExpressionNode>> exprStack;
        auto it = expr.cbegin();
        for (; it != expr.cend(); it++) {
            auto[idx, piece] = *it;
            match(piece)(
                pattern(as<LiteralType>(arg)) = [&](LiteralType type) {
                    switch (type) {
                        case LiteralType::BOOL:
                            exprStack.push_back(std::make_shared<BooleanNode>(std::get<BooleanType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::INTEGER:
                            std::cerr << "Token: " << tokens.at(idx) << "\n";
                            exprStack.push_back(std::make_shared<IntegerNode>(std::get<IntegerType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::FLOATING:
                            exprStack.push_back(std::make_shared<FloatingNode>(std::get<FloatingType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::STRING:
                            exprStack.push_back(std::make_shared<StringNode>(std::get<StringLiteralType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::IDENTIFIER:
                            // TODO: Check forward in the expr.
                            exprStack.push_back(std::make_shared<IdentifierNode>(this->parseName(tokens.begin() + idx, tokens.end())));
                            while (
                                (it + 1) != expr.cend()
                            ) {
                                if (match((*(it + 1)).second)(
                                    pattern(as<Index*>(arg)) = [&](Index* index) {
                                        std::shared_ptr<ExpressionNode> target = exprStack.back();
                                        exprStack.pop_back();
                                        exprStack.push_back(std::make_shared<IndexNode>(target, this->parseExpressions(index->indices, tokens)));
                                        return false;
                                    },
                                    pattern(as<FNCall*>(arg)) = [&](FNCall* call) {
                                        std::shared_ptr<ExpressionNode> target = exprStack.back();
                                        exprStack.pop_back();
                                        exprStack.push_back(std::make_shared<FNCallNode>(target, this->parseExpressions(call->arguments, tokens)));
                                        return false;
                                    },
                                    //pattern(as<Constructor*>(arg)) = [&](Constructor* cs) { // TODO: ConstructorNode takes a type. 
                                    //    std::shared_ptr<ExpressionNode> target = exprStack.back();
                                    //    exprStack.pop_back();
                                    //    exprStack.push_back(std::make_shared<ConstructorNode>(target, this->parseExpressions(cs->arguments, tokens)));
                                    //},
                                    pattern(_) = [] { return true; }
                                )) break;
                                it++;
                            }
                            break;
                    }
                },
                pattern(as<OperatorType>(arg)) = [&](OperatorType type) {
                    switch (type) {
                        case OperatorType::NEG:
                        {
                            std::shared_ptr<ExpressionNode> expr = exprStack.back();
                            exprStack.pop_back();
                            exprStack.push_back(std::make_shared<UnaryOperatorNode>(expr, type));
                            break;
                        }
                        case OperatorType::ADD: case OperatorType::SUB: case OperatorType::MUL: case OperatorType::DIV:
                        case OperatorType::MOD: 
                        {
                            std::shared_ptr<ExpressionNode> right = exprStack.back();
                            exprStack.pop_back();
                            std::shared_ptr<ExpressionNode> left = exprStack.back();
                            exprStack.pop_back();
                            exprStack.push_back(std::make_shared<BinaryOperatorNode>(left, right, type));
                            break;
                        }
                        default:
                            std::cerr << "ERROR: Unhandled OperatorType\n";
                            assert(false);
                            break;
                    }
                },
                pattern(_) = [&] {
                    // ERROR: Maybe
                }
            );
        }
        if (exprStack.empty()) { // Maybe return nullptr instead of an error.
            // ERROR: ExprStack is empty.
            std::cerr << "Expression Stack is Emtpy\n";
            return nullptr;
        }
        if (exprStack.size() > 1) {
            // ERROR: ExprStack contains more than 1 expressions.
        }
        return exprStack.back();
    } else {
        auto exprOrCons = std::get<std::vector<ExpressionOrConstructor>>(expression.value);
        std::vector<std::shared_ptr<ExpressionNode>> expressions;
        expressions.reserve(exprOrCons.size());
        for (auto& exprOrCon : exprOrCons) {
            this->parseExpression(exprOrCon, tokens);
        }
        return std::make_shared<ConstructorNode>(std::move(expressions));              
    }
}

std::vector<std::shared_ptr<ExpressionNode>> Parser::parseExpressions(CollectedExpressions& csExpressions, std::vector<Token>& tokens) {
    std::vector<std::shared_ptr<ExpressionNode>> expressions;
    expressions.reserve(csExpressions.size());
    for (ExpressionOrConstructor& collected : csExpressions) {
        expressions.push_back(this->parseExpression(collected, tokens));
    }
    // std::stack<std::shared_ptr<ExpressionNode>> exprStack;
    // for (const auto&[idx, piece] : raw.second) {
    //     match(piece)(
    //         pattern(as<LiteralType>(arg)) = [&] (LiteralType type) {
    //             switch (type) {
    //                 case LiteralType::IDENTIFIER:
    //                     exprStack.push(new IdentifierNode{this->parseName(raw.first.begin() + idx, raw.first.end())});
    //                     break;
    //                 case LiteralType::INTEGER:
    //                     exprStack.push(new IntegerNode{std::get<IntegerType>(raw.first[idx].type)});
    //                     break;
    //                 case LiteralType::FLOATING:
    //                     exprStack.push(new FloatingNode{std::get<FloatingType>(raw.first[idx].type)});
    //                     break;
    //                 case LiteralType::BOOL:
    //                     exprStack.push(new BooleanNode{std::get<BooleanType>(raw.first[idx].type)});
    //                     break;
    //                 case LiteralType::STRING:
    //                     exprStack.push(new StringNode{std::get<StringLiteralType>(raw.first[idx].type)});
    //                     break;
    //                 case LiteralType::FNCALL:
    //                     // TODO: This isn't even supported in the grammar yet.
    //                     break;
    //             }
    //         },
    //         pattern(as<OperatorType>(arg)) = [&] (OperatorType type) {
    //             switch (type) { // TODO: Error check for amount in the exprStack.
    //                 case OperatorType::NEG: {
    //                     std::shared_ptr<ExpressionNode> expr = exprStack.top();
    //                     exprStack.pop();
    //                     exprStack.push(new UnaryOperatorNode{expr, type});
    //                 } break;
    //                 case OperatorType::ADD: case OperatorType::SUB: case OperatorType::MUL:
    //                 case OperatorType::DIV: case OperatorType::MOD: {
    //                     std::shared_ptr<ExpressionNode> left = exprStack.top();
    //                     exprStack.pop();
    //                     std::shared_ptr<ExpressionNode> right = exprStack.top();
    //                     exprStack.pop();
    //                     exprStack.push(new BinaryOperatorNode{left, right, type});
    //                 } break;
    //             }
    //         },
    //         pattern(as<Index*>(arg)) = [&] (Index* index) {
    //             // TODO: PAIN.
    //         }
    //     );
    // }
    // if (exprStack.empty()) return nullptr;
    // if (exprStack.size() > 1) {
    //     // IDK Error or something?
    // }
    // exprStack.top();

    return expressions;
}

std::vector<Name> Parser::parseNameList(Lexer& lexer) {
    std::vector<Name> names{};
    while (true) {
        auto name = this->tryParseName(lexer);
        if (name) names.push_back(std::move(*name));
        else break;
    }
    if (names.empty()) {
        // TODO: Error. NameList is not allowed to be empty.
    }
    return names;
}

std::optional<Parser::VarDeclType> Parser::parseVarDeclType(Lexer& lexer) {
    // TODO: Make odd item count.
    auto idOpt = this->expect<IdentifierType>(lexer);
    if (!idOpt) {
        auto type = this->tryParseType(lexer);
        if (type) return std::make_pair(std::vector<std::pair<Name, Type>>{}, *type);
        return std::nullopt;
    }

    std::vector<std::pair<Name, Type>> params{};
    while (true) {
        auto type = this->collectType(lexer);
        if (type.empty()) {
            lexer.store(std::move((*idOpt).first));
            auto type = this->tryParseType(lexer);
            if (type) return std::make_pair(std::move(params), std::move(*type));
            return std::nullopt;
        }
        
        auto nextIdOpt = this->expect<IdentifierType>(lexer);
        if (!nextIdOpt) {
            lexer.store(type);
            lexer.store(std::move((*idOpt).first));
            auto type = this->tryParseType(lexer);
            if (type) return std::make_pair(std::move(params), std::move(*type));
            return std::nullopt;
        }

        params.emplace_back(Name{{}, std::move((*idOpt).second.value)}, this->parseType(type));
        idOpt = nextIdOpt;
    }

    if (!idOpt) return std::nullopt;
    lexer.store(std::move((*idOpt).first));
    auto type = this->tryParseType(lexer);
    if (!type) return std::nullopt;
    return std::make_pair(std::move(params), std::move(*type));
}