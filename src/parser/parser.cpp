#include <src/parser/parser.hpp>

#include <algorithm>
#include <cassert>

#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/binary_operator_node.hpp>
#include <src/parser/ast/boolean_node.hpp>
#include <src/parser/ast/conditional_node.hpp>
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
            std::vector<Token> tokens;
            auto rawExpressions = this->collectExpressions(lexer, tokens);
            if (rawExpressions.empty() || tokens.empty()) {
                // Try to parse a code block.
                std::cerr << "Parser::parse parsing function code block is not implemented.\n";
                assert(false);
            } else {
                if (!this->expect<SingleType::SEMICOLON>(lexer)) {
                    // ERROR: Expected Semicolon but got ...
                }
                auto expressions = this->parseExpressions(rawExpressions, tokens);
                std::shared_ptr<ExpressionNode> value;
                auto exprtype = expressions[0]->getType();
                //if (expressions.size() > 1 || (rawExpressions[0].value.index() == 0 && !(*typing).second.isConvertible(exprtype))) {
                //    value = std::make_shared<ConstructorNode>((*typing).second, std::move(expressions));
                //} else {
                    value = expressions[0];
                //}
                if (std::dynamic_pointer_cast<ConditionalNode>(value)) {
                    std::dynamic_pointer_cast<ConditionalNode>(value)->returning = (*typing).second;
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

    do {
        auto lbOpt = this->expect<SingleType::LBRACE>(lexer);
        if (lbOpt) {
            tokens.push_back(std::move((*lbOpt).first));
            auto rbOpt = this->expect<SingleType::RBRACE>(lexer);
            if (rbOpt) {
                tokens.push_back(std::move((*rbOpt).first));
                continue;
            }
            auto lengthName = this->collectName(lexer);
            if (!lengthName.empty()) {
                auto rbOpt = this->expect<SingleType::RBRACE>(lexer);
                if (!rbOpt) {
                    lexer.store(lengthName);
                } else {
                    tokens.insert(tokens.end(), std::make_move_iterator(lengthName.begin()), std::make_move_iterator(lengthName.end()));
                    tokens.push_back(std::move((*rbOpt).first));
                    continue;
                }
            }
            // TODO: Collect Expression.
        } else break;
    } while (true);
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
                auto lps = this->collectAll<SingleType::LPAREN>(lexer);
                std::vector<Token> rps{};
                auto ifs = this->expect<SingleType::IF>(lexer);
                if (!lps.empty() && ifs) {
                    std::vector<Token> storage{};
                    this->collectBalance<SingleType::LPAREN, SingleType::RPAREN>(lexer, storage, lps.size());
                    size_t count = lps.size() + 1;
                    auto firstnot = std::find_if(storage.rbegin(), storage.rend(), [&count](const auto& tok){ count--; return count == 0 || !IF_IS(tok.type, as<SingleType>(SingleType::RPAREN)); });
                    rps.insert(rps.end(), std::make_move_iterator(storage.rbegin()), std::make_move_iterator(firstnot));
                    storage.erase(storage.end() - (firstnot - storage.rbegin()), storage.end());
                    lexer.primeStorage(std::move(storage));
                } else {
                    if (!lps.empty()) lexer.store(lps);
                }

                if (ifs) {
                    if (!lps.empty()) {tokens.insert(tokens.end(), std::make_move_iterator(lps.begin()), std::make_move_iterator(lps.end())); lps.clear();}
                    size_t start = tokens.size();
                    Conditional* conditional = new Conditional();
                    tokens.push_back(std::move((*ifs).first));
                    auto condExpr = this->collectExpressions(lexer, tokens);
                    if (condExpr.empty()) {
                        // ERROR: Expected conditional expression.
                        std::cerr << "Expected conditional expression.\n";
                        assert(false);
                    } else if (condExpr.size() > 1) {
                        // ERROR: IF statement can only take 1 expression.
                        std::cerr << "IF statement can only take 1 expression.\n";
                        assert(false);
                    }
                    auto expr = this->collectExpressions(lexer, tokens);
                    if (expr.empty()) {
                        // TODO: Parse Block.
                        std::cerr << "Parsing statement blocks is not implemented yet.\n";
                        assert(false);
                    } else if (expr.size() > 1) {
                        // ERROR: IF statement must only have 1 expression.
                        std::cerr << "IF statement can only take 1 expression.\n";
                        assert(false);
                    } else {
                        // We now have expression POG.
                        conditional->conditionals.emplace_back(std::move(condExpr[0]), std::move(expr[0]));
                    }

                    while (true) {
                        auto elif = this->expect<SingleType::ELIF>(lexer);
                        if (!elif) break;
                        tokens.push_back(std::move((*elif).first));
                        auto elCond = this->collectExpressions(lexer, tokens);
                        if (elCond.empty()) {
                            // ERROR: Expected conditional expression.
                            std::cerr << "Expected conditional expression.\n";
                            assert(false);
                        } else if (elCond.size() > 1) {
                            // ERROR: ELIF statement can only take 1 expression.
                            std::cerr << "ELIF statement can only take 1 expression.\n";
                            assert(false);
                        }
                        auto expr = this->collectExpressions(lexer, tokens);
                        if (expr.empty()) {
                            // TODO: Parse Block.
                            std::cerr << "Parsing statement blocks is not implemented yet.\n";
                            assert(false);
                        } else if (expr.size() > 1) {
                            // ERROR: ELIF statement must only have 1 expression.
                            std::cerr << "ELIF statement must only have 1 expression.\n";
                            assert(false);
                        } else {
                            // We now have expression POG.
                            conditional->conditionals.emplace_back(std::move(condExpr[0]), std::move(expr[0]));
                        }
                    }
                    auto el = this->expect<SingleType::ELSE>(lexer);
                    if (el) {
                        tokens.push_back(std::move((*el).first));
                        auto expr = this->collectExpressions(lexer, tokens);
                        if (expr.empty()) {
                            // TODO: Parse Block.
                            std::cerr << "Parsing statemnt blocks is not implemented yet.\n";
                            assert(false);
                        } else if (expr.size() > 1) {
                            // ERROR: ELSE statement must have 1 expression.
                            std::cerr << "ELSE statement must have 1 expression.\n";
                            assert(false);
                        } else {
                            // We now have expression POG.
                            conditional->elseExpr = std::move(expr[0]);
                        }
                    }
                    if (!lps.empty()) {
                        tokens.insert(tokens.end(), std::make_move_iterator(rps.begin()), std::make_move_iterator(rps.end()));
                    }
                    expression.emplace_back(start, conditional);
                    lastWasOp = false;
                    continue;
                }
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

inline Type Parser::parseType(const std::vector<Token>& tokens) {
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

inline Type Parser::parseType(std::vector<Token>&& tokens) {
    auto result{this->parseType(
        std::make_move_iterator(tokens.begin()),
        std::make_move_iterator(tokens.end())
    )};
    tokens.clear();
    return std::move(result);
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
                            exprStack.push_back(std::make_shared<IntegerNode>(std::get<IntegerType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::FLOATING:
                            exprStack.push_back(std::make_shared<FloatingNode>(std::get<FloatingType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::STRING:
                            exprStack.push_back(std::make_shared<StringNode>(std::get<StringLiteralType>(tokens.at(idx).type)));
                            break;
                        case LiteralType::IDENTIFIER:
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
                                    pattern(as<Constructor*>(arg)) = [&](Constructor* cs) { // TODO: ConstructorNode takes a type. 
                                        std::cerr << "Constructors are not supported as an expression yet.\n";
                                        assert(false);
                                        return false;
                                    },
                                    
                                    pattern(_) = [] { return true; }
                                )) break;
                                it++;
                            }
                            break;
                    }
                },
                pattern(as<Conditional*>(arg)) = [&](Conditional* cond) {
                    std::vector<std::pair<std::shared_ptr<ExpressionNode>, std::shared_ptr<ExpressionNode>>> ifs{cond->conditionals.size()};
                    std::transform(cond->conditionals.begin(), cond->conditionals.end(), ifs.begin(),
                    [this, &tokens](std::pair<ExpressionOrConstructor, ExpressionOrConstructor>& p){
                        return std::make_pair(
                            this->parseExpression(p.first, tokens),
                            this->parseExpression(p.second, tokens)
                        );
                    });
                    std::shared_ptr<ExpressionNode> el{};
                    if (cond->elseExpr)
                        el = this->parseExpression((*cond->elseExpr), tokens);
                    exprStack.push_back(std::make_shared<ConditionalNode>(std::move(ifs), el));
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

    return expressions;
}

std::vector<Name> Parser::parseNameList(Lexer& lexer) {
    std::vector<Name> names{};
    while (true) {
        auto name = this->tryParseName(lexer);
        if (name) names.push_back(std::move(*name));
        else break;
    }
    return names;
}

std::optional<Parser::VarDeclType> Parser::parseVarDeclType(Lexer& lexer) {
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
            if (type) {
                return std::make_pair(std::move(params), std::move(*type));
            }
            return std::nullopt;
        }
        auto ty{this->parseType(type)};
        params.emplace_back(Name{{}, std::move((*idOpt).second.value)}, ty);
        idOpt = nextIdOpt;
    }

    if (!idOpt) return std::nullopt;
    lexer.store(std::move((*idOpt).first));
    auto type = this->tryParseType(lexer);
    if (!type) return std::nullopt;
    return std::make_pair(std::move(params), std::move(*type));
}