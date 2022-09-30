#pragma once

#include <memory>
#include <vector>

#include <src/parser/ast/expression.hpp>

struct ConstructorNode : public ExpressionNode {
    std::optional<Type> type;
    std::vector<std::shared_ptr<ExpressionNode>> arguments;
    ConstructorNode(std::vector<std::shared_ptr<ExpressionNode>>&& arguments) : arguments{std::move(arguments)} {}
    ConstructorNode(const Type& type, std::vector<std::shared_ptr<ExpressionNode>>&& arguments)
        : type{type}, arguments{std::move(arguments)} {}
    ConstructorNode(Type&& type, std::vector<std::shared_ptr<ExpressionNode>>&& arguments)
        : type{std::move(type)}, arguments{std::move(arguments)} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        std::cerr << "ConstructorNode::llvmEvaluate is not implemented.\n";
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};   
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return *this->type;
    }

};
