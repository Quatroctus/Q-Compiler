#pragma once

#include <memory>
#include <vector>

#include <src/parser/ast/expression.hpp>

struct FNCallNode : public ExpressionNode {
    std::shared_ptr<ExpressionNode> functionTarget;
    std::vector<std::shared_ptr<ExpressionNode>> arguments;
    FNCallNode(std::shared_ptr<ExpressionNode> target, std::vector<std::shared_ptr<ExpressionNode>>&& arguments)
        : functionTarget{target}, arguments{std::move(arguments)} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override{
        std::cerr << "FNCallNode::llvmEvaluate is not implemented.\n";
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override { // We need a way to get the return type from this->functionTarget
        return BuiltinType::I32;
    }
};
