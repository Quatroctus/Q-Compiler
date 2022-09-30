#pragma once

#include <memory>
#include <vector>

#include <src/parser/ast/expression.hpp>

struct IndexNode : public ExpressionNode {
    std::shared_ptr<ExpressionNode> indexTarget;
    std::vector<std::shared_ptr<ExpressionNode>> indices;
    IndexNode(std::shared_ptr<ExpressionNode> target, std::vector<std::shared_ptr<ExpressionNode>>&& indices)
        : indexTarget{target}, indices{std::move(indices)} {}
    
    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        std::cerr << "IndexNode::llvmEvaluate is not implemented.\n";
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};   
    }
    
    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return indexTarget->getType(); // TODO: Get the elemType
    }

};
