#pragma once

#include <src/parser/ast/expression.hpp>

struct BlockNode : public ExpressionNode {
    
    virtual ~BlockNode() = default;

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const {
        std::cerr << "BlockNode::llvmEvaluate is not implemented.\n";
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};
    }

    inline virtual void staticAnalysis(const QContext& qctx, llvm::LLVMContext& ctx) {

    }

};
