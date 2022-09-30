#pragma once

#include <memory>

#include <src/lexical/token.hpp>
#include <src/parser/ast/expression.hpp>

struct BooleanNode : public ExpressionNode {
    bool value;
    BooleanNode(BooleanType type) : value{type.value} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        return TypedValue{
            BuiltinType::BOOL,
            llvm::ConstantInt::getBool(ctx, this->value)
        };
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return BuiltinType::BOOL;
    }
};
