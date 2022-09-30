#pragma once

#include <iostream>
#include <memory>
#include <stacktrace>

#include <src/lexical/token.hpp>
#include <src/parser/ast/expression.hpp>

struct FloatingNode : ExpressionNode {
    bool large;
    double value;
    FloatingNode(FloatingType type) : large{type.large}, value{type.value} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        return TypedValue{
            this->getType(),
            llvm::ConstantFP::get(ctx, llvm::APFloat(this->large ? this->value : static_cast<float>(this->value)))
        };
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return large ? BuiltinType::F64 : BuiltinType::F32;
    }
};
