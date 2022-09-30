#pragma once

#include <memory>

#include <src/lexical/token.hpp>
#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/name.hpp>

struct IdentifierNode : public ExpressionNode {
    Name name;
    IdentifierNode(Name&& name) : name{std::move(name)} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        return qctx.scope->get(this->name);
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return BuiltinType::I32; // TODO: Get type from variable lookup.
    }
};
