#pragma once

#include <memory>

#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/operator_type.hpp>

struct UnaryOperatorNode : public ExpressionNode {
    OperatorType op;
    std::shared_ptr<ExpressionNode> operand;
    UnaryOperatorNode(std::shared_ptr<ExpressionNode> operand, OperatorType op) : op{op}, operand{operand} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        using enum OperatorType;
        auto[type, value] = this->operand->llvmEvaluate(qctx, ctx, builder);
        if (!type.isBuiltin()) {
            std::cerr << "UnaryOperatorType::llvmEvaluate does not support non-builtin types.\n";
            assert(false);
        }
        switch (this->op) {
            case NEG:
                return TypedValue{type, builder.CreateNeg(value, "neg")};
        }
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return operand->getType(); // TODO: Get actual type from result of `op` on operand.
    }
};
