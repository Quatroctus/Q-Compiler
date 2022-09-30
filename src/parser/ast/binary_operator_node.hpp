#pragma once

#include <memory>

#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/operator_type.hpp>

struct BinaryOperatorNode : public ExpressionNode {
    OperatorType op;
    std::shared_ptr<ExpressionNode> left, right;
    BinaryOperatorNode(std::shared_ptr<ExpressionNode> left, std::shared_ptr<ExpressionNode> right, OperatorType op)
        : op{op}, left{left}, right{right} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        // TODO: Evaluate this shit.
        using enum OperatorType;
        auto[lhsType, lhsValue] = this->left->llvmEvaluate(qctx, ctx, builder);
        auto[rhsType, rhsValue] = this->right->llvmEvaluate(qctx, ctx, builder);
        // TODO: Operator overloading for structures. struct A + struct B where A could == B.
        if (!lhsType.isBuiltin() || !rhsType.isBuiltin()) {
            std::cerr << "BinaryOperatorNode::llvmEvaluate does not support non-builtin types.\n";
            assert(false);
        }
        if (lhsType.isSuperior(rhsType)) {
            auto[_, supValue] = rhsType.convert(qctx, ctx, builder, rhsValue, lhsType);
            rhsType = lhsType;
            rhsValue = supValue;
        } else if (rhsType.isSuperior(lhsType)) {
            auto[_, supValue] = lhsType.convert(qctx, ctx, builder, lhsValue, rhsType);
            lhsType = rhsType;
            lhsValue = supValue;
        }
        switch (this->op) {
            case ADD:
                if (lhsType.isIntegral())
                    return TypedValue{lhsType, builder.CreateAdd(lhsValue, rhsValue, "iadd")};
                else return TypedValue{lhsType, builder.CreateFAdd(lhsValue, rhsValue, "fadd")};
            case SUB:
                if (lhsType.isIntegral())
                    return TypedValue{lhsType, builder.CreateSub(lhsValue, rhsValue, "isub")};
                else return TypedValue{lhsType, builder.CreateFSub(lhsValue, rhsValue, "fsub")};
            case MUL:
                if (lhsType.isIntegral())
                    return TypedValue{lhsType, builder.CreateMul(lhsValue, rhsValue, "smul")};
                else return TypedValue{lhsType, builder.CreateFMul(lhsValue, rhsValue, "fmul")};
            case DIV:
                if (lhsType.isIntegral())
                    if (lhsType.isSigned()) 
                        return TypedValue{lhsType, builder.CreateSDiv(lhsValue, rhsValue, "sdiv")};
                    else return TypedValue{lhsType, builder.CreateUDiv(lhsValue, rhsValue, "udiv")};
                else return TypedValue{lhsType, builder.CreateFDiv(lhsValue, rhsValue, "fdiv")};
            case MOD:
                if (lhsType.isIntegral())
                    if (lhsType.isSigned())
                        return TypedValue{lhsType, builder.CreateSRem(lhsValue, rhsValue, "smod")};
                    else return TypedValue{lhsType, builder.CreateURem(lhsValue, rhsValue, "umod")};
                else return TypedValue{lhsType, builder.CreateFRem(lhsValue, rhsValue, "fmod")};
        }
        
        std::cerr << "Un-handled OperatorType: " << static_cast<uint32_t>(this->op) << ".\n";
        assert(false);
        return TypedValue{BuiltinType::NIX, nullptr};
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return left->getType(); // TODO: Get actual type from result of `op` on left and right.
    }
};
