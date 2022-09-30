#pragma once

#include <algorithm>
#include <memory>
#include <string>

#include <src/lexical/token.hpp>
#include <src/parser/ast/expression.hpp>

struct StringNode : public ExpressionNode {
    std::string value;
    StringNode(const StringLiteralType& type) : value{type.translated} {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        if (!qctx.stringLiterals.contains(this->value)) {
            std::vector<llvm::Constant*> stringConst{this->value.size() + 1};
            std::transform(this->value.begin(), this->value.end(), stringConst.begin(),
                [&ctx](char c) {
                    return (llvm::Constant*) llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), c, false);
                }
            );
            stringConst.back() = llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), 0, false);
            qctx.stringLiterals.emplace(this->value, std::move(stringConst));
        }
        return TypedValue{
            this->getType(),
            llvm::ConstantArray::get(
                llvm::ArrayType::get(llvm::Type::getInt8Ty(ctx), this->value.size() + 1),
                qctx.stringLiterals.at(this->value)
            )
        };
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const override {
        return MakeArrayType({{}, "u8"}, value.size() + 1);
    }
};
