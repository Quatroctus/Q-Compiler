#pragma once

#include <iostream>
#include <optional>
#include <stacktrace>

#include <src/parser/ast/type.hpp>
#include <src/parser/ast/q_context.hpp>

#include <llvm-13/llvm/IR/LLVMContext.h>
#include <llvm-13/llvm/IR/IRBuilder.h>
#include <llvm-13/llvm/IR/Value.h>

struct ExpressionNode {
    virtual ~ExpressionNode() { }

    virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const = 0;
    virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) = 0;

    virtual Type getType() const = 0; // TODO: Add QContext parameter.
};
