#pragma once

#include <memory>

#include <src/parser/ast/block_node.hpp>
#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/name.hpp>
#include <src/parser/ast/type.hpp>

#include <lib/patterns.hpp>

#include <llvm-13/llvm/IR/LLVMContext.h>
#include <llvm-13/llvm/IR/IRBuilder.h>
#include <llvm-13/llvm/IR/Value.h>

struct QFunction {
    Name name;
    std::vector<std::pair<Name, Type>> parameters;
    Type returnType;
    std::shared_ptr<ExpressionNode> value;
    QFunction(const Name& name, std::vector<std::pair<Name, Type>>&& parameters, Type&& returnType, std::shared_ptr<ExpressionNode> value)
        : name{name}, parameters{std::move(parameters)}, returnType{std::move(returnType)}, value{value} {
            ExpandArrayTypes(this->parameters);
        }

    void llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const {
        // TODO: Evaluate a function.
        using namespace mpark::patterns;
        auto& fnCallee = qctx.functions.at(this->name);
        auto fnType = fnCallee.getFunctionType();
        llvm::Function* fn = (llvm::Function*) fnCallee.getCallee();
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(ctx, "entry", fn);
        builder.SetInsertPoint(entry);
        qctx.newScope();
        for (size_t i = 0; i < this->parameters.size(); i++) {
            qctx.addVariable(this->parameters[i].first, this->parameters[i].second, fn->getArg(i));
        }
        if (std::dynamic_pointer_cast<BlockNode>(this->value)) { // If we are a block { statement; ...}
            if (!this->value->llvmEvaluate(qctx, ctx, builder).second) {
                if (IF_IS(this->returnType.type, as<BuiltinType>(BuiltinType::NIX))) {
                    builder.CreateRet(nullptr);
                } else {
                    // ERROR: Reached end of function expecting a return value.
                }
            }
        } else { // If we are a single expression.
            auto val = this->value->llvmEvaluate(qctx, ctx, builder);
            if (!val.second) {
                if (IF_IS(this->returnType.type, as<BuiltinType>(BuiltinType::NIX))) {
                    builder.CreateRet(nullptr);
                } else {
                    // ERROR: Reached end of function expecting a return value.
                }
            } else {
                auto[t, v] = val.first.convert(qctx, ctx, builder, val.second, this->returnType);
                builder.CreateRet(v);
            }
        }
        qctx.delScope();
    }

};
