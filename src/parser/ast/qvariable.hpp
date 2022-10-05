#pragma once

#include <iostream>
#include <memory>
#include <stacktrace>

#include <src/parser/ast/expression.hpp>
#include <src/parser/ast/name.hpp>
#include <src/parser/ast/type.hpp>

struct QVariable;
extern std::vector<const QVariable*> GlobalVariables;

struct QVariable {
    Name name;
    Type type;
    std::shared_ptr<ExpressionNode> value;
    QVariable(const Name& name, const Type& type, std::shared_ptr<ExpressionNode> value)
        : name{name}, type{type}, value{value} {}
    QVariable(Name&& name, Type&& type, std::shared_ptr<ExpressionNode> value)
        : name{std::move(name)}, type{std::move(type)}, value{value} {}

    llvm::Value* llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const {
        if (qctx.scope->lastScope) {
            // Do local variable in entry block.
            auto fn = builder.GetInsertBlock()->getParent();
            llvm::IRBuilder<> tmpBuilder(
                &fn->getEntryBlock(),
                fn->getEntryBlock().begin()
            );
            llvm::Value* alloca = tmpBuilder.CreateAlloca(
                this->type.getLLVMType(qctx, ctx), this->type.getArraylength(qctx, ctx), Util::NewCopy(std::string{this->name})
            );
            qctx.addVariable(this->name, this->type, alloca);
            if (this->value)
                builder.CreateStore(this->value->llvmEvaluate(qctx, ctx, builder).second, alloca);
            return alloca;
        } else {
            std::cerr << this->name << "\n";
            llvm::GlobalVariable* gVar = static_cast<llvm::GlobalVariable*>(
                qctx.theModule->getOrInsertGlobal(Util::NewCopy(std::string{this->name}), this->type.getLLVMType(qctx, ctx))
            );
            gVar->setLinkage(llvm::GlobalValue::LinkageTypes::ExternalLinkage);
            gVar->setConstant(false); // TODO: Compile time evaluation.
            gVar->setInitializer(nullptr);
            qctx.variables.emplace(this->name, TypedValue{this->type, gVar});
            GlobalVariables.push_back(this);
            return gVar;
        }
    }
};
