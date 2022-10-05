#pragma once

#include <src/parser/ast/expression.hpp>

struct ConditionalNode : public ExpressionNode {
    std::optional<Type> returning{};
    std::vector<std::pair<std::shared_ptr<ExpressionNode>, std::shared_ptr<ExpressionNode>>> ifBlocks;
    std::shared_ptr<ExpressionNode> elseBlock;

    ConditionalNode(std::vector<std::pair<std::shared_ptr<ExpressionNode>, std::shared_ptr<ExpressionNode>>>&& ifBlocks, std::shared_ptr<ExpressionNode> elseBlock)
        : ifBlocks(std::move(ifBlocks)), elseBlock{elseBlock} {}
    virtual ~ConditionalNode() = default;

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        // TODO: Assignment to a conditional.
        llvm::BasicBlock* continueBlock = llvm::BasicBlock::Create(ctx, "continue");
        auto currFunc = builder.GetInsertBlock()->getParent();
        auto it = this->ifBlocks.begin();
        for (; it != this->ifBlocks.end(); it++) {
            const auto&[cond, value] = *it;
            auto[type, raw] = cond->llvmEvaluate(qctx, ctx, builder);
            auto[_, conditional] = type.convert(qctx, ctx, builder, raw, BuiltinType::BOOL);
            llvm::BasicBlock* ifBlock = llvm::BasicBlock::Create(ctx, "if", currFunc);
            if (it+1 == this->ifBlocks.end()) {
                if (this->elseBlock) { // this->elseBlock
                    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(ctx, "else");
                    builder.CreateCondBr(conditional, ifBlock, elseBlock);
                    builder.SetInsertPoint(ifBlock);
                    auto[ifT, ifV] = value->llvmEvaluate(qctx, ctx, builder);
                    if (this->returning) {
                        auto[type, value] = ifT.convert(qctx, ctx, builder, ifV, *this->returning);
                        builder.CreateRet(value);
                    } else
                        builder.CreateBr(continueBlock);
                    
                    currFunc->getBasicBlockList().push_back(elseBlock);
                    builder.SetInsertPoint(elseBlock);
                    auto[elT, elV] = this->elseBlock->llvmEvaluate(qctx, ctx, builder);
                    if (this->returning) {
                        auto[type, value] = elT.convert(qctx, ctx, builder, elV, *this->returning);
                        builder.CreateRet(value);
                    } else
                        builder.CreateBr(continueBlock);
                } else { // continueBlock
                    builder.CreateCondBr(conditional, ifBlock, continueBlock);
                    builder.SetInsertPoint(ifBlock);
                    auto[ifT, ifV] = value->llvmEvaluate(qctx, ctx, builder);
                    if (this->returning) {
                        auto[type, value] = ifT.convert(qctx, ctx, builder, ifV, *this->returning);
                        builder.CreateRet(value);
                    } else
                        builder.CreateBr(continueBlock);
                }
            } else { // elif Block
                llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(ctx, "else");
                builder.CreateCondBr(conditional, ifBlock, elseBlock);
                builder.SetInsertPoint(ifBlock);
                auto[ifT, ifV] = value->llvmEvaluate(qctx, ctx, builder);
                if (this->returning) {
                    auto[type, value] = ifT.convert(qctx, ctx, builder, ifV, *this->returning);
                    builder.CreateRet(value);
                } else 
                    builder.CreateBr(continueBlock);

                currFunc->getBasicBlockList().push_back(elseBlock);
                builder.SetInsertPoint(elseBlock);
            }
        }
        if (!this->returning) {
            currFunc->getBasicBlockList().push_back(continueBlock);
            builder.SetInsertPoint(continueBlock);
        }
        return TypedValue{BuiltinType::NIX, nullptr};
    } 

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {

    }

    inline virtual Type getType() const {
        return BuiltinType::NIX;
    }

};

