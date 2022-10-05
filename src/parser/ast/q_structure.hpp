#pragma once

#include <src/parser/ast/name.hpp>
#include <src/parser/ast/type.hpp>

#include <llvm-13/llvm/IR/Type.h>

struct QStructure {
    Name name;
    std::vector<std::pair<Name, Type>> members;
    llvm::StructType* llvmStructure = nullptr;
    template<typename... Members>
    QStructure(Name&& name, Members&&... members) : name{std::move(name)}, members{std::forward<Members>(members)...} {
        ExpandArrayTypes(this->members);
    }
    // TODO: Methods?, Generics, Other things.
    inline llvm::StructType* getLLVMType(QContext& qctx, llvm::LLVMContext& ctx) {
        if (this->llvmStructure) return this->llvmStructure;
        std::vector<llvm::Type*> memTypes{this->members.size()};
        std::transform(this->members.begin(), this->members.end(), memTypes.begin(),
        [&](const std::pair<Name, Type>& p) {
            return p.second.getLLVMType(qctx, ctx);
        });
        this->llvmStructure = llvm::StructType::get(ctx, memTypes, false); // IsPacked should be changeable???
        return this->llvmStructure;
    }
};
