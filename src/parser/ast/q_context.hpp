#pragma once

#include <string>
#include <unordered_map>

#include <src/parser/ast/name.hpp>
#include <src/parser/ast/type.hpp>

#include <llvm-13/llvm/IR/Value.h>
#include <llvm-13/llvm/IR/Function.h>

using TypedValue = std::pair<Type, llvm::Value*>;

struct Scope {
    Scope* lastScope;
    std::unordered_map<Name, TypedValue> variables;
    Scope(Scope* scope, std::unordered_map<Name, TypedValue>&& variables)
        : lastScope{scope}, variables{std::move(variables)} {}
    ~Scope() { if (lastScope) delete lastScope; }

    bool contains(const Name& name, bool recurse=true) const {
        if (!this->variables.contains(name)) {
            return recurse && lastScope && lastScope->contains(name);
        }
        return true;
    }

    
    TypedValue get(const Name& name) {
        if (this->variables.contains(name)) return this->variables.at(name);
        if (this->lastScope) return this->lastScope->get(name);
        return {BuiltinType::NIX, nullptr};
    }

};

struct QContext {
    Scope* scope;
    std::unordered_map<std::string, llvm::ArrayRef<llvm::Constant*>> stringLiterals;
    std::unordered_map<Name, llvm::FunctionCallee> functions;
    std::unordered_map<Name, TypedValue>& variables; // Global variables
    std::unique_ptr<llvm::Module> theModule;
    QContext(std::unique_ptr<llvm::Module>&& module)
        : scope{new Scope(nullptr, {})}, variables{scope->variables}, theModule{std::move(module)} {}

    void prime(std::unordered_map<Name, llvm::FunctionCallee>&& functions) {
        this->functions = std::move(functions);
    }

    void newScope() {
        this->scope = new Scope(this->scope, {});
    }

    void delScope() {
        auto tmp = this->scope;
        this->scope = this->scope->lastScope;
        delete tmp;
    }

    void addVariable(const Name& name, const Type& type, llvm::Value* value) {
        this->scope->variables.emplace(name, TypedValue{type, value});
    }

};
