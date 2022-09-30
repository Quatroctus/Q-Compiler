#pragma once

#include <src/lexical/token.hpp>
#include <src/parser/ast/expression.hpp>

struct IntegerNode : public ExpressionNode {
    uint64_t value;
    enum class Size { B=8, W=16, D=32, Q=64 } size;
    IntegerNode(IntegerType type) : value{type.value}, size{Size::Q} {}
    virtual ~IntegerNode() {}

    inline virtual TypedValue llvmEvaluate(QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder) const override {
        auto type = llvm::IntegerType::get(ctx, static_cast<uint32_t>(this->size));
        std::cerr << "IntegerNode::value: " << this->value << "\n";
        return TypedValue{
            this->getType(), 
            llvm::ConstantInt::get(type, llvm::APInt{static_cast<uint32_t>(this->size), this->value})
        };
    }

    inline virtual void staticAnalysis(const QContext& qctx, std::optional<Type>& expected) override {
        if (expected) {
            if ((*expected).isBuiltin()) {
                BuiltinType type = std::get<BuiltinType>((*expected).type);
                switch (type) {
                    case BuiltinType::U8: case BuiltinType::I8: case BuiltinType::BOOL: this->size = Size::B; break;
                    case BuiltinType::U16: case BuiltinType::I16: this->size = Size::W; break;
                    case BuiltinType::U32: case BuiltinType::I32: this->size = Size::D; break;
                    case BuiltinType::U64: case BuiltinType::I64: case BuiltinType::F32:
                    case BuiltinType::F64: case BuiltinType::SSIZE: case BuiltinType::SIZE:
                        this->size = Size::Q;
                        break;
                }
            } else {
                // TODO: Check arrays and structures.
            }
        }
    }

    inline virtual Type getType() const override {
        switch (this->size) {
            case Size::B: return BuiltinType::U8;
            case Size::W: return BuiltinType::U16;
            case Size::D: return BuiltinType::U32;
            case Size::Q: return BuiltinType::U64;
        }
        return BuiltinType::U64;
    }
};
