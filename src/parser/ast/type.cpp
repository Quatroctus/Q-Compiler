#include <stacktrace>

#include <src/parser/ast/type.hpp>

llvm::Type* Type::getLLVMType(const QContext& qctx, llvm::LLVMContext& ctx) const {
    using namespace mpark::patterns;
    return match(this->type)(
        pattern(as<BuiltinType>(arg)) = [&](BuiltinType type) -> llvm::Type* {
            switch (type) {
                case BuiltinType::NIX: return llvm::Type::getVoidTy(ctx);
                case BuiltinType::BOOL: return reinterpret_cast<llvm::Type*>(llvm::Type::getInt1Ty(ctx));
                case BuiltinType::U8: case BuiltinType::I8: return reinterpret_cast<llvm::Type*>(llvm::Type::getInt8Ty(ctx));
                case BuiltinType::U16: case BuiltinType::I16: return reinterpret_cast<llvm::Type*>(llvm::Type::getInt16Ty(ctx));
                case BuiltinType::U32: case BuiltinType::I32: return reinterpret_cast<llvm::Type*>(llvm::Type::getInt32Ty(ctx));
                case BuiltinType::U64: case BuiltinType::I64: 
                case BuiltinType::SSIZE: case BuiltinType::SIZE: return reinterpret_cast<llvm::Type*>(llvm::Type::getInt64Ty(ctx));
                case BuiltinType::F32: return llvm::Type::getFloatTy(ctx);
                case BuiltinType::F64: return llvm::Type::getDoubleTy(ctx);
            }
            std::cerr << "Unhandled BuiltinType: " << static_cast<uint32_t>(type) << "\n";
            assert(false);
            return nullptr;
        },
        pattern(as<ArrayType*>(arg)) = [&](const ArrayType* type) -> llvm::Type* {
            std::cerr << "Type::getLLVMType ArrayType is not implemented.\n";
            assert(false);
            return nullptr;
        },
        pattern(as<StructureType>(arg)) = [&](const StructureType& type) -> llvm::Type* {
            std::cerr << "Type::getLLVMType StructureType is not implemented.\n";
            std::cerr << std::stacktrace::current() << "\n";
            assert(false);
            return nullptr;
        },
        pattern(_) = []() -> llvm::Type* { return nullptr; }
    );
}

llvm::Value* Type::getArraylength(const QContext& qctx, llvm::LLVMContext& ctx) const {
    return nullptr;
}
