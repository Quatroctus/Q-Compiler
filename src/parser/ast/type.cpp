#include <stacktrace>

#include <src/parser/ast/type.hpp>
#include <src/parser/ast/q_context.hpp>

llvm::Type* Type::getLLVMType(QContext& qctx, llvm::LLVMContext& ctx) const {
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
        pattern(as<std::unique_ptr<ArrayType>>(arg)) = [&](const std::unique_ptr<ArrayType>& type) -> llvm::Type* {
            return llvm::PointerType::get(type->elemType.getLLVMType(qctx, ctx), 0);
            std::cerr << "Type::getLLVMType ArrayType is not implemented.\n";
            assert(false);
            return nullptr;
        },
        pattern(as<StructureType>(arg)) = [&](const StructureType& type) -> llvm::Type* {
            if (qctx.structures.contains(type.name)) return qctx.structures.at(type.name).getLLVMType(qctx, ctx);
            assert(false);
            return nullptr;
        },
        pattern(_) = []() -> llvm::Type* { assert(false); }
    );
}

llvm::Value* Type::getArraylength(const QContext& qctx, llvm::LLVMContext& ctx) const {
    return nullptr;
}

void ExpandArrayTypes(std::vector<std::pair<Name, Type>>& vec) {
    using namespace mpark::patterns;
    for (size_t i = 0; i < vec.size(); i++) {
        auto it = vec.begin() + i;
        match((*it).second.type)(
            pattern(as<std::unique_ptr<ArrayType>>(arg)) = [&vec, &it, &i](const std::unique_ptr<ArrayType>& type) -> void {
                if (IF_IS(type->arraySize, as<Name>(_))) { // TODO: Will need to check if variable exists in scope in the future. Might be done prior to this though.
                    vec.insert(it, std::make_pair(std::get<Name>(type->arraySize), BuiltinType::SIZE));
                    i++;
                }
            },
            pattern(_) = []() -> void {}
        );
    }
}