#pragma once

#include <stacktrace>

#include <src/parser/ast/name.hpp>
#include <src/util/variant.hpp>

#include <lib/patterns.hpp>

#include <llvm-13/llvm/IR/LLVMContext.h>
#include <llvm-13/llvm/IR/IRBuilder.h>
#include <llvm-13/llvm/IR/Value.h>
#include <llvm-13/llvm/IR/Type.h>

enum struct BuiltinType {
    NIX, BOOL, U8, U16, U32, U64, I8, I16, I32, I64, SIZE, SSIZE, F32, F64
};

struct StructureType {
    Name name;
};

struct ArrayType;
struct QContext;
struct Type {
    using TypedValue = std::pair<Type, llvm::Value*>;
    using Types = std::variant<BuiltinType, StructureType, std::unique_ptr<ArrayType>>;
    Types type;
    inline Type(std::unique_ptr<ArrayType>& type) = delete;
    inline Type(const std::unique_ptr<ArrayType>& type) = delete;
    // template<typename T>
    // inline Type(T&& type) : type(std::forward<T>(type)) {}

    inline Type(BuiltinType type) : type{type} {}
    inline Type(StructureType&& type) : type{std::move(type)} {}
    inline Type(const StructureType& type) : type{type} {}
    inline Type(std::unique_ptr<ArrayType>&& type) : type{std::move(type)} {}

    inline operator Types&() { return this->type; }
    inline Type(const Type& type);
    inline Type(Type&& type) = default;
    Type& operator=(const Type& type);
    Type& operator=(Type&& type) = default;
    ~Type() = default;

    inline bool operator==(const Type& type) const;

    inline bool isBuiltin() const {
        return this->type.index() == 0;
    }

    inline bool isSigned() const {
        using enum BuiltinType;
        using namespace mpark::patterns;
        return IF_IS(this->type, as<BuiltinType>(anyof(I8, I16, I32, I64, SSIZE)));
    }

    inline bool isIntegral() const {
        using enum BuiltinType;
        using namespace mpark::patterns;
        return IF_IS(this->type, as<BuiltinType>(anyof(BOOL, U8, I8, U16, I16, U32, I32, U64, I64, SIZE, SSIZE)));
    }

    inline bool isSuperior(const Type& type) const {
        using namespace mpark::patterns;
        using enum BuiltinType;
        BuiltinType self = std::get<BuiltinType>(this->type);
        BuiltinType other = std::get<BuiltinType>(type.type);
        if (other == NIX || self == NIX) return false;
        if (other == F64) return false;
        if (self == F64) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, U16, I16, U32, I32, I64, U64)) && self == F32) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, U16, I16, U32, I32, I64)) && (self == SIZE || self == U64)) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, U16, I16, U32, I32)) && (self == SSIZE || self == I64)) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, U16, I16, I32)) && self == U32) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, U16, I16)) && self == I32) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8, I16)) && self == U16) return true;
        if (IF_IS(other, anyof(BOOL, U8, I8)) && self == I16) return true;
        if (IF_IS(other, anyof(BOOL, I8)) && self == U8) return true;
        if (IF_IS(other, anyof(BOOL)) && self == I8) return true;
        return false;
    }

    inline bool isConvertible(const Type& type) const {
        using namespace mpark::patterns;
        return match(this->type, type.type)(
            pattern(as<BuiltinType>(arg), as<BuiltinType>(arg)) = [](BuiltinType t1, BuiltinType t2) {
                if (t1 == BuiltinType::NIX || t2 == BuiltinType::NIX) return false;
                return true;
            },
            pattern(_, _) = []() -> bool { return false; }
        );
    }

    inline TypedValue convert(const QContext& qctx, llvm::LLVMContext& ctx, llvm::IRBuilder<>& builder, llvm::Value* val, const Type& target) const {
        using namespace mpark::patterns;
        using enum BuiltinType;
        return match(this->type, target.type)(
            pattern(as<BuiltinType>(arg), as<BuiltinType>(arg)) = [&](BuiltinType self, BuiltinType target) {
                if (self == target) return TypedValue{self, val};
                if (target == F64) {
                    switch (self) {
                        case BOOL: case U8: case U16: case U32: case U64: case SIZE:
                            return TypedValue{F64, new llvm::UIToFPInst(val, llvm::Type::getDoubleTy(ctx), "utod", builder.GetInsertBlock())};
                        case I8: case I16: case I32: case I64: case SSIZE:
                            return TypedValue{F64, new llvm::SIToFPInst(val, llvm::Type::getDoubleTy(ctx), "stod", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{F64, new llvm::FPExtInst(val, llvm::Type::getDoubleTy(ctx), "ftod", builder.GetInsertBlock())};
                    }
                } else if (target == F32) {
                    switch (self) {
                        case BOOL: case U8: case U16: case U32: case U64: case SIZE:
                            return TypedValue{F32, new llvm::UIToFPInst(val, llvm::Type::getFloatTy(ctx), "utof", builder.GetInsertBlock())};
                        case I8: case I16: case I32: case I64: case SSIZE:
                            return TypedValue{F32, new llvm::SIToFPInst(val, llvm::Type::getFloatTy(ctx), "stof", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{F32, new llvm::FPTruncInst(val, llvm::Type::getFloatTy(ctx), "dtof", builder.GetInsertBlock())};
                    }
                } else if (target == SIZE || target == U64) {
                    switch (self) {
                        case BOOL: case U8: case U16: case U32: case I8: case I16: case I32: case SSIZE:
                            return TypedValue{target, new llvm::ZExtInst(val, llvm::Type::getInt64Ty(ctx), "zextq", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{target, new llvm::FPToUIInst(val, llvm::Type::getInt64Ty(ctx), "ftouq", builder.GetInsertBlock())};
                         case F64:
                            return TypedValue{target, new llvm::FPToUIInst(val, llvm::Type::getInt64Ty(ctx), "dtouq", builder.GetInsertBlock())};
                    }
                } else if (target == SSIZE || target == I64) {
                    switch (self) {
                        case I8: case I16: case I32: 
                            return TypedValue{target, new llvm::SExtInst(val, llvm::Type::getInt64Ty(ctx), "sextq", builder.GetInsertBlock())};
                        case BOOL: case U8: case U16: case U32: case U64: case SIZE:
                            return TypedValue{target, new llvm::ZExtInst(val, llvm::Type::getInt64Ty(ctx), "zextq", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{target, new llvm::FPToSIInst(val, llvm::Type::getInt64Ty(ctx), "ftosq", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{target, new llvm::FPToSIInst(val, llvm::Type::getInt64Ty(ctx), "dtosq", builder.GetInsertBlock())};
                    }
                } else if (target == U32) {
                    switch (self) {
                        case BOOL: case U8: case U16: case I8: case I16: case I32:
                            return TypedValue{U32, new llvm::ZExtInst(val, llvm::Type::getInt32Ty(ctx), "zextd", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{U32, new llvm::TruncInst(val, llvm::Type::getInt32Ty(ctx), "uqtoud", builder.GetInsertBlock())};
                         case I64: case SSIZE:
                            return TypedValue{U32, new llvm::TruncInst(val, llvm::Type::getInt32Ty(ctx), "sqtoud", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{U32, new llvm::FPToUIInst(val, llvm::Type::getInt32Ty(ctx), "ftoud", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{U32, new llvm::FPToUIInst(val, llvm::Type::getInt32Ty(ctx), "dtoud", builder.GetInsertBlock())};
                    }
                } else if (target == I32) {
                    switch (self) {
                        case I8: case I16:
                            return TypedValue{I32, new llvm::SExtInst(val, llvm::Type::getInt32Ty(ctx), "sextd", builder.GetInsertBlock())};
                        case BOOL: case U8: case U16: case U32:
                            return TypedValue{I32, new llvm::ZExtInst(val, llvm::Type::getInt32Ty(ctx), "zextd", builder.GetInsertBlock())};
                        case I64: case SSIZE:
                            return TypedValue{I32, new llvm::TruncInst(val, llvm::Type::getInt32Ty(ctx), "sqtosd", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{I32, new llvm::TruncInst(val, llvm::Type::getInt32Ty(ctx), "uqtosd", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{I32, new llvm::FPToSIInst(val, llvm::Type::getInt32Ty(ctx), "ftosd", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{I32, new llvm::FPToSIInst(val, llvm::Type::getInt32Ty(ctx), "dtosd", builder.GetInsertBlock())};
                    }
                } else if (target == U16) {
                    switch (self) {
                        case BOOL: case U8: case I8: case I16:
                            return TypedValue{U16, new llvm::ZExtInst(val, llvm::Type::getInt16Ty(ctx), "zextw", builder.GetInsertBlock())};
                        case U32: 
                            return TypedValue{U16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "udtouw", builder.GetInsertBlock())};
                        case I32: 
                            return TypedValue{U16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "sdtouw", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{U16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "uqtouw", builder.GetInsertBlock())};
                        case I64: case SSIZE: 
                            return TypedValue{U16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "sqtouw", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{U16, new llvm::FPToUIInst(val, llvm::Type::getInt16Ty(ctx), "ftouw", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{U16, new llvm::FPToUIInst(val, llvm::Type::getInt16Ty(ctx), "dtouw", builder.GetInsertBlock())};
                    }
                } else if (target == I16) {
                    switch (self) {
                        case BOOL: case U8: case U16:
                            return TypedValue{I16, new llvm::ZExtInst(val, llvm::Type::getInt16Ty(ctx), "zextw", builder.GetInsertBlock())};
                        case I8:
                            return TypedValue{I16, new llvm::SExtInst(val, llvm::Type::getInt16Ty(ctx), "sextw", builder.GetInsertBlock())};
                        case U32:
                            return TypedValue{I16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "udtosw", builder.GetInsertBlock())};
                        case I32:
                            return TypedValue{I16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "sdtosw", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{I16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "uqtosw", builder.GetInsertBlock())};
                        case I64: case SSIZE:
                            return TypedValue{I16, new llvm::TruncInst(val, llvm::Type::getInt16Ty(ctx), "sqtosw", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{I16, new llvm::FPToSIInst(val, llvm::Type::getInt16Ty(ctx), "ftosw", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{I16, new llvm::FPToSIInst(val, llvm::Type::getInt16Ty(ctx), "dtosw", builder.GetInsertBlock())};
                    }
                } else if (target == U8) {
                    switch (self) {
                        case BOOL: case I8:
                            return TypedValue{U8, new llvm::ZExtInst(val, llvm::Type::getInt8Ty(ctx), "zextb", builder.GetInsertBlock())};
                        case U16:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "uwtoub", builder.GetInsertBlock())};
                        case I16:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "swtoub", builder.GetInsertBlock())};
                        case U32:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "udtoub", builder.GetInsertBlock())};
                        case I32:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "sdtoub", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "uqtoub", builder.GetInsertBlock())};
                        case I64: case SSIZE:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "sqtoub", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{U8, new llvm::FPToUIInst(val, llvm::Type::getInt8Ty(ctx), "ftoub", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{U8, new llvm::FPToUIInst(val, llvm::Type::getInt8Ty(ctx), "dtoub", builder.GetInsertBlock())};
                    }
                } else if (target == I8) {
                    switch (self) {
                        case BOOL: case U8:
                            return TypedValue{U8, new llvm::ZExtInst(val, llvm::Type::getInt8Ty(ctx), "zextb", builder.GetInsertBlock())};
                        case U16:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "uwtosb", builder.GetInsertBlock())};
                        case I16:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "swtosb", builder.GetInsertBlock())};
                        case U32:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "udtosb", builder.GetInsertBlock())};
                        case I32:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "sdtosb", builder.GetInsertBlock())};
                        case U64: case SIZE:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "uqtosb", builder.GetInsertBlock())};
                        case I64: case SSIZE:
                            return TypedValue{U8, new llvm::TruncInst(val, llvm::Type::getInt8Ty(ctx), "sqtosb", builder.GetInsertBlock())};
                        case F32:
                            return TypedValue{U8, new llvm::FPToUIInst(val, llvm::Type::getInt8Ty(ctx), "ftosb", builder.GetInsertBlock())};
                        case F64:
                            return TypedValue{U8, new llvm::FPToUIInst(val, llvm::Type::getInt8Ty(ctx), "dtosb", builder.GetInsertBlock())};
                    }
                } else if (target == BOOL) {
                    switch (self) {
                        case U8:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), llvm::APInt(8, 0)), "ubtob"
                            )};
                        case I8:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), llvm::APInt(8, 0)), "sbtob"
                            )};
                        case U16:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt16Ty(ctx), llvm::APInt(16, 0)), "uwtob"
                            )};
                        case I16:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt16Ty(ctx), llvm::APInt(16, 0)), "swtob"
                            )};
                        case U32:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), llvm::APInt(32, 0)), "udtob"
                            )};
                        case I32:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), llvm::APInt(32, 0)), "sdtob"
                            )};
                        case U64: case SIZE:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx), llvm::APInt(64, 0)), "uqtob"
                            )};
                        case I64: case SSIZE:
                            return TypedValue{BOOL, builder.CreateCmp(
                                llvm::CmpInst::ICMP_NE, val, llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx), llvm::APInt(64, 0)), "sqtob"
                            )};
                        case F32:
                            return TypedValue{BOOL, builder.CreateFCmpONE(
                                val, llvm::ConstantFP::get(llvm::Type::getFloatTy(ctx), llvm::APFloat(0.0f))
                            )};
                        case F64:
                            return TypedValue{BOOL, builder.CreateFCmpONE(
                                val, llvm::ConstantFP::get(llvm::Type::getDoubleTy(ctx), llvm::APFloat(0.0))
                            )};
                    }
                } else assert(false);
                return TypedValue{NIX, nullptr};
            },
            pattern(_, _) = [&] { return TypedValue{*this, val}; }
        );
    }

    llvm::Type* getLLVMType(QContext& qctx, llvm::LLVMContext& ctx) const;

    llvm::Value* getArraylength(const QContext& qctx, llvm::LLVMContext& ctx) const;

};

struct ArrayType {
    using Length = std::variant<std::monostate, size_t, Name/*, std::shared_ptr<ExpressionNode>*/>;
    Type elemType;
    Length arraySize;
    inline ArrayType(Type&& type, Length&& length)
    : elemType{std::move(type)}, arraySize{std::move(length)} {}
};

inline bool Type::operator==(const Type& type) const {
    using namespace mpark::patterns;
    return match(this->type, type.type)(
        pattern(as<BuiltinType>(arg), as<BuiltinType>(arg)) = [](BuiltinType b1, BuiltinType b2) -> bool {
            return b1 == b2;
        },
        pattern(as<StructureType>(arg), as<StructureType>(arg)) = [](const StructureType& s1, const StructureType& s2) -> bool {
            return s1.name == s2.name;
        },
        pattern(as<std::unique_ptr<ArrayType>>(arg), as<std::unique_ptr<ArrayType>>(arg)) = [](const std::unique_ptr<ArrayType>& a1, const std::unique_ptr<ArrayType>& a2) -> bool {
            return a1->elemType == a2->elemType && a1->arraySize == a2->arraySize; // TODO: How do I check the arraySize appropriately.
        },
        pattern(_, _) = []() -> bool { return false; }
    );
}

inline Type::Type(const Type& type) {
    using namespace mpark::patterns;
    match(type.type)(
        pattern(as<std::unique_ptr<ArrayType>>(arg)) = [this] (const std::unique_ptr<ArrayType>& type) {
            this->type = std::make_unique<ArrayType>(*type);
        },
        pattern(as<BuiltinType>(_)) = [&type, this] {
            this->type = std::get<BuiltinType>(type.type);
        },
        pattern(as<StructureType>(_)) = [&type, this] {
            this->type = std::get<StructureType>(type.type);
        }
    );
}

inline Type& Type::operator=(const Type& type) {
    using namespace mpark::patterns;
    this->~Type();
    match(type.type)(
        pattern(as<std::unique_ptr<ArrayType>>(_)) = [this, &type]() { if (std::get<std::unique_ptr<ArrayType>>(type.type)) {
            this->type = std::make_unique<ArrayType>(*std::get<std::unique_ptr<ArrayType>>(type.type));
        }},
        pattern(as<BuiltinType>(_)) = [this, &type] {
            this->type = std::get<BuiltinType>(type.type);
        },
        pattern(as<StructureType>(_)) = [this, &type] {
            this->type = std::get<StructureType>(type.type);
        }
    );
    return *this;
}

inline std::ostream& operator<<(std::ostream& out, const Type& type) {
    using namespace mpark::patterns;
    if (type.type.valueless_by_exception()) assert(false);
    match(type.type)(
        pattern(as<BuiltinType>(arg)) = [&out] (BuiltinType builtin) {
            switch (builtin) {
                case BuiltinType::NIX:      out << "nix"; break;
                case BuiltinType::BOOL:     out << "bool"; break;
                case BuiltinType::U8:       out << "u8"; break;
                case BuiltinType::U16:      out << "u16"; break;
                case BuiltinType::U32:      out << "u32"; break;
                case BuiltinType::U64:      out << "u64"; break;
                case BuiltinType::I8:       out << "i8"; break;
                case BuiltinType::I16:      out << "i16"; break;
                case BuiltinType::I32:      out << "i32"; break;
                case BuiltinType::I64:      out << "i64"; break;
                case BuiltinType::SIZE:     out << "size"; break;
                case BuiltinType::SSIZE:    out << "ssize"; break;
                case BuiltinType::F32:      out << "f32"; break;
                case BuiltinType::F64:      out << "f64"; break;
            }
        },
        pattern(as<StructureType>(arg)) = [&out] (const StructureType& type) {
            out << type.name;
        },
        pattern(as<std::unique_ptr<ArrayType>>(arg)) = [&out] (const std::unique_ptr<ArrayType>& type) {
            out << type->elemType << "[" << type->arraySize << "]";
        }
    );
    return out;
}

inline static Type MakeType(Name&& name) {
    using namespace mpark::patterns;
    return match(name.nameSpace.empty(), name.name)(
        pattern(true, "nix") = [] () -> Type { return BuiltinType::NIX; },
        pattern(true, "bool") = [] () -> Type { return BuiltinType::BOOL; },
        pattern(true, anyof("char", "u8")) = [] () -> Type { return BuiltinType::U8; },
        pattern(true, "u16") = [] () -> Type { return BuiltinType::U16; },
        pattern(true, "u32") = [] () -> Type { return BuiltinType::U32; },
        pattern(true, "u64") = [] () -> Type { return BuiltinType::U64; },
        pattern(true, "i8") = [] () -> Type { return BuiltinType::I8; },
        pattern(true, "i16") = [] () -> Type { return BuiltinType::I16; },
        pattern(true, "i32") = [] () -> Type { return BuiltinType::I32; },
        pattern(true, "i64") = [] () -> Type { return BuiltinType::I64; },
        pattern(true, "size") = [] () -> Type { return BuiltinType::SIZE; },
        pattern(true, "ssize") = [] () -> Type { return BuiltinType::SSIZE; },
        pattern(true, "f32") = [] () -> Type { return BuiltinType::F32; },
        pattern(true, "f64") = [] () -> Type { return BuiltinType::F64; },
        pattern(_, _) = [&name] () -> Type { return StructureType{std::move(name)}; }
    );
}

inline static Type MakeArrayType(Type&& type, ArrayType::Length&& length) {
    auto array = std::make_unique<ArrayType>(std::move(type), ArrayType::Length{std::move(length)});
    return Type{std::move(array)};
}

void ExpandArrayTypes(std::vector<std::pair<Name, Type>>& vec);
