#include <iostream>
#include <unordered_map>

#include <src/lexical/lexer.hpp>
#include <src/parser/parser.hpp>
#include <src/util/string.hpp>

#include <lib/patterns.hpp>

#include <llvm-13/llvm/IR/LLVMContext.h>
#include <llvm-13/llvm/IR/IRBuilder.h>
#include <llvm-13/llvm/IR/Module.h>
#include <llvm-13/llvm/IR/Verifier.h>
#include <llvm-13/llvm/IR/Value.h>

#include <llvm-13/llvm/IR/LegacyPassManager.h>
#include <llvm-13/llvm/Support/TargetSelect.h>
#include <llvm-13/llvm/Support/TargetRegistry.h>
#include <llvm-13/llvm/Support/raw_ostream.h>
#include <llvm-13/llvm/Support/FileSystem.h>
#include <llvm-13/llvm/Target/TargetMachine.h>
#include <llvm-13/llvm/Target/TargetOptions.h>
#include <llvm-13/llvm/Support/Host.h>

std::vector<const QVariable*> GlobalVariables;

int generateMachineCode(const std::string& filename, const QContext& qctx) {
    std::string targetTriple = llvm::sys::getDefaultTargetTriple();
    //std::cerr << " Target triple: " << targetTriple << "\n";

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string error;
    const llvm::Target* target = llvm::TargetRegistry::lookupTarget(
        targetTriple,
        error
    );
    if (!target) {
        std::cerr << "Failed to look up target; error: " << error << "\n";
        return 1;
    }

    llvm::TargetOptions options;
    llvm::TargetMachine* targetMachine = target->createTargetMachine(
        targetTriple,
        "generic",
        "",
        options,
        llvm::Optional<llvm::Reloc::Model>()
    );

    qctx.theModule->setDataLayout(targetMachine->createDataLayout());
    qctx.theModule->setTargetTriple(targetTriple);

    std::error_code ec;
    llvm::raw_fd_ostream outfile(filename, ec, llvm::sys::fs::OF_None);
    if (ec) {
        std::cerr << "Could not open output file; error: " << ec.message() << "\n";
        return 1;
    }

    llvm::legacy::PassManager pm;
    llvm::CodeGenFileType fileType =
        llvm::CGFT_ObjectFile;
    if (targetMachine->addPassesToEmitFile(pm, outfile, NULL, fileType)) {
        std::cerr << "Can't emit object file" << "\n";
        return 1;
    }
    pm.run(*qctx.theModule);
    outfile.close();
    return 0;
}

int main(int argc, char** args) {
    // TODO: Parse command line arguments.
    Lexer lexer{args[1]};
    Parser parser;
    std::unordered_map<Name, QVariable> variables;
    std::unordered_map<Name, QFunction> functions;
    parser.parse(lexer, variables, functions);
    std::cerr << "Collected " << variables.size() << " variables.\n";
    std::cerr << "Collected " << functions.size() << " functions.\n";
    for (const auto&[vName, _] : variables) {
        std::cerr << "Variable named: \"" << vName << "\".\n";
    }
    for (const auto&[fName, _] : functions) {
        std::cerr << "Function named: \"" << fName << "\".\n";
    }


    // TODO: Perform Static Analysis.

    // TODO: Perform LLVM AST evaluation.
    llvm::LLVMContext ctx;
    QContext qctx{std::make_unique<llvm::Module>(args[1], ctx)};
    assert(qctx.theModule.get());
    llvm::IRBuilder<> builder{ctx};

    std::unordered_map<Name, llvm::FunctionCallee> funcs;
    for (const auto&[name, qfun] : functions) {
        llvm::FunctionType* fnType;
        if (qfun.parameters.empty()) fnType = llvm::FunctionType::get(qfun.returnType.getLLVMType(qctx, ctx), false);
        else {
            std::vector<llvm::Type*> paramTypes{qfun.parameters.size()};
            std::transform(qfun.parameters.begin(), qfun.parameters.end(), paramTypes.begin(),
                [&](auto&& p) {
                    return p.second.getLLVMType(qctx, ctx);
                }
            );
            fnType = llvm::FunctionType::get(qfun.returnType.getLLVMType(qctx, ctx), paramTypes, false);
            
        }
        llvm::Function* fn = llvm::Function::Create(fnType, llvm::GlobalValue::ExternalLinkage, Util::NewCopy(std::string{name}), qctx.theModule.get());
        for (size_t i = 0; i < qfun.parameters.size(); i++) {
            fn->getArg(i)->setName(Util::NewCopy(std::string{qfun.parameters[i].first}));
        }
        funcs.emplace(name, llvm::FunctionCallee{fnType, fn});
    }
    qctx.prime(std::move(funcs));

    //std::unordered_map<Name, TypedValue> vars;
    for (const auto&[name, qvar] : variables) {
        qvar.llvmEvaluate(qctx, ctx, builder);
    }

    for (auto&[fnName, qFN] : functions) {
        qFN.llvmEvaluate(qctx, ctx, builder);
        llvm::verifyFunction(*((llvm::Function*) qctx.functions.at(fnName).getCallee()));
    }

    // TODO: Output LLVM output.
    qctx.theModule->print(llvm::outs(), NULL);
    std::string outFile = args[1];
    outFile = outFile.substr(0, Util::FindLast(outFile, '.')) + 'o';
    generateMachineCode(outFile, qctx);

    return 0;
}
