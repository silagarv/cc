#ifndef CODEGEN_LLVM_H
#define CODEGEN_LLVM_H

#include "util/arena.h"

#include "codegen/codegen.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

// A struct which allows for codegeneration by llvm specifically so that we
// don't create a codegenerator which is tightly coupled to llvm.
typedef struct CodegenLLVM {
    // LLVM specific stuff is below:
    // The context that the codegeneration is occuring in.
    LLVMContextRef context;

    // The module we are doing codegeneration for
    LLVMModuleRef module;
    
    // The builder that we are going to use to add instructions to our basic
    // blocks.
    LLVMBuilderRef builder;

    // The current function that we are generating code for
    LLVMValueRef function;

    // TODO: add stuff for types, functions, and the current basic blocks etc...
} CodegenLLVM;

// Struct which should be cast to a codegen result when returning from llvm
// codegeneration.
typedef struct CodegenResultLLVM {
    Arena arena; // the arena used to allocate this result
    LLVMContextRef context;
    LLVMModuleRef module;
    LLVMBuilderRef builder;
} CodegenResultLLVM;

CodegenResult* llvm_emit_translation_unit(CodegenContext* context);
void llvm_delete_codegen_result(CodegenResult* result);

#endif /* CODEGEN_LLVM_H */
