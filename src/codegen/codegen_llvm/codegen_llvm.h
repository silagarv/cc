#ifndef CODEGEN_LLVM_H
#define CODEGEN_LLVM_H

#include "util/arena.h"

#include "codegen/codegen.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

// Struct which should be cast to a codegen result when returning from llvm
// codegeneration.
typedef struct CodegenResultLLVM {
    Arena arena; // the arena used to allocate this result
    LLVMContextRef context;
    LLVMModuleRef module;
} CodegenResultLLVM;

CodegenResult* llvm_emit_translation_unit(CodegenContext* context);
void llvm_delete_codegen_result(CodegenResult* result);

#endif /* CODEGEN_LLVM_H */
