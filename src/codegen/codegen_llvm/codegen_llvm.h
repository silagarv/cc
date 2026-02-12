#ifndef CODEGEN_LLVM_H
#define CODEGEN_LLVM_H

#include "parse/declaration.h"
#include "util/arena.h"
#include "util/hash_map.h"

#include "codegen/codegen.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

typedef struct DeclToValueRef {
    HashMap map;
} DeclToValueRef;

typedef struct JumpTarget {
    struct JumpTarget* previous;

    LLVMBasicBlockRef continue_target;
    LLVMBasicBlockRef break_target;

    // If the current break is a switch this will be the switch to add to
    LLVMValueRef current_switch;

    // If the current break is for a switch then this will be the default block
    // for that switch
    LLVMBasicBlockRef switch_default;
} JumpTarget;

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
    LLVMTypeRef function_type;

    // The current basic block
    LLVMBasicBlockRef basic_block;

    // TODO: add a hashmap from declaration to LLVMValueRef...
    DeclToValueRef map;

    // TODO: add stuff for types, functions, and the current basic blocks etc...
    JumpTarget* jump_target;    
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

void llvm_codegen_add_declaration(CodegenContext* context,
        const Declaration* decl, LLVMValueRef value);
LLVMValueRef llvm_codegen_get_declaration(CodegenContext* context,
        const Declaration* decl);

void llvm_codegen_push_break_continue(CodegenContext* context,
        LLVMBasicBlockRef br, LLVMBasicBlockRef cont);
void llvm_codegen_push_break(CodegenContext* context, LLVMBasicBlockRef bb,
        LLVMValueRef switch_val, LLVMBasicBlockRef switch_default);
void llvm_codegen_pop_jumps(CodegenContext* context);
LLVMBasicBlockRef llvm_codegen_get_break(const CodegenContext* context);
LLVMBasicBlockRef llvm_codegen_get_continue(const CodegenContext* context);
LLVMValueRef llvm_codegen_get_switch(const CodegenContext* context);
LLVMBasicBlockRef llvm_codegen_get_default(const CodegenContext* context);

#endif /* CODEGEN_LLVM_H */
