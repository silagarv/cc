#include "codegen_llvm.h"
#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_declaration.h"
#include "files/filepath.h"
#include "parse/ast.h"
#include "parse/ast_allocator.h"
#include "parse/declaration.h"
#include "util/arena.h"

#include <llvm-c-18/llvm-c/Core.h>
#include <llvm-c-18/llvm-c/Types.h>
#include <stdio.h>

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

    // TODO: add stuff for types, functions, and the current basic blocks etc...
} CodegenLLVM;

static CodegenLLVM* create_codegen_context_llvm(CodegenContext* context)
{
    LLVMContextRef c = LLVMContextCreate();

    // Get our filepath as our module name
    const char* name = filepath_get_cstr(context->input_file);

    CodegenLLVM* llvm = arena_allocate_size(&context->arena,
            sizeof(CodegenLLVM));
    *llvm = (CodegenLLVM)
    {
        .context = c,
        .module = LLVMModuleCreateWithNameInContext(name, c),
        .builder = LLVMCreateBuilderInContext(c)
    };
    
    return llvm;
}

static void delete_codegen_context_llvm(CodegenLLVM* llvm, bool okay)
{
    // Only dispose of our builder since we are all done with it!
    LLVMDisposeBuilder(llvm->builder);

    // Only delete the others if the codegen has not succeeded for some reason
    if (okay == false)
    {
        LLVMDisposeModule(llvm->module);
        LLVMContextDispose(llvm->context);
    }
}

static CodegenResult* llvm_finish_codegen(CodegenContext* context)
{
    // If the codegen had some errors with is we have freed all of our memory
    // already so we will return NULL here to indicate and error.
    if (context->codegen_okay == false)
    {
        return NULL;
    }

    // Otherwise get our backend specific data and create our codegen result.
    CodegenLLVM* c = context->backend_specific;

    CodegenResultLLVM* r = arena_allocate_size(&context->arena,
            sizeof(CodegenResultLLVM));
    r->arena = context->arena;
    r->context = c->context;
    r->module = c->module;

    // Must cast to a generic codegen result
    return (CodegenResult*) r;
}

CodegenResult* llvm_emit_translation_unit(CodegenContext* context)
{
    printf("entering codegen llvm!\n");

    // First start by initializing our backend specific data so that llvm is
    // able to work correctly.
    context->backend_specific = create_codegen_context_llvm(context);

    // Now get the external declarations from the ast
    DeclarationList decls = ast_get_external_decls(context->ast);
    DeclarationListEntry* entry = declaration_list_iter(&decls);
    for (; entry != NULL; entry = declaration_list_next(entry))
    {
        // Get the declaration, set it as the current external declaration and
        // then go and perform code generation on it
        Declaration* decl = declaration_list_entry_get(entry);
        context->current_external = decl;

        llvm_codegen_external_declaration(context, decl);
    }

    // Delete anything else that we don't need anymore but keep what we need.
    delete_codegen_context_llvm(context->backend_specific,
            context->codegen_okay);

    // Finally, finish code generation any product a codegen result for us to
    // use for later.
    return llvm_finish_codegen(context);
}

void llvm_delete_codegen_result(CodegenResult* result)
{
    // In the event that we didn't actually get any codegen result due to some
    // failure, just do nothing.
    if (result == NULL)
    {
        return;
    }

    // Otherwise these are our codegen results that we need to delete
    CodegenResultLLVM* r = (CodegenResultLLVM*) result;
    LLVMDisposeModule(r->module);
    LLVMContextDispose(r->context);

    arena_delete(&r->arena);
}
