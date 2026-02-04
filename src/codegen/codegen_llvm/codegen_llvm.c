#include "codegen_llvm.h"

#include <stdint.h>
#include <stdio.h>

#include "driver/diagnostic.h"

#include "util/arena.h"
#include "util/hash_map.h"

#include "files/filepath.h"

#include "parse/ast.h"
#include "parse/declaration.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_declaration.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ErrorHandling.h>

uint32_t hash_decl(const void* decl)
{
    return (uintptr_t) decl & UINT32_MAX;
}

bool cmp_decl(const void* d1, const void* d2)
{
    return d1 == d2;
}

static DeclToValueRef create_decl_map(void)
{
    return (DeclToValueRef) { hash_map_create(hash_decl, cmp_decl, NULL) };
}

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
        .builder = LLVMCreateBuilderInContext(c),
        .function = NULL,
        .map = create_decl_map()
    };
    
    return llvm;
}

static void delete_codegen_context_llvm(CodegenLLVM* llvm)
{
    LLVMDisposeBuilder(llvm->builder);
    LLVMDisposeModule(llvm->module);
    LLVMContextDispose(llvm->context);
}

static CodegenResult* llvm_finish_codegen(CodegenContext* context)
{
    // Otherwise get our backend specific data and create our codegen result.
    CodegenLLVM* c = context->backend_specific;
    LLVMModuleRef m = c->module;

    // Once we have fully finished dump the assembly if requested
    if (context->options->dump_assembly)
    {
        LLVMDumpModule(c->module);
    }

    // See if we correctly build the module.
    char* error = NULL;
    if (LLVMVerifyModule(m, LLVMReturnStatusAction, &error))
    {
        diagnostic_error(context->dm, "LLVM module is invalid:\n\n%s",
                error);
        diagnostic_note(context->dm, "compile with '-S' to inspect LLVM IR");
        context->codegen_okay = false;
    }
    LLVMDisposeMessage(error);

    // For whatever we do we can free our hashmap and remove all of it's decls
    hash_map_delete(&c->map.map);

    // If the codegen had some errors with it just free all of the memory that
    // llvm used.
    if (context->codegen_okay == false)
    {
        delete_codegen_context_llvm(c);
        arena_delete(&context->arena);
        return NULL;
    }

    // Otherwise allocate or codegen result and add all of our fields into it
    // so that we can delete them later once we are completely done with them!
    CodegenResultLLVM* r = arena_allocate_size(&context->arena,
            sizeof(CodegenResultLLVM));
    r->arena = context->arena;
    r->context = c->context;
    r->module = c->module;
    r->builder = c->builder;

    // Must cast to a generic codegen result
    return (CodegenResult*) r;
}

static void llvm_emit_translation_unit_internal(CodegenContext* context)
{
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
}

CodegenResult* llvm_emit_translation_unit(CodegenContext* context)
{
    // First start by initializing our backend specific data so that llvm is
    // able to work correctly.
    context->backend_specific = create_codegen_context_llvm(context);

    // Now do the internal code generation
    llvm_emit_translation_unit_internal(context);

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
    LLVMDisposeBuilder(r->builder);
    LLVMDisposeModule(r->module);
    LLVMContextDispose(r->context);

    arena_delete(&r->arena);
}

void llvm_codegen_add_declaration(CodegenContext* context,
        const Declaration* decl, LLVMValueRef value)
{
    CodegenLLVM* llvm = context->backend_specific;
    hash_map_insert(&llvm->map.map, (void*) decl, value);
}

LLVMValueRef llvm_codegen_get_declaration(CodegenContext* context,
        const Declaration* decl)
{
    CodegenLLVM* llvm = context->backend_specific;
    return hash_map_get(&llvm->map.map, (void*) decl);
}

