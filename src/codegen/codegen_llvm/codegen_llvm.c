#include "codegen_llvm.h"

#include <stdint.h>
#include <stdio.h>
#include <assert.h>

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
        .map = create_decl_map(),
        .jump_target = NULL
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

    assert(c->jump_target == NULL);

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
        diagnostic_note(context->dm, "run with '-S' to dump LLVM IR");
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

void llvm_codegen_push_break_continue(CodegenContext* context,
        LLVMBasicBlockRef br, LLVMBasicBlockRef cont)
{
    CodegenLLVM* llvm = context->backend_specific;
    
    JumpTarget* new_target = arena_allocate_size(&context->arena,
            sizeof(JumpTarget));
    new_target->previous = llvm->jump_target;
    new_target->continue_target = cont;
    new_target->break_target = br;
    new_target->current_switch = NULL;

    llvm->jump_target = new_target;
}

void llvm_codegen_push_break(CodegenContext* context, LLVMBasicBlockRef bb,
        LLVMValueRef switch_val, LLVMBasicBlockRef switch_default)
{
    // Get the previous continue which could be nothing, noting that this could
    // happen for example pushing a switch not in a loop
    CodegenLLVM* llvm = context->backend_specific;
    JumpTarget* prev = llvm->jump_target;
    LLVMBasicBlockRef prev_cont = prev == NULL ? NULL : prev->continue_target;
    
    JumpTarget* new_target = arena_allocate_size(&context->arena,
            sizeof(JumpTarget));
    new_target->previous = llvm->jump_target;
    new_target->continue_target = prev_cont;
    new_target->break_target = bb;
    new_target->current_switch = switch_val;
    new_target->switch_default = switch_default;

    llvm->jump_target = new_target;
}

void llvm_codegen_pop_jumps(CodegenContext* context)
{
    CodegenLLVM* llvm = context->backend_specific;
    llvm->jump_target = llvm->jump_target->previous;
}

LLVMBasicBlockRef llvm_codegen_get_break(const CodegenContext* context)
{
    CodegenLLVM* llvm = context->backend_specific;
    return llvm->jump_target->break_target;
}

LLVMBasicBlockRef llvm_codegen_get_continue(const CodegenContext* context)
{
    CodegenLLVM* llvm = context->backend_specific;
    return llvm->jump_target->continue_target;
}

LLVMValueRef llvm_codegen_get_switch(const CodegenContext* context)
{
    CodegenLLVM* llvm = context->backend_specific;

    LLVMValueRef switch_val = NULL;
    JumpTarget* current = llvm->jump_target;
    while (current != NULL)
    {
        if (current->current_switch != NULL)
        {
            return current->current_switch;
        }

        current = current->previous;
    }

    // TODO: panic here? (since this should be an error maybe?)
    return NULL;
}

LLVMBasicBlockRef llvm_codegen_get_default(const CodegenContext* context)
{
    CodegenLLVM* llvm = context->backend_specific;

    LLVMValueRef switch_val = NULL;
    JumpTarget* current = llvm->jump_target;
    while (current != NULL)
    {
        // We use this instead of checking for default since it could be null
        if (current->current_switch != NULL)
        {
            return current->switch_default;
        }

        current = current->previous;
    }

    // TODO: panic here? (since this should be an error maybe?)
    return NULL;
}
