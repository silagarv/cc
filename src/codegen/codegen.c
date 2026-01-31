#include "codegen.h"

#include "util/arena.h"

CodegenContext codegen_context_create(const Filepath* in, const Filepath* out,
        const Target* target, DiagnosticManager* dm,
        const CompilerOptions* options, const Ast* ast)
{
    // Create our code generation context completely giving it an arean to use
    // for any allocations that it might have to do.
    CodegenContext context = (CodegenContext)
    {
        .arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE, ARENA_DEFAULT_ALIGNMENT),
        .input_file = in,
        .output_file = out,
        .target = target,
        .dm = dm,
        .options = options,
        .ast = ast,
        .current_external = NULL,
        .codegen_okay = true,
        .backend_specific = NULL
    };

    return context;
}

void codegen_context_delete(CodegenContext* context)
{
    // TODO: do nothing since the arena will be placed in the codegen result...    
}

CodegenResult* codegen_translation_unit(const Filepath* in, const Filepath* out,
        const Target* target, DiagnosticManager* dm,
        const CompilerOptions* options, const Ast* ast,
        EmitTranslationUnitFunc emit)
{
    // Create our context, use our emitting function and then delete the 
    // context once we are done.
    CodegenContext context = codegen_context_create(in, out, target, dm,
            options, ast);
    CodegenResult* result = emit(&context);
    codegen_context_delete(&context);

    return result;
}

