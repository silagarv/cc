#ifndef GEN_H
#define GEN_H

#include "driver/diagnostic.h"
#include "driver/target.h"
#include "driver/options.h"

#include "files/filepath.h"

#include "parse/declaration.h"
#include "parse/ast.h"

// TODO: should I just have some internal state that I use for different codegen
// like if i have multiple codegenerators in the future would this be smart?
struct CodegenStateInternal;

typedef struct CodegenContext {
    // The name of the input filepath
    const Filepath* input_file;

    // The target we are compiling for
    const Target* target;

    // The diagnostic manager in the event we need to output diagnostics during
    // codegeneration. (maybe once we have inline asm we need this)
    DiagnosticManager* dm;

    // A pointer to our compiler options so that we are able to better generate
    // code for this.
    const CompilerOptions* options;

    // A pointer to the input ast for codegen which is needed for all codegen
    // types.
    const Ast* ast;

    // The current external that we are generating for
    Declaration* current_external;

    // LLVM stuff???
    struct CodegenStateInternal* internal;
} CodegenContext;

// Functions to create and delete the current code generation context.
// CodegenState codegen_state_initialize(const Ast* ast);
// void codegen_state_delete(CodegenState* state);

void codegen_translation_unit(const Filepath* input_file, const Target* target,
        DiagnosticManager* dm, const CompilerOptions* options, const Ast* ast);

#endif /* GEN_H */
