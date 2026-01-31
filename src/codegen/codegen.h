#ifndef GEN_H
#define GEN_H

#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/target.h"
#include "driver/options.h"

#include "files/filepath.h"

#include "parse/declaration.h"
#include "parse/ast.h"

typedef struct CodegenContext CodegenContext;
typedef struct CodegenResult CodegenResult;

// Declare the type of the function that we are going to use for emiting our
// translation unit.
typedef CodegenResult* (*EmitTranslationUnitFunc)(CodegenContext*);

// A base struct we can use for codegeneration that each back end can add on to.
struct CodegenContext {
    // The arena we are going to use in order to keep track of all of our code
    // generations objects and other things that we might need to enable us
    // to easily free all of the memory once we are done with it.
    Arena arena;

    // The name of the input and output filepaths (if needed)
    const Filepath* input_file;
    const Filepath* output_file;

    // Target information for the backend to use.
    const Target* target;

    // The diagnostic manager in the event we need to output diagnostics during
    // codegeneration. (maybe once we have inline asm we might need this).
    DiagnosticManager* dm;

    // A pointer to our compiler options so that we are able to better generate
    // code for this.
    const CompilerOptions* options;

    // A pointer to the input ast so that we are able to have something to 
    // traverse whilst we are generating code.
    const Ast* ast;

    // The current external that we are generating for.
    const Declaration* current_external;

    // TODO: add any other information that most (or all) backends might need
    // in order to perform their codegen

    // A bool to note if the codegen should succeed or fail
    bool codegen_okay;

    // The backend specific data that should be initialised by the backend when
    // the emit_function is called. Note, that this should be set to null on
    // initialization...
    void* backend_specific;
};

// Functions to create and delete the current code generation context.
// CodegenState codegen_state_initialize(const Ast* ast);
// void codegen_state_delete(CodegenState* state);
CodegenResult* codegen_translation_unit(const Filepath* in, const Filepath* out,
        const Target* target, DiagnosticManager* dm,
        const CompilerOptions* options, const Ast* ast,
        EmitTranslationUnitFunc emit);

#endif /* GEN_H */
