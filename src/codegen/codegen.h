#ifndef GEN_H
#define GEN_H

#include "parse/declaration.h"
#include "parse/ast.h"

typedef struct CodegenState {
    // A pointer to the input ast for codegen which is needed for all codegen
    // types.
    const Ast* ast;

    // The current external that we are generating for
    Declaration* current_external;

    // Other generic data that we want to use for codegen
    void* data;
} CodegenState;

// Functions to create and delete the current code generation context.
CodegenState codegen_state_initialize(const Ast* ast);
void codegen_state_delete(CodegenState* state);

void codegen_translation_unit(const Ast* ast);

#endif /* GEN_H */
