#ifndef GEN_H
#define GEN_H

#include "parse/declaration.h"
#include "parse/ast.h"

typedef struct CodegenState {
    // A pointer to the input ast for codegen
    const Ast* ast;

    // The current external that we are generating for
    Declaration* current_external;

    // TODO: what else do we need? possibly switches and that kind of thing too?
} CodegenState;

void codegen_translation_unit(const Ast* ast);

#endif /* GEN_H */
