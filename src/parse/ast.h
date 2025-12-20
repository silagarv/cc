#ifndef AST_H
#define AST_H

#include <stddef.h>

#include "parse/ast_allocator.h"
#include "parse/type.h"
#include "parse/declaration.h"

// This represents the abstract syntax tree for a translation unit.
typedef struct Ast {
    AstAllocator ast_allocator;
    TypeBuiltins base_types;
    QualifiedType size_type; // For the return type of sizeof expression
    DeclarationVector top_level_decls;
} Ast;

Ast ast_create(void);
void ast_delete(Ast* ast);

#endif /* AST_H */
