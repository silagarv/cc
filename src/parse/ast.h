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
    DeclarationList top_level_decls; // All the top level decls
    DeclarationList external_decls; // All of the external decls
} Ast;

Ast ast_create(void);
void ast_delete(Ast* ast);

void ast_set_top_level_decls(Ast* ast, DeclarationList decls);
DeclarationList ast_get_top_level_decls(const Ast* ast);

void ast_set_external_decls(Ast* ast, DeclarationList decls);
DeclarationList ast_get_external_decls(const Ast* ast);

#endif /* AST_H */
