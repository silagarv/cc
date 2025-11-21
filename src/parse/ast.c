#include "ast.h"
#include "parse/ast_allocator.h"
#include "parse/declaration.h"
#include "parse/type.h"

Ast ast_create(void)
{
    AstAllocator allocator = ast_allocator_create();

    Ast ast = (Ast)
    {
        .ast_allocator = allocator,
        .base_types = type_builtins_initialise(&allocator),
        .top_level_decls = declaration_vector_create(1)
    };

    return ast;
}

void ast_delete(Ast* ast)
{
    declaration_vector_free(&ast->top_level_decls, NULL);
    ast_allocator_delete(&ast->ast_allocator);
}

