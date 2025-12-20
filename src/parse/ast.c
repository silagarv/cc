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

    // Set the size type to be unsigned long long
    // TODO: this might be target dependant.
    ast.size_type = (QualifiedType)
    {
        .qualifiers = QUALIFIER_NONE,
        .type = ast.base_types.type_unsigned_long
    };

    return ast;
}

void ast_delete(Ast* ast)
{
    declaration_vector_free(&ast->top_level_decls, NULL);
    ast_allocator_delete(&ast->ast_allocator);
}

