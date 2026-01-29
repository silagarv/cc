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
        .top_level_decls = {0}
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
    ast_allocator_delete(&ast->ast_allocator);
}

void ast_set_top_level_decls(Ast* ast, DeclarationList decls)
{
    ast->top_level_decls = decls;
}

DeclarationList ast_get_top_level_decls(const Ast* ast)
{
    return ast->top_level_decls;
}

void ast_set_external_decls(Ast* ast, DeclarationList decls)
{
    ast->external_decls = decls;
}

DeclarationList ast_get_external_decls(const Ast* ast)
{
    return ast->external_decls;
}

