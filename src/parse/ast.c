#include "ast.h"

#include "parse/ast_allocator.h"
#include "parse/declaration.h"
#include "parse/type.h"

#include <assert.h>

Ast ast_create(void)
{
    AstAllocator allocator = ast_allocator_create();
    TypeBuiltins builtins = type_builtins_initialise(&allocator);

    Ast ast = (Ast)
    {
        .ast_allocator = allocator,

        .base_types = builtins,
        .t_size = qualified_type_from(builtins.t_unsigned_long),
        .t_wchar = qualified_type_from(builtins.t_int),
        .t_ptrdiff_t = qualified_type_from(builtins.t_long),

        .__builtin_va_list = NULL,

        .external_decls = {0}, // This is given later
        .top_level_decls = {0} // This is given later
    };

    return ast;
}

void ast_delete(Ast* ast)
{
    ast_allocator_delete(&ast->ast_allocator);
}

AstAllocator* ast_get_allocator(const Ast* ast)
{
    return (AstAllocator*) &ast->ast_allocator;
}

const TypeBuiltins* ast_get_builtins(const Ast* ast)
{
    return &ast->base_types;
}

QualifiedType ast_get_size_type(const Ast* ast)
{
    return ast->t_size;
}

QualifiedType ast_get_wchar_type(const Ast* ast)
{
    return ast->t_wchar;
}

QualifiedType ast_get_ptrdiff_type(const Ast* ast)
{
    return ast->t_ptrdiff_t;
}

Declaration* ast_get___builtin_va_list(const Ast* ast)
{
    return ast->__builtin_va_list;
}

DeclarationList ast_get_top_level_decls(const Ast* ast)
{
    return ast->top_level_decls;
}

DeclarationList ast_get_external_decls(const Ast* ast)
{
    return ast->external_decls;
}

void ast_set_top_level_decls(Ast* ast, DeclarationList decls)
{
    ast->top_level_decls = decls;
}

void ast_set_external_decls(Ast* ast, DeclarationList decls)
{
    ast->external_decls = decls;
}

void ast_set___builtin_va_list(Ast* ast, Declaration* builtin_va_list)
{
    assert(ast->__builtin_va_list == NULL);
    ast->__builtin_va_list = builtin_va_list;
}

