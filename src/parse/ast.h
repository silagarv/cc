#ifndef AST_H
#define AST_H

#include <stddef.h>

#include "lex/identifier_table.h"

#include "parse/ast_allocator.h"
#include "parse/type.h"
#include "parse/declaration.h"

// This represents the abstract syntax tree for a translation unit.
typedef struct Ast {
    // The allocator that is going to be used for the entire ast so that it can
    // all be free in one go.
    AstAllocator ast_allocator;

    // A pointer to the identifier table so that we are able to create builtin
    // declarations as required.
    IdentifierTable* ids;

    // All of our builtin types that we are going to use for semantic analysis.
    TypeBuiltins base_types;
    QualifiedType t_size; // For the return type of sizeof expression
    QualifiedType t_wchar; // For the non-builtin but helpful wchar type
    QualifiedType t_ptrdiff_t; // The type for the difference in pointers

    // The builtin declarations that we are going to use. These are not created
    // when the ast is created, rather are installed once the SemanticChecker
    // is created with the parser.
    Declaration* __builtin_va_list;
    // TODO: more builtins (might need some for __int128_t?)

    // All of our top level declarations that we have. These are all of our
    // file level decls even if they wouldn't appear in the final compiler 
    // binary
    DeclarationList top_level_decls;

    // All of the external decls. For the purposes of this the external decls
    // also include our statics too since, we will need to do code generation
    // for those as well.
    DeclarationList external_decls;
} Ast;

Ast ast_create(void);
void ast_delete(Ast* ast);

void ast_set_top_level_decls(Ast* ast, DeclarationList decls);
void ast_set_external_decls(Ast* ast, DeclarationList decls);
void ast_set___builtin_va_list(Ast* ast, Declaration* builtin_va_list);

AstAllocator* ast_get_allocator(const Ast* ast);
const TypeBuiltins* ast_get_builtins(const Ast* ast);
QualifiedType ast_get_size_type(const Ast* ast);
QualifiedType ast_get_wchar_type(const Ast* ast);
QualifiedType ast_get_ptrdiff_type(const Ast* ast);
Declaration* ast_get___builtin_va_list(const Ast* ast);
DeclarationList ast_get_top_level_decls(const Ast* ast);
DeclarationList ast_get_external_decls(const Ast* ast);

#endif /* AST_H */
