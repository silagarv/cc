#ifndef AST_AST_H
#define AST_AST_H

#include "lex/identifier_table.h"

#include "ast/allocator.h"
#include "ast/type.h"
#include "ast/ast_fwd.h"

// TODO: should I use this???
typedef struct ASTResult {
    void* result;
} ASTResult;

ASTResult ast_result_from_expression(Expression* expr);
ASTResult ast_result_from_statement(Statement* expr);
ASTResult ast_result_from_declaration(Declaration* expr);
ASTResult ast_result_from_initializer(Initializer* expr);

bool ast_result_is_okay(const ASTResult* res);

Expression* ast_result_as_expression(const ASTResult* result);
Statement* ast_result_as_statement(const ASTResult* result);
Declaration* ast_result_as_declaration(const ASTResult* result);
Initializer* ast_result_as_initializer(const ASTResult* result);

typedef struct AST {
    // The allocator for this AST
    AstAllocator allocator;

    // The identifier table this AST uses in order to create implicit decls
    IdentifierTable* ids;

    // All of the builtin declarations for this AST
    // TypeBuiltins base;
    QualifiedType size;
    QualifiedType wchar;
    QualifiedType ptrdiff;

    Declaration* builtin_va_list;

    // The translation unit declaration for this AST
    Declaration* tu;
} AST;

AST ast_create(void);
void ast_delete(AST* ast);

#endif /* AST_AST_H */
