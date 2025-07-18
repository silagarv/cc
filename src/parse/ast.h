#ifndef AST_H
#define AST_H

#include "util/arena.h"

#include "parse/scope.h"

#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"

// this struct contains all of the current context information e.g. current
// switch, if, else, etc...
typedef struct AstContext {
    void* current_function;

    void* current_if;
} AstContext;

// add some structure for the ast
typedef struct Ast {
    Scope scope;

    Arena ast_allocator;
} Ast;

Type* ast_allocate_type(Ast* ast);
Expression* ast_allocate_expresssion(Ast* ast);
Statement* ast_allocate_statement(Ast* ast);

#endif /* AST_H */
