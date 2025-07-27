#ifndef AST_H
#define AST_H

#include "util/arena.h"

#include "parse/scope.h"

#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"
#include <stddef.h>

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

    Declaration* declarations;
    size_t num_declarations;
    size_t cap_declarations;
} Ast;

#endif /* AST_H */
