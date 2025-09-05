#ifndef AST_H
#define AST_H

#include <stddef.h>

#include "util/arena.h"

#include "parse/scope.h"

#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"

typedef struct AstAllocator {
    Arena statements;
    Arena declarations;
    Arena expressions;
    Arena initializers;
} AstAllocator;

// this struct contains all of the current context information e.g. current
// switch, if, else, etc...
typedef struct AstContext {
    void* current_function; // the current function we are parsing

    Statement* current_iteration; // for giving continue a top level stmt
    Statement* current_breakable; // for giving switch, for, do while, and while breaks'
    Statement* current_switch; // the current switch statement.
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
