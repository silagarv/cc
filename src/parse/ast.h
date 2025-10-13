#ifndef AST_H
#define AST_H

#include <stddef.h>

#include "parse/ast_allocator.h"
#include "parse/scope.h"
#include "parse/symbol.h"
#include "parse/type.h"
#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/initializer.h"
#include "parse/statement.h"

// this struct contains all of the current context information e.g. current
// switch, if, else, etc...
// TODO: I think that I would also like to put our current scope's and stuff in
// TODO: here since it would be nice and convenient to use for this.
typedef struct AstContext {
    Statement* current_iteration;
    Statement* current_breakable;
    Statement* current_switch;

    Scope file_scope;
} AstContext;

// This represents the abstract syntax tree for a translation unit.
typedef struct Ast {
    AstAllocator ast_allocator;

    TypeBuiltins base_types;

    DeclarationVector top_level_decls;
} Ast;

// Functions to push the current ast context and save it into the stack in a
// non-allocated variable (just a struct to pointers)
AstContext ast_context_push_for(AstContext* context, Statement* for_stmt);
AstContext ast_context_push_do_while(AstContext* context, Statement* do_stmt);
AstContext ast_context_push_while(AstContext* context, Statement* while_stmt);
AstContext ast_context_push_switch(AstContext* context, Statement* switch_stmt);

// Function to pop and restore the current ast context and store it back into
// variable ast context. From is the current variable and it saves the context
// back into that (from to)
void ast_context_pop(AstContext* context, AstContext old);

// Get the following information from the ast context
Statement* ast_context_current_iterable(const AstContext* context);
Statement* ast_context_current_breakable(const AstContext* context);
Statement* ast_context_current_switch(const AstContext* context);

Ast ast_create(void);
void ast_delete(Ast* ast);

#endif /* AST_H */
