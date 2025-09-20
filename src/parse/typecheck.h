#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "parse/expression.h"
#include "parse/statement.h"
#include "parse/declaration.h"

#include "parse/ast.h"

// Functions for type checking expressions, declarations, and statements
Expression* typecheck_expression(Ast* ast, Expression* expression);
Declaration* typecheck_declaration(Ast* ast, Declaration* expression);
Statement* typecheck_statement(Ast* ast, Statement* expression);


#endif /* TYPECHECK_H */
