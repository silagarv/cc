#ifndef AST_H
#define AST_H

#include "parse/scope.h"

#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"

// add some structure for the ast
typedef struct AST {
    

    Scope scope;
} AST;

#endif /* AST_H */
