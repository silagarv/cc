#ifndef EXPRESSION_EVAL_H
#define EXPRESSION_EVAL_H

#include <stdint.h>

#include "driver/diagnostic.h"
#include "parse/expression.h"

// A struct which contains the specific information about how evaluating the 
// constant expression went, and if the current result is valid or not.
typedef struct ExpressionIntegerValue {
    int64_t value; // the value stored itself
} ExpressionIntegerValue;

ExpressionIntegerValue expression_integer_value_create_zero(void);

int64_t expression_integer_value_get(const ExpressionIntegerValue* value);

// TODO: add some functions for if the value is in int range etc...

// The function below tells us if the expression is an integer constant
// expression. Returns 'true' if it is an integer constant expression and
// 'false' otherwise.
bool expression_is_integer_constant(const Expression* expression);

// Fold the integer constant expression expression to the expression integer
// value given by 'value'. Returns 'false' if any errors occured e.g. division
// by 0, and 'true' otherwise.
bool expression_fold_to_integer_constant(const Expression* expression,
        ExpressionIntegerValue* value);

#endif /* EXPRESSION_EVAL_H */
