#include "expression.h"

ExpressionType expression_kind(const Expression* expr)
{
    return expr->base.kind;
}

QualifiedType expression_type(const Expression* expr)
{
    return expr->base.type;
}

bool expression_valid(const Expression* expr)
{
    return !expr->base.poisoned;
}

bool expression_invalid(const Expression* expr)
{
    return expr->base.poisoned;
}

bool expression_is(const Expression* expr, ExpressionType kind)
{
    return expression_kind(expr) == kind;
}


