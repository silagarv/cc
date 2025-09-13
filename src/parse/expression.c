#include "expression.h"

#include <stddef.h>
#include <string.h>

#include "files/location.h"
#include "parse/ast_allocator.h"
#include "parse/type.h"

static Expression* expression_create_base(AstAllocator* allocator, size_t size,
        ExpressionType expression_type, Type* type, LocationRange range)
{
    Expression* expr = ast_allocator_alloc(allocator, size);
    expr->base.kind = expression_type;
    expr->base.type = type;
    expr->base.range = range;

    return expr;
}

Expression* expression_create_error(AstAllocator* allocator,
        LocationRange range)
{
    return expression_create_base(allocator, sizeof(ExpressionError),
            EXPRESSION_ERROR, NULL, range);
}


