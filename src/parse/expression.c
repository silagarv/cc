#include "expression.h"

#include <stddef.h>
#include <string.h>

#include "files/location.h"
#include "parse/ast_allocator.h"
#include "parse/type.h"

static Expression* expression_create_base(AstAllocator* allocator, size_t size,
        ExpressionType expression_type)
{
    Expression* expr = ast_allocator_alloc(allocator, size);
    expr->base.kind = expression_type;



    return expr;
}

Expression* expression_create_error(AstAllocator* allocator)
{
    return expression_create_base(allocator, sizeof(ExpressionError),
            EXPRESSION_ERROR);
}

Expression* expression_create_array(AstAllocator* allocator,
        Location lbracket_loc, Location rbracket_loc, Expression* lhs,
        Expression* member)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionArrayAccess), EXPRESSION_ARRAY_ACCESS);
    expr->array.lbracket_loc = lbracket_loc;
    expr->array.rbracket_loc = rbracket_loc;
    expr->array.lhs = lhs;
    expr->array.member = member;

    return expr;
}

Expression* expression_create_unary(AstAllocator* allocator,
        ExpressionType type, Location op_loc, Expression* expression)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionUnary), type);
    expr->unary.op_loc = op_loc;
    expr->unary.rhs = expr;

    return expr;
}

Expression* expression_create_binary(AstAllocator* allocator,
        ExpressionType type, Location op_loc, Expression* lhs, Expression* rhs)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionBinary), type);
    expr->binary.op_loc = op_loc;
    expr->binary.lhs = lhs;
    expr->binary.rhs = rhs;

    return expr;
}

Expression* expression_create_parenthesised(AstAllocator* allocator,
        Location lparen_loc, Location rparen_loc, Expression* inside)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionParenthesised), EXPRESSION_PARENTHESISED);
    expr->parenthesised.lparen_loc = lparen_loc;
    expr->parenthesised.rparen_loc = rparen_loc;
    expr->parenthesised.inside = inside;

    return expr;
}


