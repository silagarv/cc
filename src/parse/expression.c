#include "expression.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "files/location.h"

#include "parse/ast_allocator.h"
#include "parse/literal_parser.h"
#include "parse/declaration.h"
#include "parse/type.h"

bool expression_is(const Expression* expr, ExpressionType type)
{
    if (expr == NULL)
    {
        return false;
    }

    return expr->base.kind == type;
}

Location expression_get_location(const Expression* expr)
{
    return (Location) 1;
}

ExpressionType expression_get_kind(const Expression* expr)
{
    if (expr == NULL)
    {
        return EXPRESSION_ERROR;
    }

    return expr->base.kind;        
}

bool expression_is_invalid(const Expression* expr)
{
    if (expr == NULL)
    {
        return true;
    }

    return expr->base.poisoned;
}

void expression_set_invalid(Expression* expr)
{
    expr->base.poisoned = true;
}

QualifiedType expression_get_qualified_type(const Expression* expr)
{
    if (expr == NULL)
    {
        return (QualifiedType) {0};
    }

    return expr->base.type;
}

static Expression* expression_create_base(AstAllocator* allocator, size_t size,
        ExpressionType expression_type, QualifiedType type)
{
    Expression* expr = ast_allocator_alloc(allocator, size);
    expr->base.kind = expression_type;
    expr->base.type = type;
    expr->base.poisoned = false;

    return expr;
}

Expression* expression_create_error(AstAllocator* allocator,
        Type* error_type)
{
    Expression* expr = expression_create_base(allocator, sizeof(ExpressionError),
            EXPRESSION_ERROR,
            (QualifiedType) {TYPE_QUALIFIER_NONE, error_type});
    expr->base.poisoned = true;

    return expr;
}

Expression* expression_create_parenthesised(AstAllocator* allocator,
        Location lparen_loc, Location rparen_loc, Expression* inside)
{
    QualifiedType type = expression_get_qualified_type(inside);
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionParenthesised), EXPRESSION_PARENTHESISED,
            type);
    expr->parenthesised.lparen_loc = lparen_loc;
    expr->parenthesised.rparen_loc = rparen_loc;
    expr->parenthesised.inside = inside;

    return expr;
}

Expression* expression_parenthesised_get_inner(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_PARENTHESISED));

    return expr->parenthesised.inside;
}

Expression* expression_create_reference(AstAllocator* allocator,
        Identifier* identifier, Location location,
        union Declaration* declaration, QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionReference), EXPRESSION_REFERENCE, expr_type);
    expr->reference.identifier = identifier;
    expr->reference.identifier_loc = location;
    expr->reference.declaration = declaration;

    return expr;
}

Expression* expression_create_integer(AstAllocator* allocator,
        Location location, IntegerValue value, QualifiedType type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionInteger), EXPRESSION_INTEGER_CONSTANT, type);
    expr->integer.value = value;
    expr->integer.num_location = location;

    return expr;
}

Expression* expression_create_float(AstAllocator* allocator, Location location,
        FloatingValue value, QualifiedType type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionInteger), EXPRESSION_FLOATING_CONSTANT, type);
    expr->floating.num_location = location;
    expr->floating.value = value;

    return expr;
}

Expression* expression_create_character(AstAllocator* allocator,
        Location location, CharValue value, QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionCharacter), EXPRESSION_CHARACTER_CONSTANT,
            expr_type);
    expr->character.value = value;

    return expr;
}

Expression* expression_create_array(AstAllocator* allocator, 
        Location lbracket_loc, Location rbracket_loc, Expression* lhs,
        Expression* member, QualifiedType expr_type, bool lhs_is_array)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionArrayAccess), EXPRESSION_ARRAY_ACCESS, expr_type);
    expr->array.lbracket_loc = lbracket_loc;
    expr->array.rbracket_loc = rbracket_loc;
    expr->array.lhs = lhs;
    expr->array.member = member;
    expr->array.lhs_is_array = lhs_is_array;

    return expr;
}

Expression* expression_create_unary(AstAllocator* allocator, 
        ExpressionType type, Location op_loc, Expression* expression,
        QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionUnary), type, expr_type);
    expr->unary.op_loc = op_loc;
    expr->unary.rhs = expression;
    
    return expr;
}

Expression* expression_create_binary(AstAllocator* allocator, 
        ExpressionType type, Location op_loc, Expression* lhs, Expression* rhs,
        QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionUnary), type, expr_type);
    expr->binary.lhs = lhs;
    expr->binary.op_loc = op_loc;
    expr->binary.rhs = rhs;
            
    return expr;
}

Expression* expression_create_member_access(AstAllocator* allocator,
        Location op_loc, Expression* lhs, Declaration* member,
        QualifiedType expr_type, bool dot)
{
    ExpressionType type = dot
            ? EXPRESSION_MEMBER_ACCESS
            : EXPRESSION_MEMBER_POINTER_ACCESS;
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionMemberAccess), type, expr_type);
    expr->access.lhs = lhs;
    expr->access.member = member;
    expr->access.location_op = op_loc;
    expr->access.is_arrow = !dot;

    return expr;
}

Expression* expression_create_cast(AstAllocator* allocator, Location lparen_loc,
        QualifiedType type, Location rparen_loc, Expression* rhs)
{
    Expression* expr = expression_create_base(allocator, sizeof(ExpressionCast),
            EXPRESSION_CAST, type);
    expr->cast.lparen_loc = lparen_loc;
    expr->cast.rparen_loc = rparen_loc;
    expr->cast.rhs = rhs;
    expr->cast.implicit = false;

    return expr;
}

Expression* expression_create_implicit_cast(AstAllocator* allocator,
        QualifiedType cast_to, Expression* expression)
{
    Expression* expr = expression_create_base(allocator, sizeof(ExpressionCast),
            EXPRESSION_CAST_IMPLICIT, cast_to);
    expr->cast.lparen_loc = LOCATION_INVALID;
    expr->cast.rparen_loc = LOCATION_INVALID;
    expr->cast.rhs = expression;
    expr->cast.implicit = true;

    return expr;
}
