#include "expression.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "files/location.h"

#include "parse/ast_allocator.h"
#include "parse/literal_parser.h"
#include "parse/declaration.h"
#include "parse/type.h"

bool expression_type_is_assignment(ExpressionType type)
{
    switch (type)
    {
        case EXPRESSION_BINARY_ASSIGN:
        case EXPRESSION_BINARY_TIMES_ASSIGN:
        case EXPRESSION_BINARY_DIVIDE_ASSIGN:
        case EXPRESSION_BINARY_MODULO_ASSIGN:
        case EXPRESSION_BINARY_ADD_ASSIGN:
        case EXPRESSION_BINARY_SUBTRACT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN:
        case EXPRESSION_BINARY_AND_ASSIGN:
        case EXPRESSION_BINARY_XOR_ASSIGN:
        case EXPRESSION_BINARY_OR_ASSIGN:
            return true;

        default:
            return false;
    }
}

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
    switch (expr->base.kind)
    {
        case EXPRESSION_ERROR:
            return expr->error.location;

        case EXPRESSION_ARRAY_DECAY:
            return expression_get_location(expr->array_decay.inner);

        case EXPRESSION_LVALUE_CAST:
            return expression_get_location(expr->lvalue_cast.inner_expression);

        case EXPRESSION_REFERENCE:
            return expr->reference.identifier_loc;

        case EXPRESSION_ENUMERATION_CONSTANT:
            return expr->reference.identifier_loc;

        case EXPRESSION_INTEGER_CONSTANT:
            return expr->integer.num_location;
        
        case EXPRESSION_FLOATING_CONSTANT:
            return expr->floating.num_location;

        case EXPRESSION_CHARACTER_CONSTANT:
            return expr->character.location;

        case EXPRESSION_STRING_LITERAL:
            panic("need to handle string literals properly");
            return (Location) 1;

        case EXPRESSION_ARRAY_ACCESS:
            return expr->array.lbracket_loc;

        case EXPRESSION_FUNCTION_CALL:
            return expr->call.lparen_loc;

        case EXPRESSION_MEMBER_ACCESS:
        case EXPRESSION_MEMBER_POINTER_ACCESS:
            return expr->access.location_op;

        case EXPRESSION_COMPOUND_LITERAL:
            return expr->compound_literal.lparen_loc;

        case EXPRESSION_SIZEOF_TYPE:
            return expr->sizeof_type.sizeof_loc;

        case EXPRESSION_SIZEOF_EXPRESSION:
            return expr->sizeof_expression.sizeof_loc;

        case EXPRESSION_CAST:
            return expr->cast.lparen_loc;

        case EXPRESSION_CAST_IMPLICIT:
            return expression_get_location(expr->cast.rhs);

        case EXPRESSION_UNARY_ADDRESS:
        case EXPRESSION_UNARY_DEREFERENCE:
        case EXPRESSION_UNARY_PLUS:
        case EXPRESSION_UNARY_MINUS:
        case EXPRESSION_UNARY_BIT_NOT:
        case EXPRESSION_UNARY_NOT:
        case EXPRESSION_UNARY_PRE_INCREMENT:
        case EXPRESSION_UNARY_PRE_DECREMENT:
        case EXPRESSION_UNARY_POST_INCREMENT:
        case EXPRESSION_UNARY_POST_DECREMENT:
            return expr->unary.op_loc;

        case EXPRESSION_BINARY_TIMES:
        case EXPRESSION_BINARY_DIVIDE:
        case EXPRESSION_BINARY_MODULO:
        case EXPRESSION_BINARY_ADD:
        case EXPRESSION_BINARY_SUBTRACT:
        case EXPRESSION_BINARY_SHIFT_LEFT:
        case EXPRESSION_BINARY_SHIFT_RIGHT:
        case EXPRESSION_BINARY_LESS_THAN:
        case EXPRESSION_BINARY_GREATER_THAN:
        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
        case EXPRESSION_BINARY_EQUAL:
        case EXPRESSION_BINARY_NOT_EQUAL:
        case EXPRESSION_BINARY_AND:
        case EXPRESSION_BINARY_XOR:
        case EXPRESSION_BINARY_OR:
        case EXPRESSION_BINARY_LOGICAL_AND:
        case EXPRESSION_BINARY_LOGICAL_OR:
        case EXPRESSION_BINARY_ASSIGN:
        case EXPRESSION_BINARY_TIMES_ASSIGN:
        case EXPRESSION_BINARY_DIVIDE_ASSIGN:
        case EXPRESSION_BINARY_MODULO_ASSIGN:
        case EXPRESSION_BINARY_ADD_ASSIGN:
        case EXPRESSION_BINARY_SUBTRACT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN:
        case EXPRESSION_BINARY_AND_ASSIGN:
        case EXPRESSION_BINARY_XOR_ASSIGN:
        case EXPRESSION_BINARY_OR_ASSIGN:
        case EXPRESSION_COMMA:
            return expr->binary.op_loc;
        
        case EXPRESSION_CONDITIONAL:
            return expr->conditional.question;

        case EXPRESSION_PARENTHESISED:
            return expr->parenthesised.lparen_loc;

        default:
            return (Location) 1;
    }

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

bool expression_is_valid(const Expression* expr)
{
    if (expr == NULL)
    {
        return false;
    }

    return !expr->base.poisoned;
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
        Type* error_type, Location location)
{
    Expression* expr = expression_create_base(allocator, sizeof(ExpressionError),
            EXPRESSION_ERROR,
            (QualifiedType) {QUALIFIER_NONE, error_type});
    expr->base.poisoned = true;
    expr->error.location = location;

    return expr;
}

Expression* expression_create_lvalue_cast(AstAllocator* allocator,
        Expression* inner, QualifiedType new_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionLValueCast), EXPRESSION_LVALUE_CAST, new_type);
    expr->lvalue_cast.inner_expression = inner;

    return expr;
}

Expression* expression_lvalue_cast_get_inner(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_LVALUE_CAST));

    return expr->lvalue_cast.inner_expression;
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

Expression* expression_parenthesised_get_innermost(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_PARENTHESISED));

    Expression* inner = expression_parenthesised_get_inner(expr);

    // Keep going until we get to a non-innermost expression
    if (expression_is(inner, EXPRESSION_PARENTHESISED))
    {
        return expression_parenthesised_get_innermost(inner);
    }

    return inner;
}

Expression* expression_ignore_parenthesis(Expression* expr)
{
    if (!expression_is(expr, EXPRESSION_PARENTHESISED))
    {
        return expr;
    }

    return expression_parenthesised_get_inner(expr);
}

Expression* expression_create_enum_constant(AstAllocator* allocator,
        Identifier* identifier, Location location,
        union Declaration* declaration, QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionReference), EXPRESSION_ENUMERATION_CONSTANT,
            expr_type);
    expr->reference.identifier = identifier;
    expr->reference.identifier_loc = location;
    expr->reference.declaration = declaration;

    return expr;
}

int expression_enum_constant_get_value(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_ENUMERATION_CONSTANT));

    Declaration* decl = expr->reference.declaration;
    return declaration_enum_constant_get_value(decl);
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

union Declaration* expression_reference_get_decl(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_REFERENCE)
        || expression_is(expression, EXPRESSION_ENUMERATION_CONSTANT));

    return expression->reference.declaration;
}

Expression* expression_create_array_decay(AstAllocator* allocator,
        Expression* expression, QualifiedType new_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionArrayToPtr), EXPRESSION_ARRAY_DECAY, new_type);
    expr->array_decay.inner = expression;

    // make sure to set this new expression invalid if the old one was
    if (expression_is_invalid(expression))
    {
        expression_set_invalid(expr);
    }

    return expr;
}

Expression* expression_array_decay_get_inner(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_ARRAY_DECAY));

    return expr->array_decay.inner;
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

IntegerValue expression_integer_get_value(const Expression* expression)
{
    return expression->integer.value;
}

Expression* expression_create_float(AstAllocator* allocator, Location location,
        FloatingValue value, QualifiedType type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionFloating), EXPRESSION_FLOATING_CONSTANT, type);
    expr->floating.num_location = location;
    expr->floating.value = value;

    return expr;
}

FloatingValue expression_float_get_value(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_FLOATING_CONSTANT));

    return expression->floating.value;
}

Expression* expression_create_character(AstAllocator* allocator,
        Location location, CharValue value, QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionCharacter), EXPRESSION_CHARACTER_CONSTANT,
            expr_type);
    expr->character.value = value;
    expr->character.location = location;

    return expr;
}

CharValue expression_character_get_value(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_CHARACTER_CONSTANT));

    return expression->character.value;
}

Expression* expression_create_call(AstAllocator* allocator, Expression* lhs,
        Location lparen_loc, ExpressionList parms, Location rparen_loc,
        QualifiedType return_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionFunctionCall), EXPRESSION_FUNCTION_CALL,
            return_type);
    expr->call.lparen_loc = lparen_loc;
    expr->call.rparen_loc = rparen_loc;
    expr->call.lhs = lhs;
    expr->call.arguments = parms;
    expr->call.num_arguments = expression_list_num_expr(&parms);

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

Expression* expression_create_compound_literal(AstAllocator* allocator,
        Location lparen_loc, QualifiedType type, Location rparen_loc,
        union Initializer* initializer)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionCompoundLiteral), EXPRESSION_COMPOUND_LITERAL,
            type);
    expr->compound_literal.lparen_loc = lparen_loc;
    expr->compound_literal.rparen_loc = rparen_loc;
    expr->compound_literal.type = type;
    expr->compound_literal.initializer = initializer;

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

Expression* expression_unary_get_rhs(const Expression* expression)
{
    return expression->unary.rhs;
}

Expression* expression_create_sizeof_type(AstAllocator* allocator,
        Location sizeof_location, Location lparen_loc, QualifiedType type,
        Location rparen_loc, QualifiedType size_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionSizeofType), EXPRESSION_SIZEOF_TYPE, size_type);
    expr->sizeof_type.sizeof_loc = sizeof_location;
    expr->sizeof_type.lparen_loc = lparen_loc;
    expr->sizeof_type.rparen_loc = rparen_loc;
    expr->sizeof_type.target_type = type;

    return expr;
}

QualifiedType expression_sizeof_type_get_type(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_SIZEOF_TYPE));

    return expr->sizeof_type.target_type;
}

Expression* expression_create_sizeof_expression(AstAllocator* allocator,
        Location sizeof_location, Expression* expression,
        QualifiedType size_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionSizeofExpression), EXPRESSION_SIZEOF_EXPRESSION,
            size_type);
    expr->sizeof_expression.sizeof_loc = sizeof_location;
    expr->sizeof_expression.expression = expression;

    return expr;
}

Expression* expression_sizeof_expression_get_expression(
        const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_SIZEOF_EXPRESSION));

    return expr->sizeof_expression.expression;
}

Expression* expression_create_binary(AstAllocator* allocator, 
        ExpressionType type, Location op_loc, Expression* lhs, Expression* rhs,
        QualifiedType expr_type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionBinary), type, expr_type);
    expr->binary.lhs = lhs;
    expr->binary.op_loc = op_loc;
    expr->binary.rhs = rhs;
            
    return expr;
}

Expression* expression_binary_get_lhs(const Expression* expression)
{
    return expression->binary.lhs;
}

Expression* expression_binary_get_rhs(const Expression* expression)
{
    return expression->binary.rhs;
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

Expression* expression_member_access_get_lhs(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_MEMBER_ACCESS)
            || expression_is(expression, EXPRESSION_MEMBER_POINTER_ACCESS));

    return expression->access.lhs;
}

Expression* expression_member_access_get_most_lhs(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_MEMBER_ACCESS)
            || expression_is(expr, EXPRESSION_MEMBER_POINTER_ACCESS));

    // Get lhs, remove parens and recurse
    Expression* lhs = expression_member_access_get_lhs(expr);
    lhs = expression_ignore_parenthesis(lhs);
    if (expression_is(lhs, EXPRESSION_MEMBER_ACCESS)
            || expression_is(lhs, EXPRESSION_MEMBER_POINTER_ACCESS))
    {
        return expression_member_access_get_most_lhs(lhs);
    }

    return lhs;
}

union Declaration* expression_member_access_get_decl(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_MEMBER_ACCESS)
            || expression_is(expr, EXPRESSION_MEMBER_POINTER_ACCESS));

    return expr->access.member;
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

    if (expression_is_invalid(expression))
    {
        expr->base.poisoned = true;
    }

    return expr;
}

Expression* expression_cast_get_inner(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_CAST));

    return expression->cast.rhs;
}

Expression* expression_implicit_cast_get_inner(const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_CAST_IMPLICIT));

    return expression->cast.rhs;   
}

Expression* expression_create_conditional(AstAllocator* allocator,
        Expression* condition, Location question, Expression* true_expr,
        Location colon, Expression* false_expr, QualifiedType type)
{
    Expression* expr = expression_create_base(allocator,
            sizeof(ExpressionConditional), EXPRESSION_CONDITIONAL, type);
    expr->conditional.question = question;
    expr->conditional.colon = colon;
    expr->conditional.condition = condition;
    expr->conditional.true_part = true_expr;
    expr->conditional.false_part = false_expr;

    return expr;
}

Expression* expression_conditional_get_cond(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_CONDITIONAL));

    return expr->conditional.condition;
}

Expression* expression_conditional_get_true(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_CONDITIONAL));

    return expr->conditional.true_part;
}

Expression* expression_conditional_get_false(const Expression* expr)
{
    assert(expression_is(expr, EXPRESSION_CONDITIONAL));

    return expr->conditional.false_part;
}

ExpressionListEntry* expression_list_entry_next(ExpressionListEntry* entry)
{
    return entry->next;
}

Expression* expression_list_entry_get(ExpressionListEntry* entry)
{
    return entry->expression;
}

ExpressionList expression_list_create(void)
{
    return (ExpressionList) {NULL, NULL, 0};
}

ExpressionListEntry* expression_list_first(const ExpressionList* list)
{
    return list->first;
}

void expression_list_push(AstAllocator* allocator, ExpressionList* list,
        Expression* expr)
{
    ExpressionListEntry* entry = ast_allocator_alloc(allocator,
            sizeof(ExpressionListEntry));
    entry->next = NULL;
    entry->expression = expr;

    if (list->tail == NULL)
    {
        list->first = entry;
    }
    else
    {
        list->tail->next = entry;
        list->tail = entry;
    }

    list->tail = entry;

    list->num_exprs++;
}

size_t expression_list_num_expr(const ExpressionList* list)
{
    return list->num_exprs;
}
