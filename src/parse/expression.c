#include "expression.h"

#include "util/panic.h"
#include "util/xmalloc.h"

#include "lex/token.h"
#include <string.h>

Expression* create_error_expression(const Token* token)
{
    Expression* expr = xmalloc(sizeof(Expression));
    expr->base = (ExpressionBase)
    {
        .kind = EXPRESSION_ERROR,
        .loc = token->loc,
        .type = NULL, // TODO: make sure the type is done properly
        .is_parenthesized = false
    };

    return expr;
}

Expression* create_integer_expression(const Token* token, IntegerValue* value)
{
    Expression* expr = xmalloc(sizeof(Expression));
    expr->integer.base = (ExpressionBase)
    {
        .kind = EXPRESSION_CHARACTER_CONSTANT,
        .loc = token->loc,
        .type = NULL, // TODO: make sure the type is done properly
        .is_parenthesized = false
    };
    expr->integer.value = *value;

    return expr;
}

Expression* create_character_expression(const Token* token, CharValue* value)
{
    Expression* expr = xmalloc(sizeof(Expression));
    expr->character.base = (ExpressionBase)
    {
        .kind = EXPRESSION_CHARACTER_CONSTANT,
        .loc = token->loc,
        .type = NULL, // TODO: make sure the type is done properly
        .is_parenthesized = false
    };
    expr->character.value = *value;

    return expr;
}

Expression* create_string_expression(const Token* token, StringLiteral* value)
{
    Expression* expr = xmalloc(sizeof(Expression));
    expr->string.base = (ExpressionBase)
    {
        .kind = EXPRESSION_CHARACTER_CONSTANT,
        .loc = token->loc,
        .type = NULL, // TODO: make sure the type is done properly
        .is_parenthesized = false
    };
    expr->string.value = *value;

    return expr;
}



