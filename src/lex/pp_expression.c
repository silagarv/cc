#include "pp_expression.h"

#include <stdint.h>
#include <assert.h>

#include "driver/diagnostic.h"

#include "files/location.h"
#include "lex/identifier_table.h"
#include "lex/macro_map.h"
#include "lex/token.h"
#include "lex/preprocessor.h"

#include "parse/literal_parser.h"

typedef enum Precedance {
    PRECEDENCE_INVALID = -1,
    PRECEDENCE_NONE,
    PRECEDENCE_CONDITIONAL,
    PRECEDENCE_LOGICAL_OR,
    PRECEDENCE_LOGICAL_AND,
    PRECEDENCE_BITWISE_OR,
    PRECEDENCE_BITWISE_XOR,
    PRECEDENCE_BITWISE_AND,
    PRECEDENCE_EQUALITY,
    PRECEDENCE_RELATIONAL,
    PRECEDENCE_SHIFT,
    PRECEDENCE_ADDITION,
    PRECEDENCE_MULTIPLICATION
} Precedance;

int64_t pp_value_get_value(const PPValue* value)
{
    return value->value;
}

bool pp_value_get_success(const PPValue* value)
{
    return value->success;
}

bool preprocessor_parse_comma_expression(Preprocessor* pp, Token* token,
        PPValue* value);
bool preprocessor_parse_subexpression(Preprocessor* pp, Token* token,
        PPValue* value, Precedance prec);

bool preprocessor_parse_defined(Preprocessor* pp, Token* token, PPValue* value)
{
    // We have 2 potential forms to handles here.
    // 1. defined <NAME>
    // 2. defined ( <NAME> )

    // Get the next unexpanded token, it is either the name or an lparen
    preprocessor_next_unexpanded_token(pp, token);

    // If we have an lparen get the next unexpanded token again
    bool lparen = false;
    if (token_is_type(token, TOK_LPAREN))
    {
        lparen = true;
        preprocessor_next_unexpanded_token(pp, token);
    }

    // Now we should check if we have an identifier or not
    if (!token_is_identifier_like(token))
    {
        diagnostic_error_at(pp->dm, token_get_location(token), "macro name "
                "must be an identifier");
        value->success = false;
        return false;
    }

    // Now we need to get the macro name and determine the value of the 
    // expression.
    Identifier* name = token_get_identifier(token);
    bool defined = macro_map_is_defined(preprocessor_macro_map(pp), name);

    // Then advance the token and handle if we need to eat an lparen
    preprocessor_next_unexpanded_token(pp, token);

    // Then if we had an lparen before we need to make sure this is a right 
    // paren otherwise we should error.
    if (lparen)
    {
        if (!token_is_type(token, TOK_RPAREN))
        {
            diagnostic_error_at(pp->dm, token_get_location(token),
                    "missing ')' after 'defined'");
            value->success = false;
            return false;
        }

        preprocessor_next_unexpanded_token(pp, token);
    }

    value->value = (int64_t) defined;
    value->success = true;
    return true;
}

bool preprocessor_parse_primary_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{
    switch (token_get_type(token))
    {
        case TOK_LPAREN:
            // Eat the '('
            preprocessor_advance_token(pp, token);

            // Then try to parse a primary expression
            if (!preprocessor_parse_comma_expression(pp, token, value))
            {
                value->success = false;
                break;
            }

            // Then try to eat the ')'
            if (!token_is_type(token, TOK_RPAREN))
            {
                diagnostic_error_at(pp->dm, token_get_location(token),
                        "expected ')' in preprocessor expression");
                value->success = false;
                break;
            }

            // Eat the ')'
            preprocessor_advance_token(pp, token);
            assert(value->success && "not successful??");
            break;

        case TOK_NUMBER:
        {
            // First parse the number in general and then we will handle if we
            // have a floating point number or if we have an integer constant
            LiteralValue int_value;
            if (!parse_preprocessing_number(&int_value, pp->dm, pp->sm,
                    pp->lang, *token))
            {
                value->success = false;
                break;
            }

            // Make sure we don't have a floating value
            if (literal_value_get_type(&int_value) == VALUE_FLOATING_TYPE)
            {
                diagnostic_error_at(pp->dm, token_get_location(token),
                        "floating point literal in preprocessor expression");
                value->success = false;
                break;
            }

            // Eat the number
            preprocessor_advance_token(pp, token);
            
            // Now extract the value from the value
            value->value = (int64_t) int_value.value.integer.value;
            value->success = true;
            break;
        }

        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
        {
            CharValue char_value;
            if (!parse_char_literal(&char_value, pp->dm, pp->sm, pp->lang,
                    *token))
            {
                value->success = false;
                break;
            }
            
            // Eat the character
            preprocessor_advance_token(pp, token);

            // Give the char lieral the value of itself
            value->value = (int64_t) char_value_get_value(&char_value);
            value->success = true;
            break;
        }

        case TOK_IDENTIFIER:
        {
            Identifier* id = token_get_identifier(token);
            if (identifier_get_pp_keyword(id) == TOK_PP_defined)
            {
                return preprocessor_parse_defined(pp, token, value);
            }

            // Eat the identifier
            preprocessor_advance_token(pp, token);
            
            // Otherwise unknown identifiers have the value of 0
            value->value = 0;
            value->success = true;
            break;
        }

        default:
            diagnostic_error_at(pp->dm, token_get_location(token), "expected "
                    "value in expression");
            value->success = false;
            break;
    }

    return value->success;
}

void preprocessor_compute_unary_expression_value(Preprocessor* pp,
        TokenType type, PPValue* value)
{
    assert(value->success == true && "sad value?");

    switch (type)
    {
        case TOK_PLUS:
            return;

        case TOK_MINUS:
            value->value = -value->value;
            return;

        case TOK_NOT:
            value->value = !value->value;
            return;

        case TOK_TILDE:
            value->value = ~value->value;
            return;

        default:
            panic("invalid type");
            return;
    }
}

bool preprocessor_parse_unary_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{
    switch (token_get_type(token))
    {
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_NOT:
        case TOK_TILDE:
        {
            // Track the token type so that we can do the calculation and 
            // advance the stream so that we can parse the primary part
            TokenType type = token_get_type(token);
            preprocessor_advance_token(pp, token);

            // If parsing a primary expression fails, we should just error
            if (!preprocessor_parse_primary_expression(pp, token, value))
            {
                return false;
            }
            
            // Otherwise compute the value of the expression
            preprocessor_compute_unary_expression_value(pp, type, value);
            return true;
        }

        // Otherwise just attempt to parse a primary expresison as one of our
        // unary expressions.
        default:
            return preprocessor_parse_primary_expression(pp, token, value);
    }
}

Precedance preprocessor_get_token_precedance(TokenType type)
{
    switch (type)
    {
        case TOK_PP_EOD:
            return PRECEDENCE_NONE;
            
        case TOK_OR_OR:
            return PRECEDENCE_LOGICAL_OR;

        case TOK_AND_AND:
            return PRECEDENCE_LOGICAL_AND;

        case TOK_OR:
            return PRECEDENCE_BITWISE_OR;

        case TOK_XOR:
            return PRECEDENCE_BITWISE_XOR;

        case TOK_AND:
            return PRECEDENCE_BITWISE_AND;

        case TOK_EQUAL_EQUAL:
        case TOK_NOT_EQUAL:
            return PRECEDENCE_EQUALITY;

        case TOK_LT:
        case TOK_LT_EQUAL:
        case TOK_GT:
        case TOK_GT_EQUAL:
            return PRECEDENCE_RELATIONAL;

        case TOK_LT_LT:
        case TOK_GT_GT:
            return PRECEDENCE_SHIFT;

        case TOK_PLUS:
        case TOK_MINUS:
            return PRECEDENCE_ADDITION;

        case TOK_STAR:
        case TOK_SLASH:
        case TOK_PERCENT:
            return PRECEDENCE_MULTIPLICATION;

        default:
            return PRECEDENCE_INVALID;
    }
}

bool preprocessor_compute_binary_expression_value(Preprocessor* pp,
        TokenType type, Location location, PPValue* lhs, PPValue* rhs)
{
    assert(lhs->success && rhs->success && "both sides should be okay");

    switch (type)
    {
        case TOK_OR_OR:
            lhs->value = lhs->value || rhs->value;
            return true;

        case TOK_AND_AND:
            lhs->value = lhs->value && rhs->value;
            return true;

        case TOK_OR:
            lhs->value = lhs->value | rhs->value;
            return true;

        case TOK_XOR:
            lhs->value = lhs->value ^ rhs->value;
            return true;

        case TOK_AND:
            lhs->value = lhs->value & rhs->value;
            return true;

        case TOK_EQUAL_EQUAL:
            lhs->value = lhs->value == rhs->value;
            return true;

        case TOK_NOT_EQUAL:
            lhs->value = lhs->value != rhs->value;
            return true;

        case TOK_LT:
            lhs->value = lhs->value < rhs->value;
            return true;

        case TOK_LT_EQUAL:
            lhs->value = lhs->value <= rhs->value;
            return true;

        case TOK_GT:
            lhs->value = lhs->value > rhs->value;
            return true;

        case TOK_GT_EQUAL:
            lhs->value = lhs->value >= rhs->value;
            return true;

        case TOK_LT_LT:
            lhs->value = lhs->value << rhs->value;
            return true;

        case TOK_GT_GT:
            lhs->value = lhs->value >> rhs->value;
            return true;

        case TOK_PLUS:
            lhs->value = lhs->value + rhs->value;
            return true;

        case TOK_MINUS:
            lhs->value = lhs->value - rhs->value;
            return true;

        case TOK_STAR:
            lhs->value = lhs->value * rhs->value;
            return true;

        case TOK_SLASH:
            if (rhs->value == 0)
            {
                diagnostic_error_at(pp->dm, location, "division by 0 in "
                        "preprocessor expression");
                lhs->success = false;
                return false;
            }

            lhs->value = lhs->value / rhs->value;
            return true;

        case TOK_PERCENT:
            if (rhs->value == 0)
            {
                diagnostic_error_at(pp->dm, location, "division by 0 in "
                        "preprocessor expression");
                lhs->success = false;
                return false;
            }

            lhs->value = lhs->value % rhs->value;
            return true;

        default:
            panic("invalid operator");
            return true;
    }
}

bool preprocessor_parse_subexpression(Preprocessor* pp, Token* token,
        PPValue* value, Precedance prec)
{
    // First try to parse a unary expression and if we fail that then we should
    // just return an error.
    if (!preprocessor_parse_unary_expression(pp, token, value))
    {
        assert(value->success == false && "should be successful");
        return false;
    }

    // Here we should have some kind of binary operator...
    while (!token_is_type(token, TOK_PP_EOD))
    {
        TokenType type = token_get_type(token);
        Precedance new_prec = preprocessor_get_token_precedance(type);

        // If the precedence we get is lower then we are done!
        if (new_prec < prec)
        {
            return value->success;
        }

        Location loc = token_get_location(token);

        // Eat the operator.
        preprocessor_advance_token(pp, token);

        // If the rhs of this subexpression fails to parse then we simply
        // just set this value to false and continue.
        PPValue rhs = {0};
        if (!preprocessor_parse_subexpression(pp, token, &rhs, new_prec))
        {
            value->success = false;
            return false;
        }

        // Otherwise calculate the value of this subexpression
        if (!preprocessor_compute_binary_expression_value(pp, type,
                loc, value, &rhs))
        {
            value->success = false;
            return false;
        }
    }

    return true;
}

bool preprocessor_parse_conditional_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{
    // Start parsing our subexpressions and parse up to a logical or expression.
    if (!preprocessor_parse_subexpression(pp, token, value,
            PRECEDENCE_LOGICAL_OR))
    {
        assert(value->success == false && "error but not set?");
        return false;
    }

    // Now check for a '?' to see if we got a conditional expression
    if (token_is_type(token, TOK_QUESTION))
    {
        // Eat the '?'
        preprocessor_advance_token(pp, token);

        // Parse the true part
        PPValue true_value = {0};
        if (!preprocessor_parse_comma_expression(pp, token, &true_value))
        {
            value->success = false;
            return false;
        }

        // Expect the ':' and eat if we have it
        if (!token_is_type(token, TOK_COLON))
        {
            diagnostic_error_at(pp->dm, token_get_location(token),
                    "expected ':'");
            value->success = false;
            return false;
        }

        preprocessor_advance_token(pp, token);

        PPValue false_value = {0};
        if (!preprocessor_parse_conditional_expression(pp, token, &false_value))
        {
            value->success = false;
            return false;
        }

        value->value = value->value ? true_value.value : false_value.value;
    }

    return true;
}

bool preprocessor_parse_comma_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{
    if (!preprocessor_parse_conditional_expression(pp, token, value))
    {
        value->success = false;
        return false;
    }

    if (!token_is_type(token, TOK_COMMA))
    {
        assert(value->success == true && "not sucessful?");
        return true;
    }

    while (token_is_type(token, TOK_COMMA))
    {
        preprocessor_advance_token(pp, token);

        if (!preprocessor_parse_conditional_expression(pp, token, value))
        {
            value->success = false;
            return false;
        }
    }

    return true;
}

bool preprocessor_parse_expression(Preprocessor* pp, Token* token,
        PPValue* value)
{
    // Parse the comma expresison.
    if (!preprocessor_parse_comma_expression(pp, token, value))
    {
        value->success = false;
        return false;
    }

    // Now we should check here for a missing binary operator and error about
    // the missing operator.
    if (!token_is_type(token, TOK_PP_EOD))
    {
        diagnostic_error_at(pp->dm, token_get_location(token), "token "
                "is not a valid binary operator in a preprocessor "
                "subexpression");
        value->success = false;            
        return false;
    }

    return true;
}
