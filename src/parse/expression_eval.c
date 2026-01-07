#include "expression_eval.h"

#include <stdint.h>

#include "parse/expression.h"
#include "parse/type.h"

int64_t expression_integer_value_get(const ExpressionIntegerValue* value)
{
    return value->value;
}

bool expression_integer_value_valid(const ExpressionIntegerValue* value)
{
    return value->valid;
}

bool expression_is_integer_constant(const Expression* expression)
{
    // Don't both to deal with invalid expressions. This will just slow us down
    if (expression_is_invalid(expression))
    {
        return false;
    }

    // Switch on the expression type.
    switch (expression_get_kind(expression))
    {
        // These base expressions will always all have integer values. All of 
        // the other types of expressions we will need to do some checking to
        // determine if they have an integer constant.
        case EXPRESSION_INTEGER_CONSTANT:
        case EXPRESSION_ENUMERATION_CONSTANT:
        case EXPRESSION_CHARACTER_CONSTANT:
        case EXPRESSION_SIZEOF_TYPE: // Types will always evaluate to a constant
            return true;

        case EXPRESSION_SIZEOF_EXPRESSION: // if variable will not evaluate
        {
            panic("TODO");
            return false;
        }

        case EXPRESSION_CAST:
        {
            // The type that it is cast to is the type of the cast expression
            QualifiedType type = expression_get_qualified_type(expression);
            if (!qualified_type_is_integer(&type))
            {
                return false;
            }

            // Get the inner expression with no parenthesis
            Expression* castee = expression_cast_get_inner(expression);
            castee = expression_ignore_parenthesis(castee);

            QualifiedType castee_type = expression_get_qualified_type(castee);
            if (!qualified_type_is_arithmetic(&castee_type))
            {
                return false;
            }

            // Floating constants only valid as IMMEDIATE operand of cast 
            // expression.
            if (expression_is(castee, EXPRESSION_FLOATING_CONSTANT))
            {
                return true;
            }

            // Otherwise just check if the castee is an integer constant 
            // expression
            return expression_is_integer_constant(castee);
        }

        // Get the answer for the inner expression
        case EXPRESSION_PARENTHESISED:
        {
            Expression* inner = expression_parenthesised_get_innermost(
                    expression);
            return expression_is_integer_constant(inner);
        }

        case EXPRESSION_UNARY_PLUS:
        case EXPRESSION_UNARY_MINUS:
        case EXPRESSION_UNARY_BIT_NOT:
        case EXPRESSION_UNARY_NOT:
        {
            Expression* rhs = expression_unary_get_rhs(expression);
            return expression_is_integer_constant(rhs);
        }
        
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
        {
            Expression* lhs = expression_binary_get_lhs(expression);
            Expression* rhs = expression_binary_get_rhs(expression);

            return expression_is_integer_constant(lhs)
                    && expression_is_integer_constant(rhs);
        }
        
        case EXPRESSION_CONDITIONAL:
        {
            Expression* cond = expression_conditional_get_cond(expression);
            Expression* t = expression_conditional_get_true(expression);
            Expression* f = expression_conditional_get_false(expression);

            return expression_is_integer_constant(cond)
                    && expression_is_integer_constant(t)
                    && expression_is_integer_constant(f);
        }

        // Definitely never be ICE's
        case EXPRESSION_FLOATING_CONSTANT:
        case EXPRESSION_STRING_LITERAL:
        case EXPRESSION_ARRAY_ACCESS:
        case EXPRESSION_FUNCTION_CALL:
        case EXPRESSION_MEMBER_ACCESS:
        case EXPRESSION_MEMBER_POINTER_ACCESS:

        case EXPRESSION_COMPOUND_LITERAL:
        case EXPRESSION_UNARY_ADDRESS:
        case EXPRESSION_UNARY_DEREFERENCE:

        case EXPRESSION_UNARY_PRE_INCREMENT:
        case EXPRESSION_UNARY_PRE_DECREMENT:
        case EXPRESSION_UNARY_POST_INCREMENT:
        case EXPRESSION_UNARY_POST_DECREMENT:

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
            return false;

        // These are all definitely not ICE's
        case EXPRESSION_CAST_IMPLICIT:
        case EXPRESSION_ARRAY_DECAY:
        case EXPRESSION_LVALUE_CAST:
        case EXPRESSION_REFERENCE:
            return false;

        case EXPRESSION_ERROR:
        default:
            panic("expression kind not handled!");
            return false;
    }

    return false;
}

// An internal struct to help us evaluate an expression. Stores state and our
// current diagnostic manage in the event that something goes quite wrong.
typedef struct ExpressionEvalulator {
    DiagnosticManager* dm;
    const Expression* expression;
    ExpressionIntegerValue value;
} ExpressionEvalulator;



