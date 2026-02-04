#include "expression_eval.h"

#include <stdint.h>
#include <assert.h>

#include "driver/diagnostic.h"
#include "parse/declaration.h"
#include "parse/literal_parser.h"
#include "parse/type.h"
#include "parse/expression.h"
#include "util/panic.h"

ExpressionIntegerValue expression_integer_value_create_zero(void)
{
    return (ExpressionIntegerValue) {0};
}

int64_t expression_integer_value_get(const ExpressionIntegerValue* value)
{
    return value->value;
}

bool expression_is_integer_constant(const Expression* expression)
{
    // Don't both to deal with invalid expressions. This will just slow us down
    if (expression_is_invalid(expression))
    {
        return false;
    }

    // Switch on the expression type.
    ExpressionType kind = expression_get_kind(expression);
    switch (kind)
    {
        // These base expressions will always all have integer values. All of 
        // the other types of expressions we will need to do some checking to
        // determine if they have an integer constant.
        case EXPRESSION_INTEGER_CONSTANT:
        case EXPRESSION_ENUMERATION_CONSTANT:
        case EXPRESSION_CHARACTER_CONSTANT:
            return true;

        case EXPRESSION_SIZEOF_TYPE: // Types will always evaluate to a constant
        {
            QualifiedType type = expression_sizeof_type_get_type(expression);
            type = qualified_type_get_canonical(&type);

            if (!qualified_type_is_complete(&type))
            {
                return false;
            }

            return true;
        }

        case EXPRESSION_SIZEOF_EXPRESSION: // if variable will not evaluate
        {
            Expression* expr = expression_sizeof_expression_get_expression(
                    expression);
            if (expression_is_invalid(expr))
            {
                return false;
            }

            return true;
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
            if (expression_is_invalid(castee))
            {
                return false;
            }
        
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

        // TODO: check division and mod by 0 when computing if we have an 
        // TODO: integer constant expression.
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

            bool lhs_okay = expression_is_integer_constant(lhs);
            bool rhs_okay = expression_is_integer_constant(rhs);

            if (rhs_okay && (kind == EXPRESSION_BINARY_DIVIDE
                    || kind == EXPRESSION_BINARY_MODULO))
            {
                // Need to keep checking for errors as we are testing for a 
                // division by zero error.
                ExpressionIntegerValue value = {0};
                rhs_okay = expression_fold_to_integer_constant(rhs, &value);
                if (rhs_okay && value.value == 0)
                {
                    rhs_okay = false;
                }
            }
            
            return lhs_okay && rhs_okay;
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

        // Remove the implicit cast
        case EXPRESSION_CAST_IMPLICIT:
        {
            Expression* inner = expression_implicit_cast_get_inner(expression);
            return expression_is_integer_constant(inner);
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

bool expression_fold_to_integer_constant(const Expression* expression,
        ExpressionIntegerValue* value)
{
    assert(expression_is_integer_constant(expression));

    ExpressionType kind = expression_get_kind(expression);
    switch (kind)
    {
        case EXPRESSION_INTEGER_CONSTANT:
        {
            IntegerValue ival = expression_integer_get_value(expression);
            uint64_t original = integer_value_get_value(&ival);

            value->value = (int64_t) original;

            return true;
        }

        case EXPRESSION_ENUMERATION_CONSTANT:
        {
            Declaration* decl = expression_reference_get_decl(expression);
            int val = declaration_enum_constant_get_value(decl);

            value->value = (int64_t) val;
            
            return true;
        }
        case EXPRESSION_CHARACTER_CONSTANT:
        {
            CharValue char_val = expression_character_get_value(expression);
            uint64_t val = char_value_get_value(&char_val);

            value->value = (int64_t) val;

            return true;
        }

        case EXPRESSION_SIZEOF_TYPE:
        {
            QualifiedType type = expression_sizeof_type_get_type(expression);

            int64_t val = (int64_t) qualified_type_get_size(&type);

            value->value = val;

            return true;
        }

        case EXPRESSION_SIZEOF_EXPRESSION:
        {
            Expression* inner = expression_sizeof_expression_get_expression(
                    expression);
            QualifiedType type = expression_get_qualified_type(inner);

            int64_t val = (int64_t) qualified_type_get_size(&type);

            value->value = val;

            return true;
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
            // expression. So here we will perform a cast from the float to the
            // integer type.
            if (expression_is(castee, EXPRESSION_FLOATING_CONSTANT))
            {
                FloatingValue float_val = expression_float_get_value(castee);
                value->value = (int64_t) floating_value_get_value(&float_val);
                return true;
            }

            // Do an integer to integer cast.
            ExpressionIntegerValue rhs_value = {0};
            expression_fold_to_integer_constant(castee, &rhs_value);

            // TODO: we will need to actually perform the integer cast here!!!
            value->value = rhs_value.value;
            return true;
        }

        // Get the answer for the inner expression
        case EXPRESSION_PARENTHESISED:
        {
            Expression* inner = expression_parenthesised_get_innermost(
                    expression);
            return expression_fold_to_integer_constant(inner, value);
        }

        case EXPRESSION_UNARY_PLUS:
        case EXPRESSION_UNARY_MINUS:
        case EXPRESSION_UNARY_BIT_NOT:
        case EXPRESSION_UNARY_NOT:
        {
            Expression* rhs = expression_unary_get_rhs(expression);
            ExpressionIntegerValue rhs_value = {0};
            expression_fold_to_integer_constant(rhs, &rhs_value);

            switch (kind)
            {
                case EXPRESSION_UNARY_PLUS:
                    value->value = +rhs_value.value;
                    break;

                case EXPRESSION_UNARY_MINUS:
                    value->value = -rhs_value.value;
                    break;

                case EXPRESSION_UNARY_BIT_NOT:
                    value->value = ~rhs_value.value;
                    break;

                case EXPRESSION_UNARY_NOT:
                    value->value = !rhs_value.value;
                    break;

                default:
                    panic("bad unary expression kind");
                    return false;
            }

            return true;
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
            ExpressionIntegerValue lhs_value = {0};
            expression_fold_to_integer_constant(lhs, &lhs_value);

            Expression* rhs = expression_binary_get_rhs(expression);
            ExpressionIntegerValue rhs_value = {0};
            expression_fold_to_integer_constant(rhs, &rhs_value);

            switch (kind)
            {
                case EXPRESSION_BINARY_TIMES:
                    value->value = lhs_value.value * rhs_value.value;
                    break;

                case EXPRESSION_BINARY_DIVIDE:
                    if (rhs_value.value == 0)
                    {
                        return false;
                    }

                    value->value = lhs_value.value / rhs_value.value;
                    break;

                case EXPRESSION_BINARY_MODULO:
                    if (rhs_value.value == 0)
                    {
                        return false;
                    }

                    value->value = lhs_value.value % rhs_value.value;
                    break;

                case EXPRESSION_BINARY_ADD:
                    value->value = lhs_value.value + rhs_value.value;
                    break;

                case EXPRESSION_BINARY_SUBTRACT:
                    value->value = lhs_value.value - rhs_value.value;
                    break;

                case EXPRESSION_BINARY_SHIFT_LEFT:
                    value->value = lhs_value.value << rhs_value.value;
                    break;

                case EXPRESSION_BINARY_SHIFT_RIGHT:
                    value->value = lhs_value.value >> rhs_value.value;
                    break;

                case EXPRESSION_BINARY_LESS_THAN:
                    value->value = lhs_value.value < rhs_value.value;
                    break;
                
                case EXPRESSION_BINARY_GREATER_THAN:
                    value->value = lhs_value.value > rhs_value.value;
                    break;

                case EXPRESSION_BINARY_LESS_THAN_EQUAL:
                    value->value = lhs_value.value <= rhs_value.value;
                    break;

                case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
                    value->value = lhs_value.value >= rhs_value.value;
                    break;

                case EXPRESSION_BINARY_EQUAL:
                    value->value = lhs_value.value == rhs_value.value;
                    break;

                case EXPRESSION_BINARY_NOT_EQUAL:
                    value->value = lhs_value.value != rhs_value.value;
                    break;

                case EXPRESSION_BINARY_AND:
                    value->value = lhs_value.value & rhs_value.value;
                    break;

                case EXPRESSION_BINARY_XOR:
                    value->value = lhs_value.value ^ rhs_value.value;
                    break;

                case EXPRESSION_BINARY_OR:
                    value->value = lhs_value.value | rhs_value.value;
                    break;

                case EXPRESSION_BINARY_LOGICAL_AND:
                    value->value = lhs_value.value && rhs_value.value;
                    break;

                case EXPRESSION_BINARY_LOGICAL_OR:
                    value->value = lhs_value.value || rhs_value.value;
                    break;

                default:
                    panic("bad binary expression type");
                    return false;
            }

            return true;
        }
        
        case EXPRESSION_CONDITIONAL:
        {
            Expression* cond = expression_conditional_get_cond(expression);
            ExpressionIntegerValue cond_val = {0};
            expression_fold_to_integer_constant(cond, &cond_val);

            // If non-zero fold the true side, otherwise fold the false side.
            Expression* t = expression_conditional_get_true(expression);
            Expression* f = expression_conditional_get_false(expression);
            if (expression_integer_value_get(&cond_val))
            {
                return expression_fold_to_integer_constant(t, value);
            }
            return expression_fold_to_integer_constant(f, value);
        }

        // Remove the implicit cast and evaluate the inner
        case EXPRESSION_CAST_IMPLICIT:
        {
            Expression* inner = expression_implicit_cast_get_inner(expression);
            return expression_fold_to_integer_constant(inner, value);
        }

        // If any of these expressions appear something has gone wrong.
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
        case EXPRESSION_ARRAY_DECAY:
        case EXPRESSION_LVALUE_CAST:
        case EXPRESSION_REFERENCE:
        case EXPRESSION_ERROR:
        default:
            panic("expression cannot be part of an ICE");
            return false;
    }

    return false;
}


