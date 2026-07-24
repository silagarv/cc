#include "lex/token.h"
#include "parser_new.h"

#include <assert.h>

// FIXME: make the interface for the semantic checker handling binary 
// FIXME: expressions to be nice so we can have a function that gets the binary
// FIXME: expression type from the operator to clean this up a little bit.

// TODO: cast and primary expressions will need to be done...

Expression* parse_parenthesised_expression(Parser* parser)
{
    assert(parser_is(parser, TOK_LPAREN));

    Location lparen_loc = parser_consume(parser);
    Expression* expr = parse_expression(parser);

    // Don't create the next expression at all, rather, return the
    // expression we just parsed so save us some trouble.
    Location rparen_loc = LOCATION_INVALID;
    if (!parser_try_consume(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ')' after expression");
        return expr;
    }

    return semantic_checker_handle_parenthesis_expression(&parser->sc,
            lparen_loc, expr, rparen_loc);
}

Expression* parse_reference_expression(Parser* parser)
{
    assert(parser_is(parser, TOK_IDENTIFIER) && "not an identifier");

    Identifier* identifier = token_get_identifier(parser_current(parser));
    Location identifer_loc = parser_consume(parser);

    bool is_function_call = parser_is(parser, TOK_LPAREN);
    return semantic_checker_handle_reference_expression(&parser->sc,
            identifer_loc, identifier, is_function_call);
}

Expression* parse_numeric_expression(Parser* parser)
{
    assert(parser_is(parser, TOK_NUMBER) && "not a number");

    Token number_tok = *parser_current(parser);
    Location loc = parser_consume(parser);

    LiteralValue value = {0};
    bool success = parse_preprocessing_number(&value, parser->dm, parser->pp.sm,
            parser->lang, number_tok);
    
    return semantic_checker_handle_number_expression(&parser->sc,
            loc, value, success);
}

static Expression* parse_character_expression(Parser* parser)
{
    assert(token_is_character(parser_current(parser)) && "not a character");
            
    Token char_token = *parser_current(parser);
    Location loc = parser_consume(parser);

    CharValue value = {0};
    bool success = parse_char_literal(&value, parser->dm, parser->pp.sm,
            parser->lang, char_token);
    
    return semantic_checker_handle_char_expression(&parser->sc,
            token_get_location(&char_token), value, success);
}

Expression* parse_string_expression(Parser* parser)
{
    assert(token_is_string(parser_current(parser)) && "not a string");

    Location start_location = parser_current_location(parser);

    // Track if the token is wide to make conversion easier
    TokenList strings = token_list(arena_new_default());
    do
    {
        token_list_push_back(&strings, *parser_current(parser));
        parser_consume(parser);
    }
    while (token_is_string(parser_current(parser)));

    // Attempt the conversion using the information we have here
    StringLiteral string;
    bool conversion = parse_string_literal(ast_get_allocator(parser->ast),
            &string, parser->dm, parser->pp.sm, parser->lang, &strings,
            /*unevaluated*/false);

    // Make sure to free our token list since we are done with it
    token_list_free(&strings);

    // Finally, create our string expression.
    return semantic_checker_handle_string_expression(&parser->sc,
            start_location, string, conversion);
}

Expression* parse_builtin_identifier(Parser* parser)
{
    assert(parser_is(parser, TOK___func__) && "not __func__");

    Location location = parser_consume(parser);
    return semantic_checker_handle_builtin_identifier(&parser->sc,
            location);
}

// static void recover_end_of_builtin(Parser* parser)
// {
//     recover(parser, TOK_RPAREN, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
// }

// static Expression* parse_builtin_va_arg(Parser* parser)
// {
//     assert(parser_is(parser, TOK___builtin_va_arg));

//     Location builtin_loc = parser_consume(parser);

//     Location lparen_loc;
//     if (!parser_try_consume(parser, TOK_LPAREN, &lparen_loc))
//     {
//         diagnostic_error_at(parser->dm, lparen_loc, "expected '('");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     Expression* expr = parse_assignment_expression(parser);

//     if (!parser_try_consume(parser, TOK_COMMA, NULL))
//     {
//         diagnostic_error_at(parser->dm, parser_current_location(parser),
//                 "expected ','");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     if (!is_typename_start(parser, current_token(parser)))
//     {
//         diagnostic_error_at(parser->dm, parser_current_location(parser),
//                 "expected a type");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     bool okay = true;
//     QualifiedType type = parse_type_name(parser, &okay);

//     Location rparen_loc;
//     if (!parser_try_consume(parser, TOK_RPAREN, &rparen_loc))
//     {
//         diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     // TODO: properly create this expression.
//     return semantic_checker_handle_error_expression(&parser->sc, builtin_loc);
// }

// static Expression* parse_builtin_offsetof(Parser* parser)
// {
//     assert(parser_is(parser, TOK___builtin_offsetof));

//     Location builtin_loc = parser_consume(parser);

//     Location lparen_loc;
//     if (!parser_try_consume(parser, TOK_LPAREN, &lparen_loc))
//     {
//         diagnostic_error_at(parser->dm, lparen_loc, "expected '('");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     if (!is_typename_start(parser, current_token(parser)))
//     {
//         diagnostic_error_at(parser->dm, parser_current_location(parser),
//                 "expected a type");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     bool okay = true;
//     QualifiedType type = parse_type_name(parser, &okay);

//     // Now try to get the membre designator for it
//     if (!parser_try_consume(parser, TOK_COMMA, NULL))
//     {
//         diagnostic_error_at(parser->dm, parser_current_location(parser),
//                 "expected ','");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     // Now try to get the member designator.
//     if (!parser_is(parser, TOK_IDENTIFIER))
//     {
//         diagnostic_error_at(parser->dm, parser_current_location(parser),
//                 "expected identifier");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     Identifier* id = current_token(parser)->data.identifier;
//     Location id_loc = parser_consume(parser);

//     while (true)
//     {
//         if (parser_is(parser, TOK_DOT))
//         {
//             Location dot = parser_consume(parser);

//             if (!parser_is(parser, TOK_IDENTIFIER))
//             {
//                 diagnostic_error_at(parser->dm, parser_current_location(parser),
//                         "expected identifier");
//                 recover_end_of_builtin(parser);
//                 return semantic_checker_handle_error_expression(&parser->sc,
//                         builtin_loc);
//             }

//             id = current_token(parser)->data.identifier;
//             id_loc = parser_consume(parser);
//         }
//         else if (parser_is(parser, TOK_LBRACKET))
//         {
//             Location lbracket_loc = parser_consume(parser);
//             Expression* expr = parse_expression(parser);
//             Location rbracket_loc = LOCATION_INVALID;
//             if (!parser_try_consume(parser, TOK_RBRACKET, &rbracket_loc))
//             {
//                 diagnostic_error_at(parser->dm, rbracket_loc, "expected ']'");
//                 recover_end_of_builtin(parser);
//                 return semantic_checker_handle_error_expression(&parser->sc,
//                         builtin_loc);
//             }
//         }
//         else
//         {
//             break;
//         }
//     }

//     // Finally, eat the trailing right parenthesis.
//     Location rparen_loc;
//     if (!parser_try_consume(parser, TOK_RPAREN, &rparen_loc))
//     {
//         diagnostic_error_at(parser->dm, lparen_loc, "expected ')'");
//         recover_end_of_builtin(parser);
//         return semantic_checker_handle_error_expression(&parser->sc,
//                 builtin_loc);
//     }

//     // TODO: create this expression properly
//     return semantic_checker_handle_error_expression(&parser->sc, builtin_loc);
// }

static Expression* parse_primary_expression(Parser* parser)
{
    switch (parser_current_type(parser))
    {
        case TOK_LPAREN:
            return parse_parenthesised_expression(parser);
        
        case TOK_IDENTIFIER:
            return parse_reference_expression(parser);

        case TOK_NUMBER:
            return parse_numeric_expression(parser);

        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
        case TOK_UTF8_CHARACTER:
        case TOK_UTF16_CHARACTER:
        case TOK_UTF32_CHARACTER:
            return parse_character_expression(parser);

        case TOK_STRING:
        case TOK_WIDE_STRING:
        case TOK_UTF8_STRING:
        case TOK_UTF16_STRING:
        case TOK_UTF32_STRING:
            return parse_string_expression(parser);

        case TOK___func__:
            return parse_builtin_identifier(parser);

        // We imitate both GCC and Clang in that va_arg is the only va_bultin
        // declared as a token in its own right. Otherwise we parse and handle
        // the other required builtins elsewhere.
        // case TOK___builtin_va_arg:
        //     return parse_builtin_va_arg(parser);

        // case TOK___builtin_offsetof:
        //     return parse_builtin_offsetof(parser);
        
        default:
        {
            Location err_loc = parser_current_location(parser);
            diagnostic_error_at(parser->dm, err_loc, "expected expression");
            return semantic_checker_handle_error_expression(&parser->sc,
                    err_loc);
        }
    }
}
Expression* parse_postfix_ending(Parser* parser, Expression* start);

Expression* parse_postfix_expression(Parser* parser)
{
    // How a handle compound literal is handled. Since the start of it looks
    // exactly like a cast expression we parse the cast first. Then since we see
    // a '{' we then parse the compound literal. After a compound literal we
    // should end up here. But since we that is hard we just pass in the 
    // compound literal expression and DON'T parse a primary expression. 
    Expression* expr = parse_primary_expression(parser);
    return parse_postfix_ending(parser, expr);
}

// Required predefinition...
Expression* parse_cast_expression(Parser* parser);

Expression* parse_unary_expression(Parser* parser)
{
    switch (parser_current_type(parser))
    {
        case TOK_PLUS_PLUS:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_INCREMENT, expr, op_loc);
        }

        case TOK_MINUS_MINUS:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_DECREMENT, expr, op_loc);
        }

        case TOK_AND:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_address_expression(&parser->sc, expr,
                    op_loc);
        }

        case TOK_STAR:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_dereference_expression(&parser->sc,
                    expr, op_loc);
        }

        case TOK_PLUS:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_PLUS, op_loc, expr);
        }

        case TOK_MINUS:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_MINUS, op_loc, expr);
        }

        case TOK_TILDE:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
        }

        case TOK_NOT:
        {
            Location op_loc = parser_consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_NOT, op_loc, expr);
        }

        case TOK_sizeof:
        {
            Location sizeof_loc = parser_consume(parser);
            
            if (parser_is(parser, TOK_LPAREN) && 
                    parser_is_next_typename(parser))
            {
                Location lparen_loc = parser_consume(parser);
                QualifiedType type = parse_type_name(parser);
                Location rparen_loc;
                if (!parser_try_consume(parser, TOK_RPAREN, &rparen_loc))
                {
                    diagnostic_error_at(parser->dm, rparen_loc,
                            "expected ')' after type name");
                    rparen_loc = LOCATION_INVALID;
                }

                return semantic_checker_handle_sizeof_type_expression(
                        &parser->sc, sizeof_loc, lparen_loc, type, rparen_loc);
            }
            else
            {
                Expression* expr = parse_unary_expression(parser);
                return semantic_checker_handle_sizeof_expression(&parser->sc,
                        sizeof_loc, expr);
            }
        }

        // GNU address of label extension is treated as a unary expression by
        // GCC (and Clang it seems), so leave this here as a TODO
        // case TOK_AND_AND:
        // { ... }

        default:
            return parse_postfix_expression(parser);
    }
}

Expression* parse_cast_expression(Parser* parser)
{
    if (!parser_is(parser, TOK_LPAREN) || !parser_is_next_typename(parser))
    {
        return parse_unary_expression(parser);
    }

    return NULL;
}

Expression* parse_multiplicative_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_STAR, TOK_SLASH, 
            TOK_PERCENT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_cast_expression(parser);

    while (parser_has_match(parser, num_operators, operators))
    {
        ExpressionType type;
        switch (parser_current_type(parser))
        {
            case TOK_STAR:
                type = EXPRESSION_BINARY_TIMES;
                break;

            case TOK_SLASH:
                type = EXPRESSION_BINARY_DIVIDE;
                break;

            case TOK_PERCENT:
                type = EXPRESSION_BINARY_MODULO;
                break;
        }
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_cast_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_additive_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_PLUS, TOK_MINUS};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_multiplicative_expression(parser);

    while (parser_has_match(parser, num_operators, operators))
    {
        ExpressionType type;
        switch (parser_current_type(parser))
        {
            case TOK_PLUS:
                type = EXPRESSION_BINARY_ADD;
                break;

            case TOK_MINUS:
                type = EXPRESSION_BINARY_SUBTRACT;
                break;
        }
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_multiplicative_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_shift_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_LT_LT, TOK_GT_GT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_additive_expression(parser);

    while (parser_has_match(parser, num_operators, operators))
    {
        ExpressionType type;
        switch (parser_current_type(parser))
        {
            case TOK_LT_LT:
                type = EXPRESSION_BINARY_SHIFT_LEFT;
                break;

            case TOK_GT_GT:
                type = EXPRESSION_BINARY_SHIFT_RIGHT;
                break;
        }
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_additive_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_relational_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_LT, TOK_GT, TOK_LT_EQUAL, 
            TOK_GT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_shift_expression(parser);

    while (parser_has_match(parser, num_operators, operators))
    {
        ExpressionType type;
        switch (parser_current_type(parser))
        {
            case TOK_LT: 
                type = EXPRESSION_BINARY_LESS_THAN;
                break;

            case TOK_GT: 
                type = EXPRESSION_BINARY_GREATER_THAN; 
                break;

            case TOK_LT_EQUAL: 
                type = EXPRESSION_BINARY_LESS_THAN_EQUAL;
                break;

            case TOK_GT_EQUAL: 
                type = EXPRESSION_BINARY_GREATER_THAN_EQUAL;
                break;
        }
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_shift_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_equality_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_EQUAL_EQUAL, TOK_NOT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_relational_expression(parser);

    while (parser_has_match(parser, num_operators, operators))
    {
        ExpressionType type;
        switch (parser_current_type(parser))
        {
            case TOK_EQUAL_EQUAL:
                type = EXPRESSION_BINARY_EQUAL;
                break;

            case TOK_NOT_EQUAL:
                type = EXPRESSION_BINARY_NOT_EQUAL;
                break;
        }
        
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_relational_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_and_expression(Parser* parser)
{
    Expression* expr = parse_equality_expression(parser);

    while (parser_is(parser, TOK_AND))
    {
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_equality_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_AND, expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_exclusive_or_expression(Parser* parser)
{
    Expression* expr = parse_and_expression(parser);

    while (parser_is(parser, TOK_XOR))
    {
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_and_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_XOR, expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_inclusive_or_expression(Parser* parser)
{
    Expression* expr = parse_exclusive_or_expression(parser);

    while (parser_is(parser, TOK_OR))
    {
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_exclusive_or_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_OR, expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_logical_and_expression(Parser* parser)
{
    Expression* expr = parse_inclusive_or_expression(parser);

    while (parser_is(parser, TOK_AND_AND))
    {
        Location op_loc = parser_consume(parser);
        Expression* rhs = parse_inclusive_or_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_LOGICAL_AND, expr, op_loc, rhs);
    }

    return expr;
}

Expression* parse_logical_or_expression(Parser* parser)
{
    Expression* expr = parse_logical_and_expression(parser);

    while (parser_is(parser, TOK_OR_OR))
    {
        Location op = parser_consume(parser);
        Expression* rhs = parse_logical_and_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_LOGICAL_OR, expr, op, rhs);
    }

    return expr;
}

Expression* parse_conditional_expression(Parser* parser)
{
    Expression* expr = parse_logical_or_expression(parser);

    if (parser_is(parser, TOK_QUESTION))
    {
        Location question = parser_consume(parser);

        Expression* true_expr = parse_expression(parser);

        // Note: both clang and gcc seem to act as if colon existed anyways
        Location colon;
        if (!parser_try_consume(parser, TOK_COLON, &colon))
        {
            diagnostic_error_at(parser->dm, colon, "expected ':'");
            colon = LOCATION_INVALID;
        }

        Expression* false_expr = parse_conditional_expression(parser);

        expr = semantic_checker_handle_conditional_expression(&parser->sc, expr,
                question, true_expr, colon, false_expr);
    }

    return expr;
}

Expression* parse_assignment_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_EQUAL, TOK_STAR_EQUAL, 
            TOK_SLASH_EQUAL, TOK_PERCENT_EQUAL, TOK_PLUS_EQUAL, 
            TOK_MINUS_EQUAL, TOK_LT_LT_EQUAL, TOK_GT_GT_EQUAL,
            TOK_AND_EQUAL, TOK_XOR_EQUAL, TOK_OR_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_conditional_expression(parser);

    if (parser_has_match(parser, num_operators, operators))
    {
        // TokenType tok_type = parser_current_type(parser);
        ExpressionType type;// = parser_binary_expression_type(tok_type);
        switch (parser_current_type(parser))
        {
            case TOK_EQUAL: 
                type = EXPRESSION_BINARY_ASSIGN;
                break;

            case TOK_STAR_EQUAL: 
                type = EXPRESSION_BINARY_TIMES_ASSIGN;
                break;

            case TOK_SLASH_EQUAL: 
                type = EXPRESSION_BINARY_DIVIDE_ASSIGN;
                break;

            case TOK_PERCENT_EQUAL: 
                type = EXPRESSION_BINARY_MODULO_ASSIGN;
                break;

            case TOK_PLUS_EQUAL: 
                type = EXPRESSION_BINARY_ADD_ASSIGN;
                break;

            case TOK_MINUS_EQUAL: 
                type = EXPRESSION_BINARY_SUBTRACT_ASSIGN;
                break;

            case TOK_LT_LT_EQUAL: 
                type = EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN;
                break;

            case TOK_GT_GT_EQUAL: 
                type = EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN;
                break;

            case TOK_AND_EQUAL: 
                type = EXPRESSION_BINARY_AND_ASSIGN;
                break;

            case TOK_XOR_EQUAL: 
                type = EXPRESSION_BINARY_XOR_ASSIGN;
                break;

            case TOK_OR_EQUAL: 
                type = EXPRESSION_BINARY_OR_ASSIGN;
                break;
        }
        Location op = parser_consume(parser);
        Expression* rhs = parse_assignment_expression(parser);

        expr = semantic_checker_handle_assignment_expression(&parser->sc, type,
                expr, op, rhs);
    }

    // Finalize the expression here before the comma expression since, we want
    // to make sure that all expressions in the comma expression are done 
    // correctly.
    return semantic_checker_expression_finalize(&parser->sc, expr);
}

Expression* parse_expression(Parser* parser) 
{
    Expression* expr = parse_assignment_expression(parser);

    while (parser_is(parser, TOK_COMMA))
    {
        Location comma_loc = parser_consume(parser);
        Expression* rhs = parse_assignment_expression(parser);

        expr = semantic_checker_handle_comma_expression(&parser->sc, expr,
                comma_loc, rhs);
    }

    return expr;
}
