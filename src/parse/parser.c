#include "parser.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "util/panic.h"
#include "util/ptr_set.h"
#include "util/str.h"

#include "driver/diagnostic.h"

#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/preprocessor.h"

#include "parse/literal_parser.h"
#include "parse/type.h"
#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"
#include "parse/initializer.h"
#include "parse/ast.h"
#include "parse/scope.h"
#include "parse/semantic.h"

#define countof(array) (sizeof(array) / sizeof(array[0]))

typedef enum RecoverFlags {
    RECOVER_NONE = 0, // No flags
    RECOVER_EAT_TOKEN = 1 << 0, // Do we eat the token we want to stop on?
    RECOVER_STOP_AT_SEMI = 1 << 1, // Do we stop at semi even if it's not the
                                   // token we're looking for
} RecoverFlags;

// Below are the start set for some of the specifiers / qualifiers
static const TokenType type_qualifier[] = {TOKEN_CONST, TOKEN_RESTRICT,
        TOKEN_VOLATILE};
static const size_t type_qualifier_count = countof(type_qualifier);

// Parser methods for getting the current and next token. We aim to be a LL(1)
// like parser so we should only ever need these in order to figure own the
// current production.
static Token* current_token(Parser* parser);

// TODO: reimplement the method below
static Token* next_token(Parser* parser);

static TokenType current_token_type(Parser* parser);
static TokenType next_token_type(Parser* parser);

static Location current_token_location(Parser* parser);
static Location current_token_end_location(Parser* parser);

// Methods for mathing a token type or unconditionally consuming a token.
static Location consume(Parser* parser);

static bool is_match(Parser* parser, TokenType type);
static bool has_match(Parser* parser, const TokenType* types, size_t count);
static bool is_next_match(Parser* parser, TokenType type);

// Parser methods for getting the current and next token. We aim to be a LL(1)
// like parser so we should only ever need these in order to figure own the
// current production.
static Token* current_token(Parser* parser)
{
    return &parser->token;
}

static Token* next_token(Parser* parser)
{
    preprocessor_peek_token(parser->pp, &parser->peek_token);

    return &parser->peek_token;
}

static TokenType current_token_type(Parser* parser)
{
    return parser->token.type;
}

static TokenType next_token_type(Parser* parser)
{
    return preprocessor_peek_next_token_type(parser->pp);
}

static Location current_token_location(Parser* parser)
{
    return parser->token.loc;
}

static Location current_token_end_location(Parser* parser)
{
    return parser->token.end;
}

static Location consume(Parser* parser)
{
    // This method helps us to automatically track the braces and brackets!
    switch (current_token_type(parser))
    {   
        // Bail out this probably wasn't intended
        case TOKEN_EOF:
            panic("attempting to consume EOF token!");
            break;

        case TOKEN_LPAREN:
            parser->paren_count++;
            break;

        case TOKEN_RPAREN:
            if (parser->paren_count)
            {
                parser->paren_count--;
            }
            break;

        case TOKEN_LBRACKET:
            parser->bracket_count++;
            break;

        case TOKEN_RBRACKET:
            if(parser->bracket_count)
            {
                parser->bracket_count--;
            }
            break;

        case TOKEN_LCURLY:
            parser->brace_count++;
            break;

        case TOKEN_RCURLY:
            if(parser->brace_count)
            {
                parser->brace_count--;
            }
            break;

        default:
            break;
    }

    Location location = current_token_location(parser);

    preprocessor_advance_token(parser->pp, &parser->token);

    return location;
}

static bool is_match(Parser* parser, TokenType type)
{
    return has_match(parser, (TokenType[]) {type}, 1);
}

static bool is_match_two(Parser* parser, TokenType type1, TokenType type2)
{
    return has_match(parser, (TokenType[]) {type1, type2}, 2);
}

static bool try_match(Parser* parser, TokenType type, Location* location)
{
    if (location != NULL)
    {
        *location = current_token_location(parser);
    }

    if (is_match(parser, type))
    {
        consume(parser);

        return true;
    }

    return false;
}

static bool has_match(Parser* parser, const TokenType* types, size_t count)
{
    const TokenType current = current_token_type(parser);

    for (size_t i = 0; i < count; i++)
    {
        if (current == types[i])
        {
            return true;
        }
    }

    return false;
}

static bool is_next_match(Parser* parser, TokenType type)
{
    return preprocessor_peek_next_token_type(parser->pp) == type;
}

static void recover_many(Parser* parser, TokenType* types, size_t num_types,
        RecoverFlags flags)
{
    bool has_skipped = false;
    while (true)
    {
        // Check if we got to a token we wanted to stop at        
        if (has_match(parser, types, num_types))
        {
            // If we're meant to eat it do so
            if (flags & RECOVER_EAT_TOKEN)
            {
                consume(parser);
            }

            return;
        }

        TokenType current = current_token_type(parser);
        switch (current)
        {
            // Don't want to accidentally consume this
            case TOKEN_EOF:
                return;

            // For each of our paren thesis types make sure we try to balance
            // then as best as possible
            case TOKEN_LPAREN:
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOKEN_RPAREN}, 1,
                        RECOVER_EAT_TOKEN);
                break;

            case TOKEN_LBRACKET:
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOKEN_RBRACKET}, 1, 
                        RECOVER_EAT_TOKEN);
                break;

            case TOKEN_LCURLY:
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOKEN_RCURLY}, 1, 
                        RECOVER_EAT_TOKEN);
                break;

            // For our closing types we need to do something different
            case TOKEN_RPAREN:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (parser->paren_count && has_skipped)
                {
                    return;
                }
                consume(parser);
                break;

            case TOKEN_RBRACKET:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (parser->bracket_count && has_skipped)
                {
                    return;
                }
                consume(parser);
                break;
                
            case TOKEN_RCURLY:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (parser->brace_count && has_skipped)
                {
                    return;
                }
                consume(parser);
                break;

            // Check if we were meant to stop at a semi
            case TOKEN_SEMI:
                if (flags & RECOVER_STOP_AT_SEMI)
                {
                    return;
                }

            /* FALLTHROUGH */

            default:
                consume(parser);
                break;
        }
        has_skipped = true;
    }
}

static void recover(Parser* parser, TokenType type, RecoverFlags flags)
{
    recover_many(parser, (TokenType[1]) {type}, 1, flags);
}

static void recover_two(Parser* parser, TokenType type1, TokenType type2,
        RecoverFlags flags)
{
    recover_many(parser, (TokenType[2]) {type1, type2}, 2, flags);
}

static void recover_three(Parser* parser, TokenType type1, TokenType type2,
        TokenType type3, RecoverFlags flags)
{
    recover_many(parser, (TokenType[3]) {type1, type2, type3}, 3, flags);
}

static bool is_typename_start(Parser* parser, const Token* tok);
static bool is_expression_start(Parser* parser, const Token* tok);
static bool is_statement_start(Parser* parser, const Token* tok);
static bool is_initializer_start(Parser* parser, const Token* tok);

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_primary_expression(Parser* parser);
static Expression* parse_postfix_expression(Parser* parser);
static void parse_argument_expression_list(Parser* parser,
        ExpressionList* list);
static Expression* parse_unary_expression(Parser* parser);
static Expression* parse_cast_expression(Parser* parser);
static Expression* parse_multiplicative_expression(Parser* parser);
static Expression* parse_additive_expression(Parser* parser);
static Expression* parse_shift_expression(Parser* parser);
static Expression* parse_relational_expression(Parser* parser);
static Expression* parse_equality_expression(Parser* parser);
static Expression* parse_and_expression(Parser* parser);
static Expression* parse_exclusive_or_expression(Parser* parser);
static Expression* parse_inclusive_or_expression(Parser* parser);
static Expression* parse_logical_and_expression(Parser* parser);
static Expression* parse_logical_or_expression(Parser* parser);
static Expression* parse_conditional_expression(Parser* parser);
static Expression* parse_assignment_expression(Parser* parser);
static Expression* parse_constant_expression(Parser* parser);
static Expression* parse_expression(Parser* parser);

// All of our functions for parsing statements
static Statement* parse_label_statement(Parser* parser);
static Statement* parse_case_statement(Parser* parser);
static Statement* parse_default_statement(Parser* parser);
static Statement* parse_compound_statement(Parser* parser);
static Statement* parse_expression_statement(Parser* parser);
static Statement* parse_declaration_statement(Parser* parser);
static Statement* parse_empty_statement(Parser* parser);
static Statement* parse_statement(Parser* parser, bool declaration_allowed);

// All of our functions for parsing declarations / definitions
// TODO: maybe all of these don't need to return a declaration type???
static DesignatorList* parse_designator_list(Parser* parser);
static Initializer* parse_initializer(Parser* parser);
static Initializer* parse_initializer_list(Parser* parser);

static Declarator parse_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext ctx);

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context);

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl);

static void parse_direct_declarator(Parser* parser, Declarator* declarator);

static void parse_identifier_list(Parser* parser, Declarator* declarator);
static Declaration* parse_paramater_declaration(Parser* parser);
static void parse_paramater_type_list(Parser* parser, Declarator* declarator);

// All of our declaration specifier parsing functions
static void declaration_specifiers_add_storage(Parser* parser,
        DeclarationSpecifiers* specifiers, StorageSpecifier storage,
        bool spec_qual_only, Location location);
static void declaration_specifiers_add_qualifier(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeQualifiers qualifier,
        Location location);
static void declaration_specifiers_add_function(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeFunctionSpecifier function,
        bool spec_qual_only, Location location);
static void declaration_specifiers_add_width(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierWidth width,
        Location location);
static void declaration_specifiers_add_sign(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierSign sign,
        Location location);
static void declaration_specifiers_add_complex(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierComplex complex,
        Location location);
static void declaration_specifiers_add_type(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierType type,
        Declaration* declaration_opt, Location location);
static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser,
        bool spec_qual_only);

static TypeQualifiers parse_type_qualifier_list(Parser* parser);
static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser);

static void parse_struct_declaration(Parser* parser, Declaration* decl);
static void parse_struct_declaration_list(Parser* parser, Declaration* decl,
        bool is_struct);
static void parse_struct_or_union_specifier(Parser* parser,
        DeclarationSpecifiers* specifiers);

static Declaration* parse_declaration(Parser* parser, DeclaratorContext ctx,
        bool eat_semi);
static QualifiedType parse_type_name(Parser* parser);

// The definitions of the functions we will use for pasing
static bool is_typename_start(Parser* parser, const Token* tok)
{
    const TokenType type = tok->type;
    switch (type)
    {
        // Type speicifiers
        case TOKEN_VOID:
        case TOKEN_CHAR:
        case TOKEN_SHORT:
        case TOKEN_INT:
        case TOKEN_LONG:
        case TOKEN_FLOAT:
        case TOKEN_DOUBLE:
        case TOKEN_SIGNED:
        case TOKEN_UNSIGNED:
        case TOKEN__BOOL:
        case TOKEN__COMPLEX:
        case TOKEN__IMAGINARY:
        case TOKEN_STRUCT:
        case TOKEN_UNION:
        case TOKEN_ENUM:

        // Type qualifiers
        case TOKEN_CONST:
        case TOKEN_VOLATILE:
        case TOKEN_RESTRICT:

        // Function specifiers
        case TOKEN_INLINE:

        // Storage classes
        case TOKEN_TYPEDEF:
        case TOKEN_EXTERN:
        case TOKEN_STATIC:
        case TOKEN_REGISTER:
        case TOKEN_AUTO:
            return true;

        // Possibly?
        case TOKEN_IDENTIFIER:
        {
            Identifier* identifier = tok->data.identifier;
            return semantic_checker_identifier_is_typename(&parser->sc,
                    identifier);
        }

        default:
            return false;
    }
}

static bool is_expression_token(TokenType type)
{
    switch (type)
    {
        case TOKEN_NUMBER:
        case TOKEN_WIDE_CHARACTER:
        case TOKEN_WIDE_STRING:
        case TOKEN_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_LPAREN:
        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS:
        case TOKEN_AND:
        case TOKEN_STAR:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_NOT:
        case TOKEN_TILDE:
        case TOKEN_SIZEOF:
        case TOKEN_IDENTIFIER:

        // Builtin identifiers
        case TOKEN___FUNC__:
            return true;

        default:
            return false;
    }
}

static bool is_expression_start(Parser* parser, const Token* tok)
{
    TokenType type = tok->type;
    if (!is_expression_token(type))
    {
        return false;
    }

    if (is_expression_token(type) && type != TOKEN_IDENTIFIER)
    {
        return true;
    }

    return !is_typename_start(parser, tok);
}

static bool is_statement_start(Parser* parser, const Token* tok)
{
    switch (tok->type)
    {
        // All of the easy statement startings
        case TOKEN_LCURLY:
        case TOKEN_CASE:
        case TOKEN_DEFAULT:
        case TOKEN_IF:
        case TOKEN_SWITCH:
        case TOKEN_WHILE:
        case TOKEN_DO:
        case TOKEN_FOR:
        case TOKEN_GOTO:
        case TOKEN_CONTINUE:
        case TOKEN_BREAK:
        case TOKEN_RETURN:
        case TOKEN_SEMI:
        case TOKEN_IDENTIFIER:
            return true;

        default:
            // Otherwise check if we can start and expression or typename
            if (is_expression_start(parser, tok))
            {
                return true;
            }
            else if (is_typename_start(parser, tok))
            {
                return true;
            }

            return false;
    }

    return false;
}

static bool is_initializer_start(Parser* parser, const Token* tok)
{
    return is_expression_start(parser, tok) || token_is_type(tok, TOKEN_LCURLY);
}

static bool is_string_token(Parser* parser, const Token* tok)
{
    (void) parser;

    switch (tok->type)
    {
        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
            return true;

        default:
            return false;
    }
}

static Expression* parse_primary_expression(Parser* parser)
{
    static const TokenType string_tokens[] = {TOKEN_STRING, TOKEN_WIDE_STRING};
    static const size_t num_string_tokens = countof(string_tokens);

    TokenType type = current_token_type(parser);
    switch (type)
    {
        case TOKEN_LPAREN:
        {
            Location lparen_loc = consume(parser);
            Expression* expr = parse_expression(parser);

            // Don't create the next expression at all, rather, return the
            // expression we just parsed so save us some trouble.
            Location rparen_loc = LOCATION_INVALID;
            if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
            {
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "expected ')' after expression");
                return expr;
            }

            return semantic_checker_handle_parenthesis_expression(&parser->sc,
                    lparen_loc, expr, rparen_loc);
        }
        
        case TOKEN_IDENTIFIER:
        {
            Identifier* identifier = current_token(parser)->data.identifier;
            Location identifer_loc = consume(parser);

            bool is_function_call = is_match(parser, TOKEN_LPAREN);
            return semantic_checker_handle_reference_expression(&parser->sc,
                    identifer_loc, identifier, is_function_call);
        }

        case TOKEN_NUMBER:
        {
            Token number_tok = *current_token(parser);
            Location loc = consume(parser);

            LiteralValue value = {0};
            bool success = parse_preprocessing_number(&value, parser->dm,
                    &number_tok);
            
            return semantic_checker_handle_number_expression(&parser->sc,
                    loc, value, success);
        }

        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
        {
            bool wide = is_match(parser, TOKEN_WIDE_CHARACTER);
            
            Token char_token = *current_token(parser);
            Location loc = consume(parser);

            CharValue value = {0};
            bool success = parse_char_literal(&value, parser->dm, &char_token,
                    wide);

            return semantic_checker_handle_char_expression(&parser->sc, loc,
                    value, success);
        }

        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
        {
            Location start_location = current_token_location(parser);

            TokenVector toks = token_vector_create(1);
            LocationVector locs = location_vector_create(1);

            // Track if the token is wide to make conversion easier
            bool wide = false;
            do
            {
                if (is_match(parser, TOKEN_WIDE_STRING))
                {
                    wide = true;
                }

                Token string_token = *current_token(parser);
                Location token_loc = consume(parser);

                token_vector_push(&toks, string_token);
                location_vector_push(&locs, token_loc);
            }
            while (has_match(parser, string_tokens, num_string_tokens));

            // Attempt the conversion using the information we have here
            StringLiteral string;
            bool conversion = parse_string_literal(&parser->ast.ast_allocator,
                    &string, parser->dm, toks, locs, wide);

            // Make sure to free our vectors after we are done with them.
            token_vector_free(&toks, NULL);
            location_vector_free(&locs, NULL);

            if (!conversion)
            {
                diagnostic_error(parser->dm, "string conversion failed");
            }
            else
            {
                // printf("%s\n", string.string.ptr);
            }

            return semantic_checker_handle_error_expression(&parser->sc,
                    start_location);
        }

        case TOKEN___FUNC__:
        {
            Location location = consume(parser);
            return semantic_checker_handle_builtin_identifier(&parser->sc,
                    location);
        }

        default:
        {
            Location err_loc = current_token_location(parser);
            diagnostic_error_at(parser->dm, err_loc, "expected expression");

            // Do not do error recovery since the function calling this one 
            // could be pretty much anything.
            return semantic_checker_handle_error_expression(&parser->sc,
                    err_loc);
        }
    }
}

static void parse_argument_expression_list(Parser* parser,
        ExpressionList* list)
{
    do
    {
        Expression* arg = parse_assignment_expression(parser);
        expression_list_push(&parser->ast.ast_allocator, list, arg);
    }
    while (try_match(parser, TOKEN_COMMA, NULL));
}

static Expression* parse_postfix_ending(Parser* parser, Expression* start)
{
    static const TokenType operators[] = {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    static const size_t num_operators = countof(operators);

    Expression* expr = start;
    while (has_match(parser, operators, num_operators))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_LBRACKET:
            {
                Location lbracket_loc = consume(parser);
                Expression* member = parse_expression(parser);

                Location rbracket_loc;
                if (!try_match(parser, TOKEN_RBRACKET, &rbracket_loc))
                {
                    diagnostic_error_at(parser->dm, rbracket_loc,
                            "expected ']'");
                    rbracket_loc = LOCATION_INVALID;
                }

                expr = semantic_checker_handle_array_expression(&parser->sc,
                        expr, lbracket_loc, member, rbracket_loc);
                break;
            }

            case TOKEN_LPAREN:
            {
                Location lparen_loc = consume(parser);

                ExpressionList list = expression_list_create();
                if  (!is_match(parser, TOKEN_RPAREN))
                {
                    parse_argument_expression_list(parser, &list);
                }

                Location rparen_loc;
                if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
                {
                    diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
                    rparen_loc = LOCATION_INVALID;
                    recover(parser, TOKEN_RPAREN,
                            RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);

                    expr = semantic_checker_handle_error_expression(&parser->sc,
                            lparen_loc);
                    break;
                }
                
                expr = semantic_checker_handle_call_expression(&parser->sc,
                        expr, lparen_loc, &list, rparen_loc);
                break;
            }

            case TOKEN_DOT:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOKEN_IDENTIFIER))
                {
                    Location current = current_token_location(parser);
                    diagnostic_error_at(parser->dm, current,
                            "expected identifier after '.'");
                    expr = semantic_checker_handle_error_expression(
                            &parser->sc, op_loc);
                    break;
                }

                Identifier* identifier = current_token(parser)->data.identifier;
                Location identifier_loc = consume(parser);

                expr = semantic_checker_handle_member_expression(&parser->sc,
                        expr, op_loc, identifier, identifier_loc, true);
                break;
            }

            case TOKEN_ARROW:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOKEN_IDENTIFIER))
                {
                    Location current = current_token_location(parser);
                    diagnostic_error_at(parser->dm, current,
                            "expected identifier after '->'");
                    expr = semantic_checker_handle_error_expression(
                            &parser->sc, op_loc);
                    break;
                }

                Identifier* identifier = current_token(parser)->data.identifier;
                Location identifier_loc = consume(parser);

                expr = semantic_checker_handle_member_expression(&parser->sc,
                        expr, op_loc, identifier, identifier_loc, false);
                break;
            }

            case TOKEN_PLUS_PLUS:
            {
                Location op_loc = consume(parser);
                expr = semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_POST_INCREMENT, expr, op_loc);
                break;
            }

            case TOKEN_MINUS_MINUS:
            {
                Location op_loc = consume(parser);
                expr = semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_POST_DECREMENT, expr, op_loc);
                break;
            }
        }
    }

    return expr;
}

static Expression* parse_postfix_expression(Parser* parser)
{
    // How a handle compound literal is handled. Since the start of it looks
    // exactly like a cast expression we parse the cast first. Then since we see
    // a '{' we then parse the compound literal. After a compound literal we
    // should end up here. But since we that is hard we just pass in the 
    // compound literal expression and DON'T parse a primary expression. 
    Expression* expr = parse_primary_expression(parser);
    return parse_postfix_ending(parser, expr);
}

static Expression* parse_unary_expression(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOKEN_PLUS_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_INCREMENT, expr, op_loc);
        }

        case TOKEN_MINUS_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_DECREMENT, expr, op_loc);
        }

        case TOKEN_AND:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_address_expression(&parser->sc, expr,
                    op_loc);
        }

        case TOKEN_STAR:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_dereference_expression(&parser->sc,
                    expr, op_loc);
        }

        case TOKEN_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_PLUS, op_loc, expr);
        }

        case TOKEN_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_MINUS, op_loc, expr);
        }

        case TOKEN_TILDE:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
        }

        case TOKEN_NOT:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_NOT, op_loc, expr);
        }

        case TOKEN_SIZEOF:
        {
            Location sizeof_loc = consume(parser);
            
            if (is_match(parser, TOKEN_LPAREN) && 
                    is_typename_start(parser, next_token(parser)))
            {
                Location lparen_loc = consume(parser);
                QualifiedType type = parse_type_name(parser);
                Location rparen_loc;
                if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
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

        default:
            return parse_postfix_expression(parser);
    }
}

static Expression* parse_compound_literal(Parser* parser, Location lparen_loc,
        QualifiedType type, Location rparen_loc)
{
    assert(is_match(parser, TOKEN_LCURLY));

    bool file_scope = semantic_checker_current_scope_is(&parser->sc,
            SCOPE_FILE);
    Initializer* initializer = parse_initializer(parser);
    
    // TODO: create the compound literal
    Expression* compound_literal = semantic_checker_handle_compound_literal(
            &parser->sc, lparen_loc, type, rparen_loc, initializer);
    return parse_postfix_ending(parser, compound_literal);
}

static Expression* parse_cast_expression(Parser* parser)
{
    // If we cant possible have a cast expression just parse a unary expression
    if (!is_match(parser, TOKEN_LPAREN)
            || !is_typename_start(parser, next_token(parser)))
    {
        return parse_unary_expression(parser);
    }
    assert(is_match(parser, TOKEN_LPAREN));
    
    Location lparen_loc = consume(parser);
    
    QualifiedType type = parse_type_name(parser);
    
    Location rparen_loc;
    if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc,
                "expected ')' after type name");
    }

    // If we are parsing along and discover we in fact don't have a cast 
    // expression but rather a compound literal, make sure to parse our args
    // along and parse the compound literal. This will then correctly parse the
    // postfix expression after.
    if (is_match(parser, TOKEN_LCURLY))
    {
        return parse_compound_literal(parser, lparen_loc, type, rparen_loc);
    }

    Expression* expr = parse_cast_expression(parser);

    // Finally, create the cast expression for parsing.
    return semantic_checker_handle_cast_expression(&parser->sc, lparen_loc,
            type, rparen_loc, expr);
}

static Expression* parse_multiplicative_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_STAR, TOKEN_SLASH, 
            TOKEN_PERCENT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_cast_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_STAR:
                type = EXPRESSION_BINARY_TIMES;
                break;

            case TOKEN_SLASH:
                type = EXPRESSION_BINARY_DIVIDE;
                break;

            case TOKEN_PERCENT:
                type = EXPRESSION_BINARY_MODULO;
                break;
        }
        Location op_loc = consume(parser);
        Expression* rhs = parse_cast_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_additive_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_PLUS, TOKEN_MINUS};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_multiplicative_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_PLUS:
                type = EXPRESSION_BINARY_ADD;
                break;

            case TOKEN_MINUS:
                type = EXPRESSION_BINARY_SUBTRACT;
                break;
        }
        Location op_loc = consume(parser);
        Expression* rhs = parse_multiplicative_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_shift_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LT_LT, TOKEN_GT_GT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_additive_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_LT_LT:
                type = EXPRESSION_BINARY_SHIFT_LEFT;
                break;

            case TOKEN_GT_GT:
                type = EXPRESSION_BINARY_SHIFT_RIGHT;
                break;
        }
        Location op_loc = consume(parser);
        Expression* rhs = parse_additive_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_relational_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LT, TOKEN_GT, TOKEN_LT_EQUAL, 
            TOKEN_GT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_shift_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_LT: 
                type = EXPRESSION_BINARY_LESS_THAN;
                break;

            case TOKEN_GT: 
                type = EXPRESSION_BINARY_GREATER_THAN; 
                break;

            case TOKEN_LT_EQUAL: 
                type = EXPRESSION_BINARY_LESS_THAN_EQUAL;
                break;

            case TOKEN_GT_EQUAL: 
                type = EXPRESSION_BINARY_GREATER_THAN_EQUAL;
                break;
        }
        Location op_loc = consume(parser);
        Expression* rhs = parse_shift_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_equality_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_relational_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_EQUAL_EQUAL:
                type = EXPRESSION_BINARY_EQUAL;
                break;

            case TOKEN_NOT_EQUAL:
                type = EXPRESSION_BINARY_NOT_EQUAL;
                break;
        }
        
        Location op_loc = consume(parser);
        Expression* rhs = parse_relational_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_and_expression(Parser* parser)
{
    Expression* expr = parse_equality_expression(parser);

    while (is_match(parser, TOKEN_AND))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_equality_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_AND, expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_exclusive_or_expression(Parser* parser)
{
    Expression* expr = parse_and_expression(parser);

    while (is_match(parser, TOKEN_XOR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_and_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_XOR, expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_inclusive_or_expression(Parser* parser)
{
    Expression* expr = parse_exclusive_or_expression(parser);

    while (is_match(parser, TOKEN_OR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_exclusive_or_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_OR, expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_logical_and_expression(Parser* parser)
{
    Expression* expr = parse_inclusive_or_expression(parser);

    while (is_match(parser, TOKEN_AND_AND))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_inclusive_or_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_LOGICAL_AND, expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_logical_or_expression(Parser* parser)
{
    Expression* expr = parse_logical_and_expression(parser);

    while (is_match(parser, TOKEN_OR_OR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_logical_and_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc,
                EXPRESSION_BINARY_LOGICAL_OR, expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_conditional_expression(Parser* parser)
{
    Expression* expr = parse_logical_or_expression(parser);

    if (is_match(parser, TOKEN_QUESTION))
    {
        Location question = consume(parser);

        Expression* true_expr = parse_expression(parser);

        // Note: both clang and gcc seem to act as if colon existed anyways
        Location colon;
        if (!try_match(parser, TOKEN_COLON, &colon))
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

static Expression* parse_assignment_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_EQUAL, TOKEN_STAR_EQUAL, 
            TOKEN_SLASH_EQUAL, TOKEN_PERCENT_EQUAL, TOKEN_PLUS_EQUAL, 
            TOKEN_MINUS_EQUAL, TOKEN_LT_LT_EQUAL, TOKEN_GT_GT_EQUAL,
            TOKEN_AND_EQUAL, TOKEN_XOR_EQUAL, TOKEN_OR_EQUAL};
    static const size_t num_operators = countof(operators);

    // Here as an extension gcc does this and then just checks the conditional
    // is valid. I'm not sure how this qualifies as an extension since this
    // honestly just seems like an easier way to write a parser so I will do
    // the same thing.
    Expression* expr = parse_conditional_expression(parser);

    if (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOKEN_EQUAL: 
                type = EXPRESSION_BINARY_ASSIGN;
                break;

            case TOKEN_STAR_EQUAL: 
                type = EXPRESSION_BINARY_TIMES_ASSIGN;
                break;

            case TOKEN_SLASH_EQUAL: 
                type = EXPRESSION_BINARY_DIVIDE_ASSIGN;
                break;

            case TOKEN_PERCENT_EQUAL: 
                type = EXPRESSION_BINARY_MODULO_ASSIGN;
                break;

            case TOKEN_PLUS_EQUAL: 
                type = EXPRESSION_BINARY_ADD_ASSIGN;
                break;

            case TOKEN_MINUS_EQUAL: 
                type = EXPRESSION_BINARY_SUBTRACT_ASSIGN;
                break;

            case TOKEN_LT_LT_EQUAL: 
                type = EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN;
                break;

            case TOKEN_GT_GT_EQUAL: 
                type = EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN;
                break;

            case TOKEN_AND_EQUAL: 
                type = EXPRESSION_BINARY_AND_ASSIGN;
                break;

            case TOKEN_XOR_EQUAL: 
                type = EXPRESSION_BINARY_XOR_ASSIGN;
                break;

            case TOKEN_OR_EQUAL: 
                type = EXPRESSION_BINARY_OR_ASSIGN;
                break;
        }
        Location op_location = consume(parser);
        Expression* rhs = parse_assignment_expression(parser);

        expr = semantic_checker_handle_assignment_expression(&parser->sc, type,
                expr, op_location, rhs);
    }

    // Finalize the expression here before the comma expression since, we want
    // to make sure that all expressions in the comma expression are done 
    // correctly.
    return semantic_checker_expression_finalize(&parser->sc, expr);
}

static Expression* parse_constant_expression(Parser* parser)
{
    // if (expression_is_integer_constant(expr))
    // {
        // ExpressionIntegerValue value = {0};
        // expression_fold_to_integer_constant(parser->dm, expr, &value);
    // }

    // TODO: handle folding the constant expression...

    return parse_conditional_expression(parser);
}

static Expression* parse_expression(Parser* parser)
{
    Expression* expr = parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        Location comma_loc = consume(parser);
        Expression* rhs = parse_assignment_expression(parser);

        expr = semantic_checker_handle_comma_expression(&parser->sc, expr,
                comma_loc, rhs);
    }

    return expr;
}

// For parsing statements

static Statement* parse_statement_after_label(Parser* parser, const char* ctx)
{
    if (is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "label at end of compound statement");
        return semantic_checker_handle_error_statement(&parser->sc);
    }
    else if (is_typename_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "%s followed by a declaration", ctx);
        return semantic_checker_handle_error_statement(&parser->sc);
    }
    else if (!is_statement_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected expression after %s", ctx);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    return parse_statement(parser, false);
}

static Statement* parse_label_statement(Parser* parser)
{
    // Get the identifier from the current token.
    Identifier* identifier = current_token(parser)->data.identifier;
    Location label_loc = consume(parser);
    Location colon_loc = consume(parser);

    // If semantic checker errors due to label redefinition, do not continue.
    Declaration* label_decl = semantic_checker_act_on_label(&parser->sc,
            identifier, label_loc);
    if (!label_decl)
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* body = parse_statement_after_label(parser, "label");
    return semantic_checker_handle_label_statement(&parser->sc, label_loc,
            colon_loc, label_decl, body);
}

static Location parse_expected_colon(Parser* parser, const char* context)
{
    if (!is_match(parser, TOKEN_COLON))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ':' after '%s'", context);
        return LOCATION_INVALID;
    }

    return consume(parser);
}

static Statement* parse_case_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_CASE));

    Location case_loc = consume(parser);
    
    Expression* expr = parse_constant_expression(parser);
    if (!semantic_checker_check_case_expression(&parser->sc, &expr))
    {
        if (!is_match(parser, TOKEN_COLON))
        {
            recover_two(parser, TOKEN_COLON, TOKEN_RCURLY,
                    RECOVER_STOP_AT_SEMI);
            return semantic_checker_handle_error_statement(&parser->sc);
        }
    }

    // Parse and error about unsupported GCC extension of case ranges. Note, 
    // that this is not even inputted into the handling of case statements and 
    // that an error statement is created if we encounter this.
    Location dots = LOCATION_INVALID;
    Expression* rhs = NULL;
    if (is_match(parser, TOKEN_ELIPSIS))
    {
        dots = consume(parser);
        diagnostic_error_at(parser->dm, dots,
                "GNU case range extension not supported");

        rhs = parse_constant_expression(parser);
        if (!semantic_checker_check_case_expression(&parser->sc, &rhs))
        {
            if (!is_match(parser, TOKEN_COLON))
            {
                recover_two(parser, TOKEN_COLON, TOKEN_RCURLY,
                        RECOVER_STOP_AT_SEMI);
                return semantic_checker_handle_error_statement(&parser->sc);
            }
        }
    }

    Location colon_loc = parse_expected_colon(parser, "case");
    
    // TODO: i think I want to turn this into semantic_checker_add_case
    // TODO: and then if that errors then we error, otherwise parse the stmt
    if (!semantic_checker_check_case_allowed(&parser->sc, case_loc))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // If we got GNU case range don't attempt to parse a folling statement and
    // insteead create an error statement.
    if (dots != LOCATION_INVALID)
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* body = parse_statement_after_label(parser, "case label");
    return semantic_checker_handle_case_statement(&parser->sc, case_loc,
            expr, colon_loc, body);
}

static Statement* parse_default_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_DEFAULT));

    // Parse the statement
    Location default_loc = consume(parser);
    Location colon_loc = parse_expected_colon(parser, "default");

    // Check if we are allowed or not. Noting that the above function will
    // produce an error for us if it is not allowed.
    if (!semantic_checker_check_default_allowed(&parser->sc, default_loc))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* stmt = parse_statement_after_label(parser, "default label");
    return semantic_checker_handle_default_statement(&parser->sc, default_loc,
            colon_loc, stmt);
}

static Statement* parse_compound_statement_internal(Parser* parser)
{
    assert(is_match(parser, TOKEN_LCURLY));

    Location l_curly = consume(parser);

    Statement* first = NULL;
    Statement* current = NULL;
    if (!is_match(parser, TOKEN_RCURLY) && !is_match(parser, TOKEN_EOF))
    {
        first = parse_statement(parser, true);
        current = first;
    }

    while (!is_match(parser, TOKEN_RCURLY) && !is_match(parser, TOKEN_EOF))
    {
        // Parse the statement and then set the current's next field and then
        // update current to be the next one.
        Statement* next = parse_statement(parser, true);

        // Make sure to ignore completely empty statement like 'int;' so that
        // we don't segfault as these are represented as NULL
        if (next == NULL)
        {
            continue;
        }

        statement_set_next(current, next);
        current = next;
    }

    Location r_curly = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_RCURLY, &r_curly))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected '}'");
    }

    // Finally create the compound statement.
    return semantic_checker_handle_compound_statement(&parser->sc, l_curly,
            first, r_curly);
}

static Statement* parse_compound_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_LCURLY));

    // Set-up the new scope for declarations
    Scope scope = scope_block(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* stmt = parse_compound_statement_internal(parser);

    // Make sure to pop the scope at the end.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return stmt;
}

static Location parse_trailing_semi(Parser* parser, const char* context)
{
    if (!is_match(parser, TOKEN_SEMI))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ';' after %s", context);

        // Only attempt some recovery if we arent at the end of a compound stmt.
        // Note that if this is at top level it will emit an extraneous closing
        // brace warning which will need to be fixed anyways so I think this is
        // fine.
        recover(parser, TOKEN_RCURLY, RECOVER_STOP_AT_SEMI);
        
        return LOCATION_INVALID;
    }

    return consume(parser);
}

static Statement* parse_expression_statement(Parser* parser)
{
    // NOTE: this can be achieved through being called to parse a statement and
    // mathing nothing, when we should be matching the end of a compound stmt
    if (is_match(parser, TOKEN_RCURLY))
    {   
        Location err_loc = current_token_location(parser);
        diagnostic_error_at(parser->dm, err_loc, "expected statement");
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // NOTE: we may get here due to trying to parse a declaration statement as
    // the body of a for, if, while, ... So make sure we are actually an
    // expression. If we aren't, error and recover
    if (!is_expression_start(parser, current_token(parser)))
    {
        Location err_loc = current_token_location(parser);
        diagnostic_error_at(parser->dm, err_loc, "expected expression");
        recover_two(parser, TOKEN_RCURLY, TOKEN_SEMI, RECOVER_NONE);

        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(is_expression_start(parser, current_token(parser)));

    Expression* expr = parse_expression(parser);

    // Another clang special case error message which we support.
    if (is_match(parser, TOKEN_RPAREN) && is_next_match(parser, TOKEN_SEMI))
    {
        Location rparen = consume(parser);
        diagnostic_error_at(parser->dm, rparen, "extraneous ')' before ';'");
    }

    Location semi_loc = parse_trailing_semi(parser, "expression");
    return semantic_checker_handle_expression_statement(&parser->sc,
            expr, semi_loc);
}

static bool parse_expression_for_statement(Parser* parser, Location kw_location,
        Location* lparen_loc, Expression** cond, Location* rparen_loc,
        bool is_switch, const char* context)
{
    if (!is_match(parser, TOKEN_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected '(' after '%s'", context);
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return false;
    }

    *lparen_loc = consume(parser);

    // Parse and check the condition is valid
    *cond = parse_expression(parser);
    *cond = semantic_checker_check_condition(&parser->sc, kw_location, *cond,
            is_switch, context);

    if (!is_match(parser, TOKEN_RPAREN))
    {
        // TODO: could better error recovery here be to check for a statement
        // TODO: start somehow and then only sometimes killing the parse
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ')' after condition");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return false;
    }

    *rparen_loc = consume(parser);

    // Clang will emit errors for exsessive paren usage. But continue on parsing
    // as if there was no error, since this error is likely only limited to one
    // paren anyways
    while (is_match(parser, TOKEN_RPAREN))
    {   
        Location extra_paren = consume(parser);
        diagnostic_error_at(parser->dm, extra_paren,
                "extraneous ')' after condition, expected a statement");
    }

    return true;
}

static Statement* parse_if_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_IF));

    Statement* out = NULL;

    Location if_loc = consume(parser);

    Scope if_scope = scope_if(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &if_scope);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, if_loc, &lparen_loc, &cond,
            &rparen_loc, false, "if"))
    {
        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }

    // Check the condition is valid before parsing the body
    Statement* if_body = parse_statement(parser, false);

    Location else_loc = LOCATION_INVALID;
    Statement* else_body = NULL;
    if (is_match(parser, TOKEN_ELSE))
    {
        else_loc = consume(parser);
        else_body = parse_statement(parser, false);
    }

    out = semantic_checker_handle_if_statement(&parser->sc, if_loc,
            lparen_loc, cond, rparen_loc, if_body, else_loc, else_body);

done:
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&if_scope);

    return out;
}

static Statement* parse_switch_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_SWITCH));

    Statement* out = NULL;

    Location switch_loc = consume(parser);

    // Create and push the switch scope
    Scope switch_scope = scope_switch(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &switch_scope);

    Location lparen_loc = LOCATION_INVALID;
    Expression* expr = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, switch_loc, &lparen_loc, &expr,
            &rparen_loc, true, "switch"))
    {
        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }

    // Push the switch stack and parse the body of the switch statement.
    semantic_checker_push_switch_stack(&parser->sc);
    Statement* body = parse_statement(parser, false);
    semantic_checker_pop_switch_stack(&parser->sc);

    out = semantic_checker_handle_switch_statement(&parser->sc, switch_loc,
            lparen_loc, expr, rparen_loc, body);

done:
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&switch_scope);

    return out;
}

static Statement* parse_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_WHILE));

    Statement* out = NULL;

    Location while_loc = consume(parser);

    // Create and push the scope
    Scope while_scope = scope_while(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &while_scope);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, while_loc, &lparen_loc, &cond,
            &rparen_loc, false, "while"))
    {
        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }

    // Parse the body and set the inner while stmt
    Statement* body = parse_statement(parser, false);

    // Create our while statement
    out = semantic_checker_handle_while_statement(&parser->sc, while_loc,
            lparen_loc, cond, rparen_loc, body);

done:
    // Pop and delete the current scope.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&while_scope);

    return out;
}

static Statement* parse_do_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_DO));

    Statement* out = NULL;

    // Consume the 'do' and then push the scope.
    Location do_loc = consume(parser);

    Scope do_while_scope = scope_do_while(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &do_while_scope);

    Statement* body = parse_statement(parser, false);
    
    // If the token is not a while just skip till we get a semi...
    if (!is_match(parser, TOKEN_WHILE))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected 'while' in do/while loop");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }
    assert(is_match(parser, TOKEN_WHILE));

    // make sure we capture the while part.
    Location while_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, do_loc, &lparen_loc, &cond,
            &rparen_loc, false, "do/while"))
    {
        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }

    Location semi_loc = parse_trailing_semi(parser, "do/while statement");
    out = semantic_checker_handle_do_while_statement(&parser->sc, do_loc,
            body, while_loc, lparen_loc, cond, rparen_loc, semi_loc);

done:
    // Pop scope.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&do_while_scope);

    return out;
}

static Statement* parse_for_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_FOR));

    Location for_loc = consume(parser);

    // Make sure we got a lparen after!
    if (!is_match(parser, TOKEN_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected '(' after 'for'");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Create and push the for scope.
    Scope for_scope = scope_for(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &for_scope);

    assert(is_match(parser, TOKEN_LPAREN));
    Location lparen_loc = consume(parser);

    Statement* init = NULL;

    Declaration* init_declaration = NULL;
    Expression* init_expression = NULL;
    if (is_typename_start(parser, current_token(parser)))
    {   
        init_declaration = parse_declaration(parser, DECL_CTX_BLOCK, false);
    }
    else if (is_expression_start(parser, current_token(parser)))
    {
        init_expression = parse_expression(parser);
    }
    else if (!is_match(parser, TOKEN_SEMI))
    {
        // TODO: ensure this is adequete error recovery
        diagnostic_error_at(parser->dm, current_token_location(parser), 
                "expected expression");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        init = semantic_checker_handle_error_statement(&parser->sc);
    }

    if (!try_match(parser, TOKEN_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* cond = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        cond = parse_expression(parser);
    }

    if (!try_match(parser, TOKEN_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* inc = NULL;
    if (!is_match(parser, TOKEN_RPAREN))
    {
        inc = parse_expression(parser);
    }

    Location rparen_loc = LOCATION_INVALID;
    if (!is_match(parser, TOKEN_RPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ')'");
    }
    else
    {
        rparen_loc = consume(parser);
    }

    Statement* body = parse_statement(parser, false);

    // Pop the scope
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&for_scope);

    return semantic_checker_handle_for_statement(&parser->sc, for_loc,
            lparen_loc, init_declaration, init_expression, cond, inc,
            rparen_loc, body);
}

static Statement* parse_goto_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_GOTO));

    Location goto_loc = consume(parser);

    // Need to have an identifier next. GCC and clang have computed gotos as an
    // extension to the language but this is not supported here.
    if (!is_match(parser, TOKEN_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected identifier after 'goto'");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(is_match(parser, TOKEN_IDENTIFIER));

    // Get the identifier name and consume the identifier.
    Identifier* label_name = current_token(parser)->data.identifier;
    Location label_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "goto statement");

    return semantic_checker_handle_goto_statement(&parser->sc, goto_loc,
            label_name, label_loc, semi_loc);
}

static Statement* parse_continue_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_CONTINUE));

    Location continue_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "continue statement");

    return semantic_checker_handle_continue_statement(&parser->sc, continue_loc,
            semi_loc);
}

static Statement* parse_break_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_BREAK));

    Location break_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "break statement");

    return semantic_checker_handle_break_statement(&parser->sc, break_loc,
            semi_loc);
}

static Statement* parse_return_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_RETURN));

    Location return_loc = consume(parser);

    Expression* expr_opt = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        expr_opt = parse_expression(parser);
    }
    Location semi_loc = parse_trailing_semi(parser, "return statement");

    return semantic_checker_handle_return_statement(&parser->sc, return_loc,
            expr_opt, semi_loc);
}

static Statement* parse_declaration_statement(Parser* parser)
{
    // Choose block since we are known to be in one and can parse declarations
    // inside of it.
    Declaration* decl = parse_declaration(parser, DECL_CTX_BLOCK, false);
    Location semi_loc = parse_trailing_semi(parser, "declaration");

    return semantic_checker_handle_declaration_statement(&parser->sc,
            decl, semi_loc);
}

static Statement* parse_empty_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_SEMI));

    Location semi_loc = consume(parser);

    return semantic_checker_handle_empty_statement(&parser->sc, semi_loc);
}

static Statement* parse_error_statement(Parser* parser)
{
    recover_two(parser, TOKEN_RCURLY, TOKEN_SEMI, RECOVER_NONE);

    return semantic_checker_handle_error_statement(&parser->sc);
}

static Statement* parse_statement(Parser* parser, bool declaration_allowed)
{
    switch(current_token_type(parser))
    {
        case TOKEN_LCURLY:
            return parse_compound_statement(parser);

        case TOKEN_CASE:
            return parse_case_statement(parser);

        case TOKEN_DEFAULT:
            return parse_default_statement(parser);

        case TOKEN_IF:
            return parse_if_statement(parser);

        case TOKEN_SWITCH:
            return parse_switch_statement(parser);

        case TOKEN_WHILE:
            return parse_while_statement(parser);

        case TOKEN_DO:
            return parse_do_while_statement(parser);

        case TOKEN_FOR:
            return parse_for_statement(parser);

        case TOKEN_GOTO:
            return parse_goto_statement(parser);

        case TOKEN_CONTINUE:
            return parse_continue_statement(parser);

        case TOKEN_BREAK:
            return parse_break_statement(parser);

        case TOKEN_RETURN:
            return parse_return_statement(parser);

        case TOKEN_SEMI:
            return parse_empty_statement(parser);

        case TOKEN_IDENTIFIER:
            if (is_next_match(parser, TOKEN_COLON))
            {
                return parse_label_statement(parser);
            }
            
            /* FALLTHROUGH */

        default:
            if (is_typename_start(parser, current_token(parser))
                    && declaration_allowed)
            {
                // Note: declarations are not considered statements all of the
                // time in C. e.g. they are not allowed after labels.
                return parse_declaration_statement(parser);
            }
            return parse_expression_statement(parser);
    }
}

static bool is_designation_start(Parser* parser)
{
    return has_match(parser, (TokenType[2]) {TOKEN_DOT, TOKEN_LBRACKET}, 2);
}

static Designator* parse_member_designator(Parser* parser)
{
    assert(is_match(parser, TOKEN_DOT));

    Location dot = consume(parser);

    if (!is_match(parser, TOKEN_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected a field designator, such as '.field = 4'");
        return NULL;
    }

    Identifier* identifier = current_token(parser)->data.identifier;
    Location identifier_location = consume(parser);

    return designator_create_member(&parser->ast.ast_allocator, dot,
            identifier_location, identifier);
}

static Designator* parse_array_designator(Parser* parser)
{
    assert(is_match(parser, TOKEN_LBRACKET));

    Location lbracket = consume(parser);
    Expression* expr = parse_constant_expression(parser);
    Location rbracket;
    if (!try_match(parser, TOKEN_RBRACKET, &rbracket))
    {
        diagnostic_error_at(parser->dm, rbracket,
                "expected ']' after designator");
        recover(parser, TOKEN_RBRACKET,
                RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
    }

    // Create the array designator even if we didn't get the rbracket.
    return designator_create_array(&parser->ast.ast_allocator, lbracket, expr,
            rbracket);
}

static Designator* parse_designator(Parser* parser)
{
    assert(is_designation_start(parser));

    if (is_match(parser, TOKEN_DOT))
    {
        return parse_member_designator(parser);
    }
    return parse_array_designator(parser);
}

static DesignatorList* parse_designator_list(Parser* parser)
{
    assert(is_designation_start(parser));

    DesignatorList* start = NULL;
    DesignatorList* current = NULL;
    while (is_designation_start(parser))
    {
        // If parse designator fails to return a list, then recover to the 
        // closing curly of the initializer, so that we can recover quickly.
        Designator* designator = parse_designator(parser);
        if (designator == NULL)
        {
            recover(parser, TOKEN_RCURLY, RECOVER_STOP_AT_SEMI);
            return NULL;
        }

        DesignatorList* list = designator_list_create(
                &parser->ast.ast_allocator, designator);
        
        // Finally, build our designator list.
        if (start == NULL)
        {
            start = list;
        }
        current = designator_list_set_next(current, list);
    }
 
    return start;
}

static InitializerListMember* parse_initializer_list_member(Parser* parser)
{
    DesignatorList* list = NULL;
    Location equal_loc = LOCATION_INVALID;

    if (is_designation_start(parser))
    {
        // If this fails, recovery should have occured already.
        list = parse_designator_list(parser);
        if (list == NULL)
        {
            return NULL;
        }

        if (!try_match(parser, TOKEN_EQUAL, &equal_loc))
        {
            diagnostic_error_at(parser->dm, equal_loc,
                    "expected '=' or another designator");
            equal_loc = LOCATION_INVALID;
        }
    }

    // Finally, parse the initializer for either the member or just in general
    // to build our initializer list member.
    Initializer* init = parse_initializer(parser);
    if (init == NULL)
    {
        return NULL;
    }

    // Finally, create the initializer list member. And Return this to caller.
    return initializer_list_member_create(&parser->ast.ast_allocator, list,
            equal_loc, init);
}

static Initializer* parse_initializer_list(Parser* parser)
{   
    assert(is_match(parser, TOKEN_LCURLY));

    Location lcurly = consume(parser);
    Location rcurly;

    // See if we match an empty initializer list, and if it is then create and
    // empty list returning from the function early.
    if (try_match(parser, TOKEN_RCURLY, &rcurly))
    {
        diagnostic_warning_at(parser->dm, lcurly,
                "use of an empty initializer is a C23 extension");
        return semantic_checker_initializer_from_list(&parser->sc, lcurly,
                NULL, rcurly);
    }

    bool list_okay = true;
    InitializerListMember* first_member = NULL;
    InitializerListMember* current_member = NULL;
    do
    {
        // If we see the end of an initializer list then we are simply done.
        if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }

        // If we can match a designation do that...
        InitializerListMember* next = parse_initializer_list_member(parser);

        // If the list member had an error, continue to the next iteration.
        if (next == NULL)
        {
            list_okay = false;
            continue;
        }

        // Finally build our initializer list, adding the member onto the end
        // or the list.
        if (first_member == NULL)
        {
            first_member = next;
        }
        current_member = initializer_list_member_set_next(current_member, next);
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    // Try to match the end of the initializer
    if (!try_match(parser, TOKEN_RCURLY, &rcurly))
    {
        diagnostic_error_at(parser->dm, rcurly,
                "expected '}' after initializer");
        recover(parser, TOKEN_RCURLY, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
        rcurly = LOCATION_INVALID;
    }

    if (list_okay)
    {
        return semantic_checker_initializer_from_list(&parser->sc, lcurly,
            first_member, rcurly);
    }
    else
    {
        return NULL;
    }
}

static Initializer* parse_initializer_assignment_expression(Parser* parser)
{
    Expression* expression = parse_assignment_expression(parser);
    if (expression_is_invalid(expression))
    {
        return NULL;
    }

    return semantic_checker_initializer_from_expression(&parser->sc,
            expression);
}

static Initializer* parse_initializer(Parser* parser)
{
    if (is_match(parser, TOKEN_LCURLY))
    {
        return parse_initializer_list(parser);
    }
    return parse_initializer_assignment_expression(parser);
}

static void parse_declarator_internal(Parser* parser, Declarator* declarator)
{
    // If we get a pointer before our declarator do this first.
    if (is_match(parser, TOKEN_STAR))
    {
        Location star_location = consume(parser);
        TypeQualifiers qualifiers = parse_type_qualifier_list_opt(parser);

        // Hold off on pushing the pointer declarator. So that the correct
        // precedence of the declarators can be achieved. Instead we will 
        // just recurse so that we can parse other declarators first.
        parse_declarator_internal(parser, declarator);

        declarator_push_pointer(declarator, qualifiers);

        return;
    }
    
    parse_direct_declarator(parser, declarator);
}

static Declarator parse_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext ctx)
{
    Declarator declarator = declarator_create(specifiers, ctx,
            &parser->ast.ast_allocator);

    // If we don't allow bitfields or if we allow them and we don't match a ':'
    // parse a declarator how we would expect it to. Making sure to parse the
    // bitfield at the end if we need.
    bool bitfields = declarator_allowed_bitfields(&declarator);
    if (!bitfields || !is_match(parser, TOKEN_COLON))
    {
        parse_declarator_internal(parser, &declarator);
    }

    // To parse a bitfield we must first make sure that they area allowed then
    // we must also match a ':' to then try to parse one.
    if (bitfields && is_match(parser, TOKEN_COLON))
    {
        Location colon_loc = consume(parser);
        Expression* expr = parse_constant_expression(parser);
        declarator_add_bitfield(&declarator, colon_loc, expr);
    }

    return declarator;
}

static DeclarationList parse_knr_function_parameters(Parser* parser,
        DeclaratorPiece* piece)
{
    Scope prototype = scope_function_prototype(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &prototype);

    while (is_typename_start(parser, current_token(parser)))
    {
        DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
                false);

        // It is an error if we do not get a complete declaration after a param
        Location semi_loc = LOCATION_INVALID;
        if (try_match(parser, TOKEN_SEMI, &semi_loc))
        {
            diagnostic_error_at(parser->dm, semi_loc,
                    "declaration does not declare parameter");
            continue;
        }

        // Otherwise parse our declarators until we are done
        do
        {
            Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_KNR);
            semantic_checker_process_knr_param_defn(&parser->sc, &d, piece);
        }
        while (try_match(parser, TOKEN_COMMA, NULL));

        // parse the trailing semi-colon at the end of the declaration list.
        Location semi = LOCATION_INVALID;
        if (!try_match(parser, TOKEN_SEMI, &semi))
        {
            diagnostic_error_at(parser->dm, semi,
                    "expected ';' after declaration");
            recover(parser, TOKEN_RCURLY, RECOVER_STOP_AT_SEMI);
            if (is_match(parser, TOKEN_SEMI))
            {
                semi = consume(parser);
            }
        }
    }

    // Finally, finish off the declaraiton list by adding in any parameters that
    // were missing from here to be implicitly int
    DeclarationList decls = scope_get_declarations(&prototype);
    semantic_checker_handle_end_of_knr_parameters(&parser->sc, &decls, piece);

    // pop and delete the scope since we are done with it.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&prototype);

    return decls;
}

static void parse_skip_function_body(Parser* parser)
{
    // TODO: this...
    assert(is_match(parser, TOKEN_LCURLY));

    recover(parser, TOKEN_LCURLY, RECOVER_NONE);
}

static Declaration* parse_function_definition(Parser* parser,
        Declarator* declarator)
{
    assert(declarator_has_function(declarator));
    assert(!is_match(parser, TOKEN_SEMI));

    // Set that this is a function definition so that when we are handling the
    // declarator we handle it correctly.
    declarator_set_func_defn(declarator);

    // Checck for a knr function declaration. If we have it parse all of the
    // parameter declarations and override the declarators current parameters
    // with these new ones, so that when we add them into the function we get
    // the correct decls instead of just a bunch of implicit ints.
    DeclaratorPiece* piece = declarator_get_function_piece(declarator);
    if (declarator_piece_is_knr_function(piece))
    {
        DeclarationList parms = parse_knr_function_parameters(parser, piece);
        declarator_function_piece_set_all_decls(piece, parms);
    }

    // Now process the declaration.
    Declaration* function = semantic_checker_process_declarator(&parser->sc,
            declarator);

    // This here is only possible to get to if we had a knr function definition
    // that did not have a lcurly after it.
    if (!is_match(parser, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected function body after function declarator");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    // Create and push our function scope before we try to add our parameters.
    FunctionScope func_scope = function_scope_create(function);
    sematic_checker_push_function_scope(&parser->sc, &func_scope);

    Scope function_body = scope_block(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &function_body);

    // Add all of our important function parameters into this scope. Making sure
    // to use this declaration that we are currently parsing to avoid weird
    // errors.
    semantic_checker_add_function_parameters(&parser->sc, function);

    Statement* stmt = parse_compound_statement_internal(parser);

    // Finish the function by checking all of our labels...
    sematic_checker_act_on_end_of_function(&parser->sc);

    // Finally add the function definition to the function
    semantic_checker_handle_function_end(&parser->sc, function, stmt);

    // Delete our function scope and function body scope.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&function_body);

    sematic_checker_pop_function_scope(&parser->sc);
    function_scope_delete(&func_scope);

    // Finally return the function declaration
    return function;
}

static bool is_function_definition(Parser* parser, Declarator* declarator)
{
    assert(declarator_has_function(declarator));

    // Obvious case
    if (is_match(parser, TOKEN_LCURLY))
    {
        return true;
    }

    // Need to know if we are knr function. If we are then the function piece
    // will be a knr type piece. If not then we should have a typename after it.
    // NOTE: I belive implicit int is not allowed here.
    DeclaratorPiece* piece = declarator_get_function_piece(declarator);
    if (declarator_piece_is_knr_function(piece))
    {
        return is_typename_start(parser, current_token(parser));
    }

    return false;
}

static bool has_declaration(Parser* parser)
{
    static const TokenType tokens[] = {TOKEN_EQUAL, TOKEN_COMMA, TOKEN_SEMI};
    return has_match(parser, tokens, countof(tokens));
}

static void parse_initializer_after_declarator(Parser* parser,
        Declaration* declaration, DeclaratorContext context)
{
    // If we don't have an initializer then we can simple return.
    if (!is_match(parser, TOKEN_EQUAL))
    {
        return;
    }

    // Eat the equal token.
    Location equal_loc = consume(parser);

    // Otherwise parse the initializer checking we have the start of it before
    // we actively attempt to parse anything. Recover by going to the next
    // comma, or semi if possible as if we are in here we are always parsing
    // some kind of declaration list so this should never fail.
    if (!is_initializer_start(parser, current_token(parser)))
    {
        Location location = current_token_location(parser);
        diagnostic_error_at(parser->dm, location, "expected expression");
        recover(parser, TOKEN_COMMA, RECOVER_STOP_AT_SEMI);
        return;
    }

    // Finally, we know we can parse the initializer and try to add it to the
    // declaration of our choice.
    QualifiedType type = declaration_get_type(declaration);
    Initializer* initializer = parse_initializer(parser);
    semantic_checker_declaration_add_initializer(&parser->sc, declaration, 
            context, equal_loc, initializer);
}

static Declaration* parse_declaration_after_declarator(Parser* parser,
        Declarator* declarator, DeclaratorContext context)
{
    // Set if we have an initializer so that we can give correct diagnostics
    // about tentative definitions and such.
    if (is_match(parser, TOKEN_EQUAL))
    {
        declarator_set_initializer(declarator);
    }

    // Process the declaration so that we can then parse the initializer after
    Declaration* decl = semantic_checker_process_declarator(&parser->sc,
            declarator);

    // If we get a null declaration that means a bad error has occured so bail
    // early and recover to a reasonable point.
    if (decl == NULL)
    {
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    parse_initializer_after_declarator(parser, decl, context);
    semantic_checker_declaration_finish(&parser->sc, decl);

    return decl;
}

static bool maybe_parse_function(Parser* parser, Declarator* declarator,
        DeclaratorContext context, Declaration** declaration)
{
    // Check for a function definition being allowed here. We do that by seeing
    // if a declarator had a function declarator. An example of a function that
    // does not follow this is below:
    // 
    // typedef int foo(void);
    // foo func { ... } -> syntax error = "expected ';' ..."
    // 
    // Also note that both clang and gcc use this type of logic to determine if
    // we have a function definition. As before I was allowing the above invalid
    // case when I should have been regecting it.
    //
    // Additionally, clang seems to behave that if we don't have a '=', ',', or
    // ';', and we are a function declaration, then we MUST have a definiton.
    // So copy this behaviour.
    if (declarator_has_function(declarator) && !has_declaration(parser))
    {
        // Now check we have a valid context for being able to parse a function
        // definition. Knowing that it should be allow and that we might want
        // to have one.
        if (context == DECL_CTX_FILE)
        {
            if (!is_function_definition(parser, declarator))
            {
                // Here we are expecting a function definition base on the next
                // token. if it is not an '=' ',' or ';' then then we must have
                // a definition.
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "expected function body after function declarator");
                recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
                return true;
            }

            *declaration = parse_function_definition(parser, declarator);

            return true;
        }
        else
        {
            // Only error if we get the start of a function definition. This is
            // where we diverge from clang and meet up with gcc in that we will
            // consider knr parameter lists here...
            if (is_function_definition(parser, declarator))
            {
                // Otherwise we are not allowed to have a function at all, do 
                // not properly handle the declarator and instead error, recover
                // and return NULL
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "function definition is not allowed here");
                parse_skip_function_body(parser);
                return true;
            }
        }
    }

    return false;
}

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context)
{
    // First parse the declarator but do not create a declaration as we need to
    // delay this to potentially prevent some helpful but maybe irrelavent
    // diagnostics.
    Declarator declarator = parse_declarator(parser, specifiers, context);

    // First see if we are okay to parse a function. If so go and parse it
    // returning the function that we parsed or, NULL, if there was some error
    // to do with parsing the function.
    //
    // Note that even if maybe parse function returns true then that doesn't
    // necessarily mean that we got a function, but it does mean that we should
    // not keep trying to parse after this.
    Declaration* decl = NULL;
    if (maybe_parse_function(parser, &declarator, context, &decl))
    {
        return decl;
    }

    // Here we know that we shouldn't create a function definition and so we
    // can finally process the declarator and potentially try to parse an
    // initializer after the definition.
    decl = parse_declaration_after_declarator(parser, &declarator,
            context);

    // Otherwise keep trying to parse declarations with an initializer until
    // we appear to be at the end of all of our declarations.
    while (try_match(parser, TOKEN_COMMA, NULL))
    {
        // Parse a fresh declarator and then a declaration after it.
        declarator = parse_declarator(parser, specifiers, context);
        decl = parse_declaration_after_declarator(parser, &declarator, context);
    }
    
    return decl;
}

static void parse_identifier_list(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LPAREN));

    Location lparen_loc = consume(parser);
    Location rparen_loc = LOCATION_INVALID;

    // Create the list of parameter declarations that we will use to add our
    // 'declarations' too.
    DeclarationList parms = declaration_list_create(&parser->ast.ast_allocator);
    size_t num_parms = 0;

    // See if we got an empty parameter list. This is still an old style
    // declaration, however, we are just trying to skip doing a whole bunch of
    // work that we don't have to do
    if (is_match(parser, TOKEN_RPAREN))
    {
        rparen_loc = consume(parser);
        goto done;
    }

    do
    {
        if (!is_match(parser, TOKEN_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected identifier");
            recover(parser, TOKEN_RPAREN, RECOVER_NONE);
            break;
        }

        Token* identifier_token = current_token(parser);
        Identifier* identifier = identifier_token->data.identifier;
        Location identifier_loc = consume(parser);

        // Process the declaration and create a new fake declaration for us
        Declaration* decl = semantic_checker_process_knr_param(&parser->sc,
                identifier, identifier_loc);
        if (decl != NULL)
        {
            declaration_list_push(&parms, decl);
            num_parms++;
        }
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
        recover(parser, TOKEN_RPAREN, RECOVER_STOP_AT_SEMI);
    }

done:
    declarator_push_function(declarator, lparen_loc, rparen_loc, parms,
            num_parms, parms, LOCATION_INVALID, true);
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    // Parse declaration specifiers
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
            false);

    // Parse the function paramater declarator
    Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_PARAM);
    Declaration* declaration = semantic_checker_process_function_param(
            &parser->sc, &d);

    return declaration;
}

static void parse_paramater_type_list(Parser* parser, Declarator* declarator)
{
    // Make sure we are actually parsing this correctly
    assert(is_match(parser, TOKEN_LPAREN));
    Location lparen_loc = consume(parser);

    // The location of the dots and if the function is variadic
    Location dots = LOCATION_INVALID;

    // The paramater declarations themselves
    DeclarationList parms = declaration_list_create(&parser->ast.ast_allocator);
    size_t num_parms = 0;
    do
    {
        // First check if we have elipsis
        if (is_match(parser, TOKEN_ELIPSIS))
        {
            dots = consume(parser);

            // Check that we have a parameter. If not error, but we will try to
            // recover and ignore the error to be a bit better
            if (num_parms == 0)
            {
                diagnostic_error_at(parser->dm, dots,
                        "ISO C requires a named parameter before '...'");
            }
            break;
        }

        if (!is_typename_start(parser, current_token(parser)))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected parameter declarator");
            continue;
        }

        // Otherwise we will try to parse a declaration.
        Declaration* declaration = parse_paramater_declaration(parser);

        // Push the paramater into the list.
        declaration_list_push(&parms, declaration);
        num_parms++;
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    // Match the end of the parameter list
    if (!is_match(parser, TOKEN_RPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ')'");
        recover(parser, TOKEN_RPAREN, RECOVER_STOP_AT_SEMI);
    }
    Location rparen_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_RPAREN))
    {
        rparen_loc = consume(parser);
    }

    // Push the function type onto the end of the declarator
    Scope* current = semantic_checker_current_scope(&parser->sc);
    DeclarationList all_decls = scope_get_declarations(current);
    
    declarator_push_function(declarator, lparen_loc, rparen_loc, parms, 
            num_parms, all_decls, dots, false);
}

static void parse_function_declarator(Parser* parser, Declarator* declarator)
{
    // Push our function prototype scope so that we can easily reject parameters
    // with duplicate names.
    Scope function_proto = scope_function_prototype(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &function_proto);

    // Note the check for elipsis is explained by parse direct-declarator
    bool typename = is_typename_start(parser, next_token(parser));

    // Note: we check for elipsis to help improve a possible bad parse's error
    // mesasage so that we parse it correctly as a function declarator.
    if (typename || is_next_match(parser, TOKEN_ELIPSIS))
    {
        parse_paramater_type_list(parser, declarator);
    }
    else
    {
        parse_identifier_list(parser, declarator);
    }

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&function_proto);
}

static void parse_array_declarator(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LBRACKET));

    // TODO: I think it would be interesting here to parse the differently
    // TODO: depending on if static / type-qualifiers are allowed. Since it
    // TODO: would improve error messages. However, it is more of a semantic
    // TODO: checker role
    // TODO: Also, maybe we would not parse static if we KNOW that we are an
    // TODO: abstract declarator. Since that could be considered something to
    // TODO: be handled by parsing. 

    Location lbracket_loc = consume(parser);
    
    Location static_loc = LOCATION_INVALID;
    bool is_static = false;
    if (is_match(parser, TOKEN_STATIC))
    {
        static_loc = consume(parser);
        is_static = true;
    }

    // Get the type qualifiers if any...
    TypeQualifiers qualifiers = parse_type_qualifier_list_opt(parser);

    // Test again for static, erroring if we got it twice
    if (is_match(parser, TOKEN_STATIC) && !is_static)
    {
        static_loc = consume(parser);
        is_static = true;
    }
    else if (is_match(parser, TOKEN_STATIC) && is_static)
    {
        Location duplicate = consume(parser);
        diagnostic_error_at(parser->dm, duplicate,
                "duplicate 'static' array qualifier");
        recover(parser, TOKEN_RBRACKET, RECOVER_STOP_AT_SEMI);
    }

    // Make sure we don't accidentally match the start of a dereference
    // expression. Since that would be important.
    bool is_star = false;
    if (is_match(parser, TOKEN_STAR) && is_next_match(parser, TOKEN_RBRACKET))
    {
        consume(parser);

        is_star = true;
    }

    Expression* expression = NULL;
    if (!is_match(parser, TOKEN_RBRACKET))
    {
        expression = parse_assignment_expression(parser);
    }

    Location rbracket_loc = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_RBRACKET, &rbracket_loc))
    {
        diagnostic_error_at(parser->dm, rbracket_loc, "expected ']'");
        recover(parser, TOKEN_RBRACKET, RECOVER_STOP_AT_SEMI);
        return; // ???
    }

    declarator_push_array(declarator, lbracket_loc, rbracket_loc, static_loc,
            qualifiers, expression, is_static, is_star);
}

static void parse_direct_declarator(Parser* parser, Declarator* declarator)
{
    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        // Get and add the identifier if it is allowed to be in this declarator
        Identifier* identifier = current_token(parser)->data.identifier;
        Location loc = consume(parser);

        if (declarator_identifier_allowed(declarator))
        {
            declarator_set_identifier(declarator, identifier, loc);
        }
        else
        {
            // Don't recover as we will just pretend this identifier didn't
            // exist. That is the recovery.
            DeclaratorContext ctx = declarator_get_context(declarator);
            diagnostic_error_at(parser->dm, loc, "%s cannot have a name",
                    declarator_context_to_name(ctx));
        }
    }
    else if (is_match(parser, TOKEN_LPAREN))
    {
        // If the next token is a typename then we don't want to parse the
        // bracket part of the declaration like this. We actually want to parse
        // it as a function declarator
        //
        // Below is not parsed here and is skipped here
        // 1. void(int)
        //
        // But this one is parse here (if identifier is valid here) but will
        // later be rejected by semantic analysis
        // 2. void(a, b, c)
        //
        // Also this one below should be parsed as a function type
        // 3. void()
        //
        // Note: the check for an elipsis match is there to improve diagnostics
        // since that construct is valid in C23, and it would be nice to handle
        // the parsing a bit better.
        if (!declarator_identifier_required(declarator)
                && (is_typename_start(parser, next_token(parser))
                || is_next_match(parser, TOKEN_RPAREN)
                || is_next_match(parser, TOKEN_ELIPSIS)))
        {
            // Here like the case below we also want to set the identifier.
            // e.g. in the below
            // int foo(int(int))
            //            ^ we are here. This is where we would want to set the
            //              location of the identifier for.
            // Note: clang sets the location on the token after the '(' but it
            // makes more sense to be to set it at the arrow as that is where
            // the identifier would have been.
            Location location = current_token_location(parser);
            declarator_set_identifier(declarator, NULL, location);

            goto parse_func_array;
        }

        Location lparen_loc = consume(parser);

        parse_declarator_internal(parser, declarator);
        
        if (!is_match(parser, TOKEN_RPAREN))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected ')'");
            recover(parser, TOKEN_RPAREN,
                    RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
            if (!is_match(parser, TOKEN_RPAREN))
            {
                return;
            }
        }
        Location rparen_loc = consume(parser);
    }
    else if (!declarator_identifier_required(declarator))
    {
        // 'Set' the declarators location so that we can provide warning about
        // functions not have parameter names if we need.
        Location location = current_token_location(parser);
        declarator_set_identifier(declarator, NULL, location);
    }
    else
    {
        // Give an appriopriate diagnostic based on the current context. Noting
        // that we are erroring since we expected an identifier at some point
        // but never ended up getting one.
        DeclaratorContext context = declarator_get_context(declarator);
        const char* const message = context == DECL_CTX_STRUCT
                ? "expected member name or ';' after declaration specifiers"
                : "expected identifier or '('";
        Location current = current_token_location(parser);
        diagnostic_error_at(parser->dm, current, message);

        declarator_set_invalid(declarator);
        return;
    }

    // Now finish parsing the declarator.
parse_func_array:
    while (true)
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            parse_function_declarator(parser, declarator);
            continue;
        }
        
        if (is_match(parser, TOKEN_LBRACKET))
        {
            parse_array_declarator(parser, declarator);
            continue;
        }

        // If were not a function or array declarator assume we are safe to be
        // done
        break;
    }
}

static TypeQualifiers parse_type_qualifier_list(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    // Set up a declaration spec with no type qualifiers initially
    DeclarationSpecifiers qualifiers = { .qualifiers = QUALIFIER_NONE };
    do
    {
        TypeQualifiers qualifier = QUALIFIER_NONE;
        switch (current_token_type(parser))
        {
            case TOKEN_CONST: qualifier = QUALIFIER_CONST; break;
            case TOKEN_RESTRICT: qualifier = QUALIFIER_RESTRICT; break;
            case TOKEN_VOLATILE: qualifier = QUALIFIER_VOLATILE; break;
            default: panic("unreachable"); break;
        }

        Location location = consume(parser);
        declaration_specifiers_add_qualifier(parser, &qualifiers, qualifier,
                location);
    }
    while (has_match(parser, type_qualifier, type_qualifier_count));

    return qualifiers.qualifiers;
}

static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser)
{
    if (!has_match(parser, type_qualifier, type_qualifier_count))
    {
        return QUALIFIER_NONE;
    }

    return parse_type_qualifier_list(parser);
}

// Simply parse declaration specifiers and remove typenames and function
// specifiers. Simpler than diagnosing in declaration specifiers but doing that
// there would be a bit nicer and more informative I think
static DeclarationSpecifiers parse_specifier_qualifier_list(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
            true);
    assert(specifiers.storage_spec == STORAGE_NONE);
    assert(specifiers.function_spec == FUNCTION_SPECIFIER_NONE);

    return specifiers;
}

static void parse_struct_declaration(Parser* parser, Declaration* decl)
{
    assert(declaration_is(decl, DECLARATION_STRUCT) ||
            declaration_is(decl, DECLARATION_UNION));

    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);

    // Struct declarations will always need to have a name to be useful. Unless
    // we implement c11's anonymous struct/union injection into the scope. But,
    // we are currently a C99 only compiler and barely implement structs as is,
    // so this is a little far off for use.
    if (is_match(parser, TOKEN_SEMI))
    {
        Location loc = consume(parser);
        diagnostic_warning_at(parser->dm, loc,
                "declaration does not declare anything");
        return;
    }

    // Main loop of parsing all of the declarators for this declaration and 
    // adding them into the structure if they are what we are looking for.
    do
    {
        Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_STRUCT);
        Declaration* member = semantic_checker_process_struct_declarator(
                &parser->sc, decl, &d);

        if (member != NULL)
        {
            declaration_struct_add_member(decl, member);
        }
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    Location semi = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_SEMI, &semi))
    {
        diagnostic_error_at(parser->dm, semi,
                "expected ';' after struct declaration");
        if (!is_match(parser, TOKEN_RCURLY))
        {
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        }
    }
}

static void parse_struct_declaration_list(Parser* parser, Declaration* decl,
        bool is_struct)
{
    assert(is_match(parser, TOKEN_LCURLY));
    
    Location l_curly = consume(parser);
    Location r_curly = LOCATION_INVALID;
    if (try_match(parser, TOKEN_RCURLY, &r_curly))
    {
        diagnostic_error_at(parser->dm, r_curly, "empty %s is not supported",
                is_struct ? "struct" : "union");
        return;
    }

    Scope member_scope = scope_member(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &member_scope);

    while (!is_match_two(parser, TOKEN_RCURLY, TOKEN_EOF))
    {
        // Consume any extra ';' that appear inside the struct and warn about
        // them.
        if (is_match(parser, TOKEN_SEMI))
        {
            Location sem = consume(parser);
            while (try_match(parser, TOKEN_SEMI, NULL))
                ;
            diagnostic_warning_at(parser->dm, sem, "extra ';' inside a struct");
            continue;
        }

        if (!is_typename_start(parser, current_token(parser)))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "type name requires a specifier or qualifier");
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
            continue;
        }

        // Otherwise we can parse a struct declaration.
        parse_struct_declaration(parser, decl);
    }

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&member_scope);

    // Try to match the closign '}'
    if (!try_match(parser, TOKEN_RCURLY, &r_curly))
    {
        Location current = current_token_location(parser);
        diagnostic_error_at(parser->dm, current, "expected '}'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
    }

    semantic_checker_finish_struct_declaration(&parser->sc, decl);
}

static void parse_struct_or_union_specifier(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    assert(is_match_two(parser, TOKEN_STRUCT, TOKEN_UNION));

    Declaration* declaration = NULL;
    bool error = false;

    bool is_struct = is_match(parser, TOKEN_STRUCT);
    DeclarationType type = is_struct ? DECLARATION_STRUCT : DECLARATION_UNION;

    Location tag_loc = consume(parser);

    if (!is_match_two(parser, TOKEN_IDENTIFIER, TOKEN_LCURLY))
    {
        const char* const tag_name = is_struct ? "struct" : "union";
        diagnostic_error_at(parser->dm, tag_loc,
                "declaration of anonymous %s must be a definition", tag_name);
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        goto finish;
    }

    Identifier* identifier = NULL;
    Location identifier_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        identifier = current_token(parser)->data.identifier;
        identifier_loc = consume(parser);
    }

    // The action we are going to do in the semantic checker depends on if we
    // are wanting to parse a definition. Some of the following cases apply
    //
    // Below we want to use the existing declaration:
    // 1.     struct foo { int a; }; void func(void) { struct foo a; }
    // 2.     struct foo { int a; }; void func(void) { struct foo; }
    //
    // Whereas here we actually create a new declaration of enum foo
    // 3.   struct foo { int a; }; void func(void) { struct foo { int a; }; }
    bool is_definition = is_match(parser, TOKEN_LCURLY);
    declaration = semantic_checker_handle_tag(&parser->sc, type, type,
            identifier, identifier_loc, is_definition);
    assert(declaration != NULL);

    // Parse the definition if we have one to parse.
    if (is_definition)
    {
        parse_struct_declaration_list(parser, declaration, is_struct);
    }

    // Finally, we will need to add the struct / union specifiers and 
    // declaration to the declspec.
finish:;
    TypeSpecifierType type_spec;
    if (error)
    {
        type_spec = TYPE_SPECIFIER_ERROR;
    }
    else if (is_struct)
    {
        type_spec = TYPE_SPECIFIER_STRUCT;
    }
    else
    {
        type_spec = TYPE_SPECIFIER_UNION;
    }
    declaration_specifiers_add_type(parser, specifiers, type_spec,
            declaration, tag_loc);
}

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl)
{
    assert(enum_decl && declaration_is(enum_decl, DECLARATION_ENUM));
    assert(is_match(parser, TOKEN_LCURLY));

    // Here we can actually parse the enum
    Location opening_curly = consume(parser);

    // Check for an empty enum, setting it to be a complete declaration if so
    Location closing_curly = LOCATION_INVALID;
    if (try_match(parser, TOKEN_RCURLY, &closing_curly))
    {
        diagnostic_error_at(parser->dm, closing_curly, "use of empty enum");
        declaration_enum_set_entries(enum_decl, NULL, 0);
        return;
    }

    Declaration* previous = NULL;
    // Make sure we don't infinitely loop
    while (!is_match(parser, TOKEN_EOF))
    {
        // Here parse the enumerator declaration placing it in the symbol table
        if (!is_match(parser, TOKEN_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected identifier");
            recover_three(parser, TOKEN_COMMA, TOKEN_RCURLY, TOKEN_IDENTIFIER,
                    RECOVER_STOP_AT_SEMI);

            if (is_match(parser, TOKEN_COMMA))
            {
                consume(parser);
                continue;
            }
                
            if (is_match(parser, TOKEN_RCURLY))
            {
                break;
            }
        }

        // Due to previous potential error recovery
        assert(is_match(parser, TOKEN_IDENTIFIER));

        Identifier* identifier = current_token(parser)->data.identifier;
        Location identifier_loc = consume(parser);

        // Get the expresison first since we cannot build the enum value with
        // the identifier itself
        Location equal_loc = LOCATION_INVALID;
        Expression* expression = NULL;
        if (is_match(parser, TOKEN_EQUAL))
        {
            equal_loc = consume(parser);
            expression = parse_constant_expression(parser);
        }

        // Create the declaration. Note, this handles any redefinitions that
        // we might get which is nice and convenient.
        Declaration* constant_decl = semantic_checker_handle_enum_constant(
                &parser->sc, identifier_loc, identifier, equal_loc, expression,
                previous);
        // TODO: add to some vector somewhere...
        previous = constant_decl;

        // Now see if we are at the end and finish the definition  
        if (!is_match_two(parser, TOKEN_RCURLY, TOKEN_COMMA))      
        {
            if (is_match(parser, TOKEN_IDENTIFIER))
            {
                // Common error, no recovery needed, just restart loop
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "missing ',' between enumerators");
                continue;
            }
            
            if (is_match(parser, TOKEN_SEMI))
            {
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "expected '= constant-expression' or end of "
                        "enumerator definition");
                break;
            }
            
            // Otherwise, recover by finding the next enumerator or the end
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected ',' or '}' after enumerator");
            recover_two(parser, TOKEN_COMMA, TOKEN_RCURLY,
                    RECOVER_STOP_AT_SEMI);
        }

        if (is_match(parser, TOKEN_COMMA))
        {
            consume(parser);
        }

        if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }
    }

    declaration_enum_set_entries(enum_decl, NULL, 0);

    closing_curly = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_RCURLY, &closing_curly))
    {
        diagnostic_error_at(parser->dm, closing_curly, "expected '}'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
    }
}

static void parse_enum_specificier(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    assert(is_match(parser, TOKEN_ENUM));

    // Track if we got a fatal error whilst parsing the enum. and pre-init the
    // declaration.
    Declaration* declaration = NULL;
    bool error = false;

    Location enum_location = consume(parser);

    // We need to have a match for one of these here otherwise we have a problem
    if (!is_match_two(parser, TOKEN_IDENTIFIER, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser), 
                "expected identifier or '{' after 'enum'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        error = true;
        goto finish;
    }

    Identifier* identifier = NULL;
    Location identifier_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        identifier = current_token(parser)->data.identifier;
        identifier_loc = consume(parser);
    }

    // The action we are going to do in the semantic checker depends on if we
    // are wanting to parse a definition. Some of the following cases apply
    //
    // Below we want to use the existing declaration:
    // 1.     enum foo {X, Y, Z}; void func(void) { enum foo a; }
    // 2.     enum foo {X, Y, Z}; void func(void) { enum foo; }
    //
    // Whereas here we actually create a new declaration of enum foo
    // 3.     enum foo {X, Y, Z}; void func(void) { enum foo {A, B, C}; }
    bool is_definition = is_match(parser, TOKEN_LCURLY);
    declaration = semantic_checker_handle_tag(&parser->sc, DECLARATION_ENUM,
            enum_location, identifier, identifier_loc, is_definition);
    assert(declaration != NULL);

    // Parse the enumerator list if we have one.
    if (is_definition)
    {
        parse_enumerator_list(parser, declaration);
    }

    // Finally, we will need to add the struct / union specifiers and 
    // declaration to the declspec.

finish:;
    TypeSpecifierType type = error ? TYPE_SPECIFIER_ERROR : TYPE_SPECIFIER_ENUM;
    declaration_specifiers_add_type(parser, specifiers, type, declaration,
            enum_location);
}

static bool try_parse_typename_specifier(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    assert(is_match(parser, TOKEN_IDENTIFIER));

    Token* current = current_token(parser);
    if (!is_typename_start(parser, current))
    {
        return false;
    }

    Identifier* id = current->data.identifier;
    Declaration* typename = semantic_checker_get_typename(&parser->sc, id);
    declaration_specifiers_add_type(parser, specifiers, TYPE_SPECIFIER_TYPENAME,
            typename, current_token_location(parser));

    return true;
}

static QualifiedType parse_type_name(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);
    Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_TYPE_NAME);
    QualifiedType type = semantic_checker_process_typename(&parser->sc, &d);

    return type;
}

static void declaration_specifiers_add_storage(Parser* parser,
        DeclarationSpecifiers* specifiers, StorageSpecifier storage,
        bool spec_qual_only, Location location)
{
    // First error if we only got specifier qualifier but got a storage class
    if (spec_qual_only)
    {
        diagnostic_error_at(parser->dm, location, "type name does not allow "
                "storage class to be specified (got '%s')",
                storage_specifier_to_name(storage));
        return;
    }

    // If we haven't recieved a storage specifier we can add it and be done.
    if (specifiers->storage_spec == STORAGE_NONE)
    {
        specifiers->storage_spec = storage;
    }
    else if (specifiers->storage_spec == storage)
    {
        diagnostic_warning_at(parser->dm, location,
                "duplicate '%s' storage specifier",
                storage_specifier_to_name(storage));
    }
    else
    {
        diagnostic_error_at(parser->dm, location,
                "cannot combine '%s' with previous '%s' storage specifier",
                storage_specifier_to_name(storage),
                storage_specifier_to_name(specifiers->storage_spec));
    }
}

static void declaration_specifiers_add_qualifier(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeQualifiers qualifier,
        Location location)
{
    if (specifiers->qualifiers & qualifier)
    {
        diagnostic_warning_at(parser->dm, location,
                "duplicate '%s' type qualifier",
                type_qualifier_to_name(qualifier));
    }

    specifiers->qualifiers |= qualifier;
}

static void declaration_specifiers_add_function(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeFunctionSpecifier function,
        bool spec_qual_only, Location location)
{
    // Error if we got a function specifier but it wasnt allowed to be here
    if (spec_qual_only)
    {
        diagnostic_error_at(parser->dm, location, "type name does not allow "
                "function specifier to be specified (got '%s')",
                function_specifier_to_name(function));
        return;
    }

    if (specifiers->function_spec & function)
    {
        diagnostic_warning_at(parser->dm, location,
                "duplicate '%s' function specifier",
                function_specifier_to_name(function));
    }

    specifiers->function_spec |= function;
}

static void declaration_specifiers_add_width(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierWidth width,
        Location location)
{
    // Here we differ from clang which allows duplicate short specifier
    if (specifiers->type_spec_width == WIDTH_SPECIFIER_NONE)
    {
        specifiers->type_spec_width = width;
    }
    else if (specifiers->type_spec_width == WIDTH_SPECIFIER_LONG
            && width == WIDTH_SPECIFIER_LONG_LONG)
    {
        specifiers->type_spec_width = width;
    }
    else
    {
        diagnostic_error_at(parser->dm, location,
                "cannot combine '%s' with previous '%s' width specifier",
                width_specifier_to_name(width),
                width_specifier_to_name(specifiers->type_spec_width));
    }
}

static void declaration_specifiers_add_sign(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierSign sign,
        Location location)
{
    if (specifiers->type_spec_sign == SIGN_SPECIFIER_NONE)
    {
        specifiers->type_spec_sign = sign;
    }
    else if (specifiers->type_spec_sign == sign)
    {
        diagnostic_warning_at(parser->dm, location,
                "got duplicate '%s' sign specifier",
                sign_specifier_to_name(sign));
    }
    else
    {
        diagnostic_error_at(parser->dm, location,
                "cannot combine '%s' with previous '%s' sign specifier",
                sign_specifier_to_name(sign),
                sign_specifier_to_name(specifiers->type_spec_sign));
    }
}

static void declaration_specifiers_add_complex(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierComplex complex,
        Location location)
{
    if (specifiers->type_spec_complex == COMPLEX_SPECIFIER_NONE)
    {
        specifiers->type_spec_complex = complex;
    }
    else if (specifiers->type_spec_complex == complex)
    {
        diagnostic_warning_at(parser->dm, location,
                "got duplicate '%s' complex specifier",
                complex_specifier_to_name(complex));
    }
    else
    {
        diagnostic_error_at(parser->dm, location,
                "cannot combine '%s' with previous '%s' complex specifier",
                complex_specifier_to_name(complex),
                complex_specifier_to_name(specifiers->type_spec_complex));
    }
}

static void declaration_specifiers_add_declaration(
        DeclarationSpecifiers* specifiers, Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_STRUCT)
            || declaration_is(declaration, DECLARATION_UNION)
            || declaration_is(declaration, DECLARATION_ENUM)
            || declaration_is(declaration, DECLARATION_TYPEDEF));

    // Add both the declaration and the type into the declaration specifiers.
    // TODO: this feels a bit weird, maybe more checks are needed?
    if (specifiers->declaration == NULL && specifiers->type == NULL)
    {
        specifiers->declaration = declaration;
        specifiers->type = declaration->base.qualified_type.type;
    }
}

static void declaration_specifiers_add_type(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierType type,
        Declaration* declaration_opt, Location location)
{
    if (specifiers->type_spec_type == TYPE_SPECIFIER_NONE)
    {
        specifiers->type_spec_type = type;
    }
    else if (specifiers->type_spec_type == TYPE_SPECIFIER_ERROR
            || type == TYPE_SPECIFIER_ERROR)
    {
        // TODO: be silent on bad error case?
        // I think this is the best option and set the type to error
        specifiers->type_spec_type = type;
    }
    else
    {
        if (specifiers->type_spec_type != type)
        {
            diagnostic_error_at(parser->dm, location,
                    "cannot combine '%s' with previous '%s' type specifier",
                    type_specifier_to_name(type),
                    type_specifier_to_name(specifiers->type_spec_type));
        }
        else
        {
            diagnostic_error_at(parser->dm, location,
                    "got duplicate '%s' type specifier",
                    type_specifier_to_name(type));
        }
    }

    if (declaration_opt)
    {
        declaration_specifiers_add_declaration(specifiers, declaration_opt);
    }
}

static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser,
        bool spec_qual_only)
{
    Location start = current_token_location(parser);
    DeclarationSpecifiers specifiers = declaration_specifiers_create(start);

    while (true)
    {
        // Get the location of this token we are about to use.
        Location location = current_token_location(parser);

        switch (current_token_type(parser))
        {
            // Storage specifiers
            case TOKEN_TYPEDEF:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_TYPEDEF, spec_qual_only, location);
                break;

            case TOKEN_EXTERN:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_EXTERN, spec_qual_only, location);
                break;

            case TOKEN_STATIC:
               declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_STATIC, spec_qual_only, location);
                break;

            case TOKEN_AUTO:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_AUTO, spec_qual_only, location);
                break;

            case TOKEN_REGISTER:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_REGISTER, spec_qual_only, location);
                break;

            // Qualifiers
            case TOKEN_CONST:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_CONST, location);
                break;

            case TOKEN_VOLATILE:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_VOLATILE, location);
                break;

            case TOKEN_RESTRICT:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_RESTRICT, location);
                break;
                
            // Function specifier
            case TOKEN_INLINE:
                declaration_specifiers_add_function(parser, &specifiers,
                        FUNCTION_SPECIFIER_INLINE, spec_qual_only, location);
                break;

            // Width specifiers
            case TOKEN_SHORT:
                declaration_specifiers_add_width(parser, &specifiers,
                        WIDTH_SPECIFIER_SHORT, location);
                break;

            case TOKEN_LONG:
                if (specifiers.type_spec_width == WIDTH_SPECIFIER_LONG)
                {
                    declaration_specifiers_add_width(parser, &specifiers,
                            WIDTH_SPECIFIER_LONG_LONG, location);
                }
                else
                {
                    declaration_specifiers_add_width(parser, &specifiers,
                            WIDTH_SPECIFIER_LONG, location);
                }
                break;

            // Sign specifiers
            case TOKEN_SIGNED:
                declaration_specifiers_add_sign(parser, &specifiers,
                        SIGN_SPECIFIER_SIGNED, location);
                break;

            case TOKEN_UNSIGNED:
                declaration_specifiers_add_sign(parser, &specifiers,
                        SIGN_SPECIFIER_UNSIGNED, location);
                break;

            // Complex specifiers here
            case TOKEN__COMPLEX:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        COMPLEX_SPECIFIER_COMPLEX, location);
                break;

            case TOKEN__IMAGINARY:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        COMPLEX_SPECIFIER_IMAGINAIRY, location);
                break;

            // normal specifiers are below
            case TOKEN_VOID:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_VOID, NULL, location);
                break;

            case TOKEN_CHAR:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_CHAR, NULL, location);
                break;

            case TOKEN_INT:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_INT, NULL, location);
                break;

            case TOKEN_FLOAT:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_FLOAT, NULL, location);
                break;
            
            case TOKEN_DOUBLE:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_DOUBLE, NULL, location);
                break;

            case TOKEN__BOOL:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_BOOL, NULL, location);
                break;

            // All of our tag types below. Note, we continue for them since, if
            // we were to break we would consume the current token, but we have 
            // already dealt with that :)
            case TOKEN_STRUCT:
            case TOKEN_UNION:
                parse_struct_or_union_specifier(parser, &specifiers);
                continue;
                    
            case TOKEN_ENUM:
                parse_enum_specificier(parser, &specifiers);
                continue;
            
            // Special case of identifier since we could have a typedef.
            case TOKEN_IDENTIFIER:
                // If were not allowed a typename were are likely done
                if (!declaration_specifiers_allow_typename(&specifiers))
                {
                    goto finish;
                }

                // If we sucessfully get a typename specifier go and eat the 
                // token. Otherwise, fall through as we are done with our 
                // specifiers
                if (try_parse_typename_specifier(parser, &specifiers))
                {
                    break;
                }

            /* FALLTHROUGH */

            finish:
            default:
                // Make sure our declaration specifiers are definitely valid. 
                // This elimates things like 'signed float' and other weird 
                // mistakes like that.
                declaration_specifiers_finish(&parser->sc, &specifiers);
                return specifiers;
        }

        // Finally, consume the token whose location we just used.
        consume(parser);
    }
}

static Declaration* parse_declaration(Parser* parser, DeclaratorContext context,
        bool eat_semi_no_decl)
{
    // First we need to get our declaration specifiers here
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
            false);

    // Check for a possibly empty declaration with a token and pass it to
    // the semantic checker to deal with
    if (is_match(parser, TOKEN_SEMI))
    {
        // Process the declaration specifiers so we know if we need to eat the
        // semi-colon or not.
        Declaration* decl = semantic_checker_process_specifiers(&parser->sc,
                &specifiers);
        
        // If we don't have a declaration, eat the semi, since nowhere else will
        // know to eat it.
        if (eat_semi_no_decl && !decl)
        {
            consume(parser);
        }

        return decl;
    }

    return parse_init_declarator_list(parser, &specifiers, context);
}

static void parse_declaration_or_definition(Parser* parser)
{
    Declaration* declaration = parse_declaration(parser, DECL_CTX_FILE,
            true);
    
    // If we had a function declaration with a body then we do not want to try
    // to parse a ';' afterwards and can simply return. Otherwise try to...
    // Also note we will also not try to parse a semi if the declaration is NULL
    // this still works if for example we get 'int' since it will try to parse
    // an identifier after. Note, that a NULL declaration means some kind of 
    // pretty bad error occured so we can just return here as parse_declaration
    // already handled if there was a semi or not.
    if ((declaration_is(declaration, DECLARATION_FUNCTION)
            && declaration_function_has_body(declaration))
            || declaration == NULL)
    {
        return;
    }

    parse_trailing_semi(parser, "top level declarator");
}

// The definitions of the functions we will use for pasing
static void parse_top_level(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOKEN_EOF:
            return;

        case TOKEN_SEMI:
        {
            Location semi = consume(parser);
            while (try_match(parser, TOKEN_SEMI, NULL))
                ;

            diagnostic_error_at(parser->dm, semi,
                    "extra ';' outside of a function");
            return;
        }

        case TOKEN_RCURLY:
        {
            Location curly = consume(parser);
            diagnostic_error_at(parser->dm, curly,
                    "extraneous closing brace ('}')");
            return;
        }

        // All the rest of the tokens, since we want to be able to parse any
        // top level declaration. Any invalid declarations will be caught and
        // errored about. 
        default:
            parse_declaration_or_definition(parser);
            return;
    }
}

static void parse_translation_unit_internal(Parser* parser)
{
    // Create our file scope which will be used throughout parsing the t-unit
    Scope externals = scope_extern(&parser->ast.ast_allocator);
    semantic_checker_push_externals(&parser->sc, &externals);

    Scope file = scope_file(&parser->ast.ast_allocator);
    semantic_checker_push_scope(&parser->sc, &file);

    // Check for case of empty translation unit which is not allowed.
    if (is_match(parser, TOKEN_EOF))
    {
        diagnostic_warning_at(parser->dm, current_token_location(parser),
                "ISO C requires a translation unit to contain at least one "
                "declaration");
    }

    while (!is_match(parser, TOKEN_EOF))
    {
        parse_top_level(parser);
    }

    // Finally, after EOF, we can check all of our external definitions.
    semantic_checker_check_externals(&parser->sc);

    // Here we can pop and delete since all of our needed decl's are in the top
    // level delcaration vector.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&file);

    semantic_checker_pop_externals(&parser->sc);
    scope_delete(&externals);
}

void parse_translation_unit(DiagnosticManager* dm, Preprocessor* pp)
{
    Parser parser = {0};
    parser.dm = dm;
    parser.pp = pp;
    preprocessor_advance_token(parser.pp, &parser.token);
    parser.ast = ast_create();
    parser.sc = sematic_checker_create(dm, &pp->identifiers, &parser.ast);
    parser.paren_count = 0;
    parser.bracket_count = 0;
    parser.brace_count = 0;

    parse_translation_unit_internal(&parser);

    ast_delete(&parser.ast);
}
