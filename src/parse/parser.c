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
static const TokenType storage_class[] = {TOKEN_TYPEDEF, TOKEN_EXTERN, 
        TOKEN_STATIC,TOKEN_AUTO,TOKEN_REGISTER};

static const TokenType type_specifier[] = { TOKEN_VOID, TOKEN_CHAR, TOKEN_SHORT,
        TOKEN_INT, TOKEN_LONG, TOKEN_FLOAT, TOKEN_DOUBLE, TOKEN_SIGNED,
        TOKEN_UNSIGNED, TOKEN__BOOL, TOKEN__COMPLEX, TOKEN__IMAGINARY,
        TOKEN_STRUCT, TOKEN_UNION, TOKEN_ENUM};

static const TokenType type_qualifier[] = {TOKEN_CONST, TOKEN_RESTRICT,
        TOKEN_VOLATILE};

static const TokenType function_specificer[] = {TOKEN_INLINE};

static const size_t storage_class_count = countof(storage_class);
static const size_t type_specifier_count = countof(type_specifier);
static const size_t type_qualifier_count = countof(type_qualifier);
static const size_t function_specificer_count = countof(function_specificer);


// Parser methods for getting the current and next token. We aim to be a LL(1)
// like parser so we should only ever need these in order to figure own the
// current production.
static Token* current_token(Parser* parser);

// TODO: reimplement the method below
static Token* next_token(Parser* parser);

static TokenType current_token_type(Parser* parser);
static TokenType next_token_type(Parser* parser);

static Location current_token_start_location(Parser* parser);
static Location current_token_end_location(Parser* parser);

// Methods for mathing a token type or unconditionally consuming a token.
static Location match(Parser* parser, TokenType type);
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

static Location current_token_start_location(Parser* parser)
{    
    return parser->token.loc;
}

static Location current_token_end_location(Parser* parser)
{
    return parser->token.end;
}

// Methods for mathing a token type or unconditionally consuming a token.
static Location match(Parser* parser, TokenType type)
{
    if (current_token_type(parser) == type)
    {
        Location location = current_token_start_location(parser);

        consume(parser);

        return location;
    }

    // diagnostic_error_at(parser->dm, parser->token.loc,
    //         "expected '%s' but got '%s'", token_type_get_name(type), 
    //         token_type_get_name(current_token_type(parser)));

    return LOCATION_INVALID;
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

    Location location = current_token_start_location(parser);

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
        *location = current_token_start_location(parser);
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

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_primary_expression(Parser* parser);
static Expression* parse_postfix_expression(Parser* parser,
        Expression* compound_literal);
static Expression* parse_argument_expression_list(Parser* parser);
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
static Statement* parse_selection_statement(Parser* parser);
static Statement* parse_iteration_statement(Parser* parser);
static Statement* parse_jump_statement(Parser* parser);
static Statement* parse_declaration_statement(Parser* parser);
static Statement* parse_empty_statement(Parser* parser);
static Statement* parse_error_statement(Parser* parser);
static Statement* parse_statement(Parser* parser, bool declaration_allowed);

// All of our functions for parsing declarations / definitions
// TODO: maybe all of these don't need to return a declaration type???
static Initializer* parse_designation(Parser* parser);
static Initializer* parse_designator_list(Parser* parser);
static Initializer* parse_designator(Parser* parser);
static Initializer* parse_initializer(Parser* parser);
static Initializer* parse_initializer_list(Parser* parser);

static Declarator parse_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext ctx);

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context);

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl);
static Declaration* parse_enum_specificer(Parser* parser);

static void parse_direct_declarator(Parser* parser, Declarator* declarator);

static void parse_identifier_list(Parser* parser, Declarator* declarator);
static Declaration* parse_paramater_declaration(Parser* parser);
static void parse_paramater_type_list(Parser* parser, Declarator* declarator);
static Declaration* parse_type_specificer(Parser* parser);

// Functions for us getting our declaration specifiers
static TypeQualifiers parse_type_qualifier(Parser* parser);

// All of our declaration specifier parsing functions
static bool has_declaration_specifier(Parser* parser, const Token* tok);
static void declaration_specifiers_add_storage(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeStorageSpecifier storage);
static void declaration_specifiers_add_qualifier(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeQualifiers qualifier);
static void declaration_specifiers_add_function(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeFunctionSpecifier function);
static void declaration_specifiers_add_width(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierWidth width);
static void declaration_specifiers_add_sign(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierSign sign);
static void declaration_specifiers_add_complex(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierComplex complex);
static void declaration_specifiers_add_type(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierType type);
static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser);

static TypeQualifiers parse_type_qualifier_list(Parser* parser);
static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser);

static Declaration* parse_struct_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers);
static void parse_struct_declaration(Parser* parser, Declaration* decl);
static void parse_struct_declaration_list(Parser* parser, Declaration* decl,
        bool is_struct);
static Declaration* parse_struct_or_union_specifier(Parser* parser);

static Declaration* parse_declaration(Parser* parser, DeclaratorContext ctx);
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

static bool is_expression_start(Parser* parser, const Token* tok)
{
    const TokenType type = tok->type;

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
            return true;

        case TOKEN_IDENTIFIER:
            return !is_typename_start(parser, tok);
        
        default:
            return false;
    }
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

    switch (current_token_type(parser))
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
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
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
            Location start_location = current_token_start_location(parser);

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

        default:
        {
            Location err_loc = current_token_start_location(parser);
            diagnostic_error_at(parser->dm, err_loc, "expected expression");

            // Do not do error recovery since the function calling this one 
            // could be pretty much anything.
            return semantic_checker_handle_error_expression(&parser->sc,
                    err_loc);
        }
    }
}

static Expression* parse_argument_expression_list(Parser* parser)
{
    do
    {
        Expression* arg = parse_assignment_expression(parser);
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    return NULL;
}

static Expression* parse_postfix_expression(Parser* parser,
        Expression* compound_literal)
{
    static const TokenType operators[] = {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    static const size_t num_operators = countof(operators);

    // How a handle compound literal is handled. Since the start of it looks
    // exactly like a cast expression we parse the cast first. Then since we see
    // a '{' we then parse the compound literal. After a compound literal we
    // should end up here. But since we that is hard we just pass in the 
    // compound literal expression and DON'T parse a primary expression. 
    Expression* expr = compound_literal != NULL 
            ? compound_literal
            : parse_primary_expression(parser);

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
                Expression* expr_list = NULL;
                if (!is_match(parser, TOKEN_RPAREN))
                {
                    expr_list = parse_argument_expression_list(parser);
                }

                Location rparen_loc;
                if (!try_match(parser, TOKEN_RPAREN, &rparen_loc))
                {
                    diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
                    rparen_loc = LOCATION_INVALID;
                    recover(parser, TOKEN_RPAREN, RECOVER_EAT_TOKEN);
                }
                
                expr = semantic_checker_handle_call_expression(&parser->sc,
                        expr, lparen_loc, expr_list, rparen_loc);
                break;
            }

            case TOKEN_DOT:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOKEN_IDENTIFIER))
                {
                    Location current = current_token_start_location(parser);
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
                    Location current = current_token_start_location(parser);
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
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_ADDRESS, op_loc, expr);
        }

        case TOKEN_STAR:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_DEREFERENCE, op_loc, expr);
        }

        case TOKEN_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_PLUS, op_loc, expr);
        }

        case TOKEN_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_MINUS, op_loc, expr);
        }

        case TOKEN_TILDE:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
        }

        case TOKEN_NOT:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return NULL;
            // return expression_create_unary(&parser->ast.ast_allocator,
            //         EXPRESSION_UNARY_NOT, op_loc, expr);
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

                return NULL;
            }
            else
            {
                Expression* expr = parse_unary_expression(parser);

                return NULL;
            }
        }

        default:
            return parse_postfix_expression(parser, NULL);
    }
}

static Expression* parse_compound_literal(Parser* parser, Location lparen_loc,
        QualifiedType type, Location rparen_loc)
{
    assert(is_match(parser, TOKEN_LCURLY));

    Location l_curly = consume(parser);

    Initializer* initializer = parse_initializer(parser);

    Location rcurly;
    if (!try_match(parser, TOKEN_RCURLY, &rcurly))
    {
        diagnostic_error_at(parser->dm, rparen_loc,
                "expected '}' after initializer");
    }
    
    // TODO: create the compound literal
    Expression* compound_literal = NULL;
    return parse_postfix_expression(parser, compound_literal);
}

static Expression* parse_cast_expression(Parser* parser)
{
    // If we cant possible have a cast expression just parse a unary expression
    if (!is_match(parser, TOKEN_LPAREN) ||
            !is_typename_start(parser, next_token(parser)))
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

    // Here we do a little trickery to get this parsing properly, see parse
    // postfix expression for details
    if (is_match(parser, TOKEN_LCURLY))
    {
        return parse_compound_literal(parser, lparen_loc, type, rparen_loc);
    }

    Expression* expr = parse_cast_expression(parser);

    // TODO: create the cast expression

    return expr;
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator, type, 
        //         op_loc, expr, rhs);
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

        // expr = expression_create_binary(&parser->ast.ast_allocator, type, 
        //         op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator, type,
        //         op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator, type,
        //         op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator, type, 
        //         op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator,
        //         EXPRESSION_BINARY_AND, op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator,
        //         EXPRESSION_BINARY_XOR, op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator,
        //         EXPRESSION_BINARY_OR, op_loc, expr, rhs);
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

        // expr = expression_create_binary(&parser->ast.ast_allocator, 
        //         EXPRESSION_BINARY_LOGICAL_AND, op_loc, expr, rhs);
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

        // return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator,
        //         EXPRESSION_BINARY_LOGICAL_OR, op_loc, expr, rhs);
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

        expr = NULL;
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

        // Now make our new binary expression...
        return NULL;
        // expr = expression_create_binary(&parser->ast.ast_allocator,
        //         type, op_location, expr, rhs);
    }

    return expr;
}

static Expression* parse_constant_expression(Parser* parser)
{
    Expression* expr = parse_conditional_expression(parser);

    // TODO: handle folding the constant expression...
    return expr;
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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "label at end of compound statement");
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    if (!is_statement_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
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
    Location colon_loc = parse_expected_colon(parser, "case");
    
    // TODO: i think I want to turn this into semantic_checker_add_case
    // TODO: and then if that errors then we error, otherwise parse the stmt

    if (!semantic_checker_check_case_allowed(&parser->sc, case_loc))
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

    StatementVector stmts = statement_vector_create(32);
    while (!is_match(parser, TOKEN_RCURLY) && !is_match(parser, TOKEN_EOF))
    {
        Statement* stmt = parse_statement(parser, true);
        statement_vector_push(&stmts, stmt);
    }

    Location r_curly = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_RCURLY, &r_curly))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected '}'");
    }

    return statement_create_compound(&parser->ast.ast_allocator, l_curly,
            r_curly, &stmts);
}

static Statement* parse_compound_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_LCURLY));

    // Set-up the new scope for declarations
    Scope scope = scope_new_block();
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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' after %s", context);
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
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
        Location err_loc = current_token_start_location(parser);
        diagnostic_error_at(parser->dm, err_loc, "expected statement");
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // NOTE: we may get here due to trying to parse a declaration statement as
    // the body of a for, if, while, ... So make sure we are actually an
    // expression. If we aren't, error and recover
    if (!is_expression_start(parser, current_token(parser)))
    {
        Location err_loc = current_token_start_location(parser);
        diagnostic_error_at(parser->dm, err_loc, "expected expression");
        recover_two(parser, TOKEN_RCURLY, TOKEN_SEMI, RECOVER_NONE);

        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(is_expression_start(parser, current_token(parser)));

    Expression* expr = parse_expression(parser);
    Location semi_loc = parse_trailing_semi(parser, "expression");
    
    return semantic_checker_handle_expression_statement(&parser->sc,
            expr, semi_loc);
}

static bool parse_expression_for_statement(Parser* parser, Location* lparen_loc,
        Expression** cond, Location* rparen_loc, const char* context)
{
    if (!is_match(parser, TOKEN_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected '(' after '%s'", context);
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
                
        return false;
    }

    *lparen_loc = consume(parser);

    *cond = parse_expression(parser);

    if (!is_match(parser, TOKEN_RPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ')'");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return false;
    }

    *rparen_loc = consume(parser);

    return true;
}

static Statement* parse_if_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_IF));

    Location if_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, &lparen_loc, &cond, &rparen_loc,
            "if"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* if_body = parse_statement(parser, false);

    Location else_loc = LOCATION_INVALID;
    Statement* else_body = NULL;
    if (is_match(parser, TOKEN_ELSE))
    {
        else_loc = consume(parser);
        else_body = parse_statement(parser, false);
    }

    return semantic_checker_handle_if_statement(&parser->sc, if_loc,
            lparen_loc, cond, rparen_loc, if_body, else_loc, else_body);
}

static Statement* parse_switch_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_SWITCH));

    Location switch_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* expr = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, &lparen_loc, &expr, &rparen_loc,
            "switch"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // TODO: need to check duplicate switch statements. So instead here, what
    // TODO: we should do is create the switch statement. Then when we parse a 
    // TODO: case statement or a default statement, what we need to do is in
    // TODO: the semantic checker we need to automatically try to add it into
    // TODO: some kind of structure and if it errors then we create an error 
    // TODO: statement, otherwise, if it succeeds, then allow it to create the
    // TODO: statemnet

    // Create the statement here to avoid possible memory leak
    Scope switch_scope = scope_new_switch();
    semantic_checker_push_scope(&parser->sc, &switch_scope);

    Statement* body = parse_statement(parser, false);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&switch_scope);

    return semantic_checker_handle_switch_statement(&parser->sc, switch_loc,
            lparen_loc, expr, rparen_loc, body);
}

static Statement* parse_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_WHILE));

    Location while_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, &lparen_loc, &cond, &rparen_loc,
            "while"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Create and push the scope here to avoid potential memory leak!
    Scope while_scope = scope_new_while();
    semantic_checker_push_scope(&parser->sc, &while_scope);

    // Parse the body and set the inner while stmt
    Statement* body = parse_statement(parser, false);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&while_scope);

    return semantic_checker_handle_while_statement(&parser->sc, while_loc,
            lparen_loc, cond, rparen_loc, body);
}

static Statement* parse_do_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_DO));

    Scope do_while_scope = scope_new_do_while();
    semantic_checker_push_scope(&parser->sc, &do_while_scope);

    Location do_loc = consume(parser);
    Statement* body = parse_statement(parser, false);

    // Pop statement here to avoid poential memory leak
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&do_while_scope);
    
    // If the token is not a while just skip till we get a semi...
    if (!is_match(parser, TOKEN_WHILE))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected 'while' in do/while loop");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(is_match(parser, TOKEN_WHILE));

    // make sure we capture the while part.
    Location while_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen_loc = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, &lparen_loc, &cond, &rparen_loc,
            "do/while"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Location semi_loc = parse_trailing_semi(parser, "do/while statement");

    return semantic_checker_handle_do_while_statement(&parser->sc, do_loc,
            body, while_loc, lparen_loc, cond, rparen_loc, semi_loc);
}

static Statement* parse_for_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_FOR));

    Location for_loc = consume(parser);

    // Make sure we got a lparen after!
    if (!is_match(parser, TOKEN_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected '(' after 'for'");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Scope for_scope = scope_new_for();
    semantic_checker_push_scope(&parser->sc, &for_scope);

    assert(is_match(parser, TOKEN_LPAREN));
    Location lparen_loc = consume(parser);

    Statement* init = NULL;

    Declaration* init_declaration = NULL;
    Expression* init_expression = NULL;
    if (is_typename_start(parser, current_token(parser)))
    {   
        init_declaration = parse_declaration(parser, DECLARATION_CONTEXT_BLOCK);
    }
    else if (is_expression_start(parser, current_token(parser)))
    {
        init_expression = parse_expression(parser);
    }
    else if (!is_match(parser, TOKEN_SEMI))
    {
        // TODO: ensure this is adequete error recovery
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "expected expression");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        init = semantic_checker_handle_error_statement(&parser->sc);
    }

    if (!try_match(parser, TOKEN_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* cond = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        cond = parse_expression(parser);
    }

    if (!try_match(parser, TOKEN_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ')'");
    }
    else
    {
        rparen_loc = consume(parser);
    }

    Statement* body = parse_statement(parser, false);

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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
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
    Declaration* decl = parse_declaration(parser, DECLARATION_CONTEXT_BLOCK);
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
        {
            if (is_typename_start(parser, current_token(parser)))
            {
                // Note: declarations are not considered statements all of the
                // time in C. e.g. they are not allowed after labels.
                if (declaration_allowed)
                {
                    return parse_declaration_statement(parser);
                }
            }

            return parse_expression_statement(parser);
        }
    }
}

// Below is things for declarations

static Initializer* parse_designation(Parser* parser)
{
    parse_designator_list(parser);

    match(parser, TOKEN_EQUAL);
 
    return NULL;
}

static Initializer* parse_designator_list(Parser* parser)
{
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_DOT}, 2))
    {
        parse_designator(parser);
    }
 
    return NULL;
}

static Initializer* parse_designator(Parser* parser)
{
    if (is_match(parser, TOKEN_LBRACKET))
    {
        consume(parser);

        parse_constant_expression(parser);

        match(parser, TOKEN_RBRACKET);
    }
    else if (is_match(parser, TOKEN_DOT))
    {
        consume(parser);
        match(parser, TOKEN_IDENTIFIER);
    }
    else
    {
        panic("parse_designator");
    }
 
    return NULL;
}

static Initializer* parse_initializer(Parser* parser)
{
    if (is_match(parser, TOKEN_LCURLY))
    {
        consume(parser);

        parse_initializer_list(parser);

        if (is_match(parser, TOKEN_COMMA))
        {
            consume(parser);
        }
        match(parser, TOKEN_RCURLY);
    }
    else
    {
        parse_assignment_expression(parser);
    }


    return NULL;
}

static Initializer* parse_initializer_list(Parser* parser)
{
    while (!is_match(parser, TOKEN_RCURLY))
    {
        // If we can match a designation do that...
        if (has_match(parser, (TokenType[]) {TOKEN_DOT, TOKEN_LBRACKET}, 2))
        {
            parse_designation(parser);
        }

        // Then get the initializer
        parse_initializer(parser);

        // End of initializer list e.g. , }
        if (is_match(parser, TOKEN_COMMA) && is_next_match(parser, TOKEN_RCURLY))
        {
            consume(parser); // Consume the comma
            break;
        }
        else if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }
        else // Get the comma and keep going
        {
            consume(parser);
            assert(!is_match(parser, TOKEN_RCURLY));
        }
    }

    return NULL;
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

static void parse_initializer_after_declarator(Parser* parser,
        Declaration* declaration)
{
    // Then parse the initializer if there is one
    if (is_match(parser, TOKEN_EQUAL))
    {
        Location equal_loc = consume(parser);
        Initializer* initializer = parse_initializer(parser);

        semantic_checker_declaration_add_initializer(&parser->sc, declaration, 
                equal_loc, initializer);
    }
}

static void parse_knr_function_parameters(Parser* parser, Declaration* decl)
{

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

    // Now process the declaration.
    Declaration* function = semantic_checker_process_declarator(&parser->sc,
            declarator);

    // TODO: check for knr functions
    // if (declaration_function_has_knr(decl))
    parse_knr_function_parameters(parser, function);

    // This here is only possible to get to if we had a knr function definition
    // that did not have a lcurly after it.
    if (!is_match(parser, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected function body after function declarator");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    // Do some checks when we 
    semantic_checker_handle_function_start(&parser->sc, function);

    // Create and push our function scope
    FunctionScope func_scope = function_scope_create(function);
    sematic_checker_push_function_scope(&parser->sc, &func_scope);

    Scope function_body = scope_new_function_declaration();
    semantic_checker_push_scope(&parser->sc, &function_body);

    // Add all of our important function parameters into this scope. Making sure
    // to use this declaration that we are currently parsing to avoid weird
    // errors.
    semantic_checker_add_function_parameters(&parser->sc, function);

    Statement* stmt = parse_compound_statement_internal(parser);

    // Finish the function by checking all of our labels...
    sematic_checker_act_on_end_of_function(&parser->sc);

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
    if (/*is_knr_function()*/0)
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

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context)
{
    // First parse the declarator but do not create a declaration as we need to
    // delay this to potentially prevent some helpful but maybe irrelavent
    // diagnostics.
    Declarator declarator = parse_declarator(parser, specifiers, context);

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
    if (declarator_has_function(&declarator) && !has_declaration(parser))
    {
        // Now check we have a valid context for being able to parse a function
        // definition. Knowing that it should be allow and that we might want
        // to have one.
        if (context == DECLARATION_CONTEXT_FILE)
        {
            if (is_function_definition(parser, &declarator))
            {
                return parse_function_definition(parser, &declarator);
            }

            // Here we are expecting a function definition base on the next
            // token. if it is not an '=' ',' or ';' then then we must have
            // a definition.
            Location location = current_token_start_location(parser);
            diagnostic_error_at(parser->dm, location,
                    "expected function body after function declarator");
            recover(parser, TOKEN_SEMI, RECOVER_NONE);
            return NULL;
        }
        else
        {
            // Only error if we get the start of a function definition. This is
            // where we diverge from clang and meet up with gcc in that we will
            // consider knr parameter lists here...
            if (is_function_definition(parser, &declarator))
            {
                // Otherwise we are not allowed to have a function at all, do 
                // not properly handle the declarator and instead error, recover
                // and return NULL
                Location location = current_token_start_location(parser);
                diagnostic_error_at(parser->dm, location,
                        "function definition is not allowed here");
                recover(parser, TOKEN_SEMI, RECOVER_NONE);
                return NULL;
            }
        }
    }
    
    // Here we know that we shouldn't create a function definition and so we
    // can finally process the declarator and potentially try to parse an
    // initializer after the definition.
    Declaration* decl = semantic_checker_process_declarator(&parser->sc,
            &declarator);

    parse_initializer_after_declarator(parser, decl);
    semantic_checker_declaration_finish(&parser->sc, decl);

    // Otherwise keep trying to parse declarations with an initializer until
    // we appear to be at the end of all of our declarations.
    while (try_match(parser, TOKEN_COMMA, NULL))
    {
        declarator = parse_declarator(parser, specifiers, context);
        decl = semantic_checker_process_declarator(&parser->sc, &declarator);

        parse_initializer_after_declarator(parser, decl);
        semantic_checker_declaration_finish(&parser->sc, decl);
    }
    
    return decl;
}

static void parse_identifier_list(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LPAREN));
    Location lparen_loc = consume(parser);

    // See if we got an empty parameter list. This is still an old style
    // declaration, however, we are just trying to skip doing a whole bunch of
    // work that we don't have to do
    if (is_match(parser, TOKEN_RPAREN))
    {
        Location rparen_loc = consume(parser);
        return;
    }

    // Create our set to keep track of the seen identifiers
    PtrSet identifier_set = pointer_set_create();
    IdentifierVector vec = identifier_vector_create(1);

    do
    {
        if (!is_match(parser, TOKEN_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected identifier");
            recover(parser, TOKEN_RPAREN, RECOVER_NONE);
            break;
        }

        Token identifier_token = *current_token(parser);
        Identifier* identifier = identifier_token.data.identifier;
        Location identifier_loc = consume(parser);

        // If we get a typename, diagnose and continue, for this we will just
        // insert it into the identifiers anyways.
        if (is_typename_start(parser, &identifier_token))
        {
            diagnostic_error_at(parser->dm, identifier_loc,
                    "unexpected type name '%s': expected identifier",
                    identifier->string.ptr);
        }
        
        if (pointer_set_contains(&identifier_set, identifier))
        {
            diagnostic_error_at(parser->dm, identifier_loc,
                    "redefinition of parameter '%s'",
                    identifier->string.ptr);
        }
        else
        {
            pointer_set_insert(&identifier_set, identifier);
            identifier_vector_push(&vec, identifier);
        }
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    // Match the end of the parameter list
    if (!is_match(parser, TOKEN_RPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ')'");
        recover(parser, TOKEN_RPAREN, RECOVER_STOP_AT_SEMI);
    }
    Location rparen_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_RPAREN))
    {
        rparen_loc = consume(parser);
    }

    // TODO: will need to use this identifier vector to create an array of
    // declarators that we can later fill when we get the function definition.
    pointer_set_delete(&identifier_set);
    identifier_vector_free(&vec, NULL);
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    // Parse declaration specifiers
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);

    // Parse the function paramater declarator
    Declarator declarator = parse_declarator(parser, &specifiers,
            DECLARATION_CONTEXT_FUNCTION_PARAM);

    // Finish processing the declaration
    Declaration* declaration = semantic_checker_process_function_param(
            &parser->sc, &declarator);

    return declaration;
}

static void parse_paramater_type_list(Parser* parser, Declarator* declarator)
{
    Scope function_proto = scope_new_function_prototype();
    semantic_checker_push_scope(&parser->sc, &function_proto);

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
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
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
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ')'");
        recover(parser, TOKEN_RPAREN, RECOVER_STOP_AT_SEMI);
    }
    Location rparen_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_RPAREN))
    {
        rparen_loc = consume(parser);
    }

    // Push the function type onto the end of the declarator
    Declaration* all_decls = scope_get_declarations(&function_proto);
    declarator_push_function(declarator, lparen_loc, rparen_loc, parms, 
            num_parms, all_decls, dots);


    // Pop the finished function scope.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&function_proto);
}

static void parse_function_declarator(Parser* parser, Declarator* declarator)
{
    if (is_typename_start(parser, next_token(parser)))
    {
        parse_paramater_type_list(parser, declarator);
    }
    else
    {
        parse_identifier_list(parser, declarator);
    }

    // TODO: create our function declarator here.
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
    if (!is_match(parser, TOKEN_RBRACKET))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ']'");
        recover(parser, TOKEN_RBRACKET, RECOVER_STOP_AT_SEMI);
        return;
    }
    else
    {
        rbracket_loc = consume(parser);
    }

    declarator_push_array(declarator, lbracket_loc, rbracket_loc, static_loc,
            qualifiers, expression, is_static, is_star);
}

static void parse_direct_declarator(Parser* parser, Declarator* declarator)
{
    bool identifier_allowed = declarator_identifier_allowed(declarator);
    bool identifier_needed = declarator_identifier_required(declarator);

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        // Get and add the identifier if it is allowed to be in this declarator
        Identifier* identifier = current_token(parser)->data.identifier;
        Location loc = consume(parser);

        if (identifier_allowed)
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
        if (!identifier_needed &&
                (is_typename_start(parser, next_token(parser)) ||
                is_next_match(parser, TOKEN_RPAREN)))
        {
            goto parse_tail;
        }

        Location lparen_loc = consume(parser);

        parse_declarator_internal(parser, declarator);
        
        if (!is_match(parser, TOKEN_RPAREN))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
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
    else if (!identifier_needed)
    {
        // 'Set' the declarators location so that we can provide warning about
        // functions not have parameter names if we need.
        Location location = current_token_start_location(parser);
        declarator_set_identifier(declarator, NULL, location);
    }
    else
    {
        // Give an appriopriate diagnostic based on the current context
        Location current = current_token_start_location(parser);
        if (declarator_get_context(declarator) == DECLARATION_CONTEXT_STRUCT)
        {
            diagnostic_error_at(parser->dm, current,
                    "expected member name or ';' after declaration specifiers");
        }
        else
        {   
            diagnostic_error_at(parser->dm, current,
                    "expected identifier or '('");
        }
        declarator_set_invalid(declarator);
        return;
    }

    // Now finish parsing the declarator.
parse_tail:
    while (is_match_two(parser, TOKEN_LPAREN, TOKEN_LBRACKET))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            parse_function_declarator(parser, declarator);
        }
        else if (is_match(parser, TOKEN_LBRACKET))
        {
            parse_array_declarator(parser, declarator);
        }
    }
}

static TypeQualifiers parse_type_qualifier_list(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    // Set up a declaration spec with no type qualifiers initially
    DeclarationSpecifiers qualifiers = { .qualifiers = TYPE_QUALIFIER_NONE };
    do
    {
        TypeQualifiers qualifier;
        switch (current_token_type(parser))
        {
            case TOKEN_CONST: qualifier = TYPE_QUALIFIER_CONST; break;
            case TOKEN_RESTRICT: qualifier = TYPE_QUALIFIER_RESTRICT; break;
            case TOKEN_VOLATILE: qualifier = TYPE_QUALIFIER_VOLATILE; break;
        }
        
        // Note: the below will consume the token itself
        declaration_specifiers_add_qualifier(parser, &qualifiers, qualifier);
    }
    while (has_match(parser, type_qualifier, type_qualifier_count));

    return qualifiers.qualifiers;
}

static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser)
{
    if (!has_match(parser, type_qualifier, type_qualifier_count))
    {
        return TYPE_QUALIFIER_NONE;
    }

    return parse_type_qualifier_list(parser);
}

// Simply parse declaration specifiers and remove typenames and function
// specifiers. Simpler than diagnosing in declaration specifiers but doing that
// there would be a bit nicer and more informative I think
static DeclarationSpecifiers parse_specifier_qualifier_list(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);

    // Make sure that we have no storage specifier for each of the members.
    if (specifiers.storage_spec != TYPE_STORAGE_SPECIFIER_NONE)
    {
        diagnostic_error_at(parser->dm, specifiers.location, 
                "type name does not allow storage class to be specified");
        specifiers.storage_spec = TYPE_STORAGE_SPECIFIER_NONE;
    }

    // Also make sure we have no function specififers
    if (specifiers.function_spec != TYPE_FUNCTION_SPECIFIER_NONE)
    {
        diagnostic_error_at(parser->dm, specifiers.location, 
                "type name does not allow function specifier to be specified");
        specifiers.function_spec = TYPE_FUNCTION_SPECIFIER_NONE;
    }

    return specifiers;
}

static void parse_struct_declaration(Parser* parser, Declaration* decl)
{
    assert(declaration_is(decl, DECLARATION_STRUCT) ||
            declaration_is(decl, DECLARATION_UNION));

    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);

    // Struct declarations will always need to have a name to be useful. Unless
    // we implement c11's anonymous struct/union injection into the scope.
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
        Declarator declarator = parse_declarator(parser, &specifiers,
                DECLARATION_CONTEXT_STRUCT);
        Declaration* member = semantic_checker_process_struct_declarator(
                &parser->sc, decl, &declarator);

        if (member != NULL)
        {
            declaration_struct_add_member(decl, member);
        }
    }
    while (try_match(parser, TOKEN_COMMA, NULL));

    if (!is_match(parser, TOKEN_SEMI))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' after struct declaration");
        
        // We could be at end of struct definition!
        if (!is_match(parser, TOKEN_RCURLY))
        {
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        }

        return;
    }

    assert(is_match(parser, TOKEN_SEMI));
    consume(parser);
}

static void parse_struct_declaration_list(Parser* parser, Declaration* decl,
        bool is_struct)
{
    assert(is_match(parser, TOKEN_LCURLY));
    
    Location l_curly = consume(parser);

    if (is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "empty %s is not supported", is_struct ? "struct" : "union");
        consume(parser);
        return;
    }

    Scope member_scope = scope_new_member();
    semantic_checker_push_scope(&parser->sc, &member_scope);

    while (!is_match_two(parser, TOKEN_RCURLY, TOKEN_EOF))
    {
        // Consume any extra ';' that appear inside the struct and warn about
        // them.
        if (is_match(parser, TOKEN_SEMI))
        {
            Location semi = consume(parser);
            while (try_match(parser, TOKEN_SEMI, NULL))
                ;
            diagnostic_warning_at(parser->dm, semi,
                    "extra ';' inside a struct");
            continue;
        }

        if (!is_typename_start(parser, current_token(parser)))
        {
            // TODO: not exactly sure what error recovery clang is doing for
            // TODO: this. But we will just use this easy strategy for now
            Location loc = current_token_start_location(parser);
            diagnostic_error_at(parser->dm, loc,
                    "type name requires a specifier or qualifier");
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
            continue;
        }

        // Otherwise we can parse a struct declaration.
        parse_struct_declaration(parser, decl);
    }

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&member_scope);

    Location closing_curly = LOCATION_INVALID;
    if (!try_match(parser, TOKEN_RCURLY, &closing_curly))
    {
        Location current = current_token_start_location(parser);
        diagnostic_error_at(parser->dm, current, "expected '}'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
    }

    semantic_checker_finish_struct_declaration(&parser->sc, decl);
}

static Declaration* parse_struct_or_union_specifier(Parser* parser)
{
    assert(is_match_two(parser, TOKEN_STRUCT, TOKEN_UNION));

    bool is_struct = is_match(parser, TOKEN_STRUCT);
    DeclarationType type = is_struct ? DECLARATION_STRUCT : DECLARATION_UNION;

    Location tag_loc = consume(parser);

    if (!is_match_two(parser, TOKEN_IDENTIFIER, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, tag_loc,
                "declaration of anonymous %s must be a definition",
                is_struct ? "struct" : "union");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        return declaration_create_error(&parser->ast.ast_allocator, tag_loc);
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
    Declaration* declaration = semantic_checker_handle_tag(&parser->sc,
            type, type, identifier, identifier_loc, is_definition);
    assert(declaration != NULL);

    // Return the definition if we don't have a body to parse
    if (!is_match(parser, TOKEN_LCURLY))
    {
        return declaration;
    }

    parse_struct_declaration_list(parser, declaration, is_struct);
    
    return declaration;
}

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl)
{
    assert(enum_decl && declaration_is(enum_decl, DECLARATION_ENUM));
    assert(is_match(parser, TOKEN_LCURLY));

    // Here we can actually parse the enum
    Location opening_curly = consume(parser);

    // Check for an empty enum, setting it to be a complete declaration if so
    if (is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "use of empty enum");
        consume(parser);
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
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected identifier");
            recover_three(parser, TOKEN_COMMA, TOKEN_IDENTIFIER, TOKEN_RCURLY,
                    RECOVER_STOP_AT_SEMI);

            if (is_match(parser, TOKEN_COMMA))
            {
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
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "missing ',' between enumerators");
                continue;
            }
            
            if (is_match(parser, TOKEN_SEMI))
            {
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected '= constant-expression' or end of "
                        "enumeration");
                break;
            }
            
            // Otherwise, recover by finding the next enumerator or the end
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
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

    Location closing_curly = LOCATION_INVALID;
    if (!is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected '}'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
    }
    else
    {
        closing_curly = consume(parser);
    }
}

static Declaration* parse_enum_specificier(Parser* parser)
{
    assert(is_match(parser, TOKEN_ENUM));

    Location enum_location = consume(parser);

    // We need to have a match for one of these here otherwise we have a problem
    if (!is_match_two(parser, TOKEN_IDENTIFIER, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "expected identifier or '{' after 'enum'");
        recover(parser, TOKEN_SEMI, RECOVER_NONE);
        return declaration_create_error(&parser->ast.ast_allocator,
                enum_location);
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
    Declaration* declaration = semantic_checker_handle_tag(&parser->sc,
            DECLARATION_ENUM, enum_location, identifier, identifier_loc,
            is_definition);
    assert(declaration != NULL);

    // Return the definition if we don't have a body to parse
    if (!is_match(parser, TOKEN_LCURLY))
    {
        return declaration;
    }

    parse_enumerator_list(parser, declaration);

    return declaration;
}

static QualifiedType parse_type_name(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);
    Declarator declarator = parse_declarator(parser, &specifiers,
            DECLARATION_CONTEXT_TYPE_NAME);
    QualifiedType type = semantic_checker_process_typename(&parser->sc,
            &declarator);

    return type;
}

static void declaration_specifiers_add_storage(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeStorageSpecifier storage)
{
    Location location = consume(parser);

    // If we haven't recieved a storage specifier we can add it and be done.
    if (specifiers->storage_spec == TYPE_STORAGE_SPECIFIER_NONE)
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
        DeclarationSpecifiers* specifiers, TypeQualifiers qualifier)
{
    Location location = consume(parser);

    if (specifiers->qualifiers & qualifier)
    {
        diagnostic_warning_at(parser->dm, location,
                "duplicate '%s' type qualifier",
                type_qualifier_to_name(qualifier));
    }

    specifiers->qualifiers |= qualifier;
}

static void declaration_specifiers_add_function(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeFunctionSpecifier function)
{
    Location location = consume(parser);

    if (specifiers->function_spec & function)
    {
        diagnostic_warning_at(parser->dm, location,
                "duplicate '%s' function specifier",
                function_specifier_to_name(function));
    }

    specifiers->function_spec |= function;
}

static void declaration_specifiers_add_width(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierWidth width)
{
    Location location = consume(parser);

    // Here we differ from clang which allows duplicate short specifier
    if (specifiers->type_spec_width == TYPE_SPECIFIER_WIDTH_NONE)
    {
        specifiers->type_spec_width = width;
    }
    else if (specifiers->type_spec_width == TYPE_SPECIFIER_WIDTH_LONG &&
            width == TYPE_SPECIFIER_WIDTH_LONG_LONG)
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
        DeclarationSpecifiers* specifiers, TypeSpecifierSign sign)
{
    Location location = consume(parser);

    if (specifiers->type_spec_sign == TYPE_SPECIFIER_SIGN_NONE)
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
        DeclarationSpecifiers* specifiers, TypeSpecifierComplex complex)
{
    Location location = consume(parser);

    if (specifiers->type_spec_complex == TYPE_SPECIFIER_COMPLEX_NONE)
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

static void declaration_specifiers_add_type(Parser* parser,
        DeclarationSpecifiers* specifiers, TypeSpecifierType type)
{
    // Don't consume the token if it is struct union or enum type as the parsing
    // functions for these require that we don't eat this and leave it to be
    // consumed later.
    Location location;
    if (type == TYPE_SPECIFIER_TYPE_STRUCT || 
            type == TYPE_SPECIFIER_TYPE_UNION || 
            type == TYPE_SPECIFIER_TYPE_ENUM)
    {
        location = current_token_start_location(parser);
    }
    else
    {
        location = consume(parser);
    }

    if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_NONE)
    {
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
}

static void declaration_specifiers_add_declaration(
        DeclarationSpecifiers* specifiers, Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_STRUCT) ||
            declaration_is(declaration, DECLARATION_UNION) ||
            declaration_is(declaration, DECLARATION_ENUM) ||
            declaration_is(declaration, DECLARATION_TYPEDEF));

    // Add both the declaration and the type into the declaration specifiers.
    // TODO: this feels a bit weird, maybe more checks are needed?
    if (specifiers->declaration == NULL && specifiers->type == NULL)
    {
        specifiers->declaration = declaration;
        specifiers->type = declaration->base.qualified_type.type;
    }
}

static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser)
{
    DeclarationSpecifiers specifiers = declaration_specifiers_create(
            current_token_start_location(parser));

    do
    {
        switch (current_token_type(parser))
        {
            // Storage specifiers
            case TOKEN_TYPEDEF:
                declaration_specifiers_add_storage(parser, &specifiers,
                        TYPE_STORAGE_SPECIFIER_TYPEDEF);
                break;

            case TOKEN_EXTERN:
                declaration_specifiers_add_storage(parser, &specifiers,
                        TYPE_STORAGE_SPECIFIER_EXTERN);
                break;

            case TOKEN_STATIC:
               declaration_specifiers_add_storage(parser, &specifiers,
                        TYPE_STORAGE_SPECIFIER_STATIC);
                break;

            case TOKEN_AUTO:
                declaration_specifiers_add_storage(parser, &specifiers,
                        TYPE_STORAGE_SPECIFIER_AUTO);
                break;

            case TOKEN_REGISTER:
                declaration_specifiers_add_storage(parser, &specifiers,
                        TYPE_STORAGE_SPECIFIER_REGISTER);
                break;

            // Qualifiers
            case TOKEN_CONST:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        TYPE_QUALIFIER_CONST);
                break;

            case TOKEN_VOLATILE:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        TYPE_QUALIFIER_VOLATILE);
                break;

            case TOKEN_RESTRICT:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        TYPE_QUALIFIER_RESTRICT);
                break;
                
            // Function specifier
            case TOKEN_INLINE:
                declaration_specifiers_add_function(parser, &specifiers,
                        TYPE_FUNCTION_SPECIFIER_INLINE);
                break;

            // Width specifiers
            case TOKEN_SHORT:
                declaration_specifiers_add_width(parser, &specifiers,
                        TYPE_SPECIFIER_WIDTH_SHORT);
                break;

            case TOKEN_LONG:
                if (specifiers.type_spec_width == TYPE_SPECIFIER_WIDTH_LONG)
                {
                    declaration_specifiers_add_width(parser, &specifiers,
                        TYPE_SPECIFIER_WIDTH_LONG_LONG);
                }
                else
                {
                    declaration_specifiers_add_width(parser, &specifiers,
                        TYPE_SPECIFIER_WIDTH_LONG);
                }
                break;

            // Sign specifiers
            case TOKEN_SIGNED:
                declaration_specifiers_add_sign(parser, &specifiers,
                        TYPE_SPECIFIER_SIGN_SIGNED);
                break;

            case TOKEN_UNSIGNED:
                declaration_specifiers_add_sign(parser, &specifiers,
                        TYPE_SPECIFIER_SIGN_UNSIGNED);
                break;

            // Complex specifiers here
            case TOKEN__COMPLEX:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        TYPE_SPECIFIER_COMPLEX_COMPLEX);
                break;

            case TOKEN__IMAGINARY:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        TYPE_SPECIFIER_COMPLEX_IMAGINAIRY);
                break;

            // normal specifiers are below
            case TOKEN_VOID:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_VOID);
                break;

            case TOKEN_CHAR:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_CHAR);
                break;

            case TOKEN_INT:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_INT);
                break;

            case TOKEN_FLOAT:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_FLOAT);
                break;
            
            case TOKEN_DOUBLE:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_DOUBLE);
                break;

            case TOKEN__BOOL:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_BOOL);
                break;

            case TOKEN_STRUCT:
            {
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_STRUCT);
                Declaration* struct_decl = 
                        parse_struct_or_union_specifier(parser);
                declaration_specifiers_add_declaration(&specifiers,
                        struct_decl);
                break;
            }

            case TOKEN_UNION:
            {
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_UNION);
                Declaration* union_decl = 
                        parse_struct_or_union_specifier(parser);
                declaration_specifiers_add_declaration(&specifiers, union_decl);
                break;
            }
            
            case TOKEN_ENUM:
            {
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_ENUM);
                Declaration* enum_decl = 
                        parse_enum_specificier(parser);
                declaration_specifiers_add_declaration(&specifiers, enum_decl);
                break;
            }
            
            // Special case of identifier since we could have a typedef.
            case TOKEN_IDENTIFIER:
            {
                // If we have not had any information about the type yet then we
                // should check if we have a typedef.
                if (declaration_specifiers_allow_typename(&specifiers)
                        && is_typename_start(parser, current_token(parser)))
                {
                    Identifier* id = current_token(parser)->data.identifier;
                    Declaration* typename = semantic_checker_get_typename(
                            &parser->sc, id);
                    
                    declaration_specifiers_add_declaration(&specifiers,
                            typename);
                    declaration_specifiers_add_type(parser, &specifiers,
                            TYPE_SPECIFIER_TYPE_TYPENAME);
                    break;
                }
            }

            /* FALLTHROUGH */

            default:
                // Make sure our declaration specifiers are definitely valid. 
                // This elimates things like 'signed float' and other weird 
                // mistakes like that.
                declaration_specifiers_finish(&parser->sc, &specifiers);
                return specifiers;
        }
    }
    while (true);
}

static Declaration* parse_declaration(Parser* parser, DeclaratorContext context)
{
    // First we need to get our declaration specifiers here
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);

    // Check for a possibly empty declaration with a token and pass it to
    // the semantic checker to deal with
    if (is_match(parser, TOKEN_SEMI))
    {
        return semantic_checker_process_specifiers(&parser->sc, &specifiers);
    }

    return parse_init_declarator_list(parser, &specifiers, context);
}

static void parse_declaration_or_definition(Parser* parser)
{
    Declaration* declaration = parse_declaration(parser,
            DECLARATION_CONTEXT_FILE);
    
    // If we had a function declaration with a body then we do not want to try
    // to parse a ';' afterwards and can simply return. Otherwise try to...
    if (declaration_is(declaration, DECLARATION_FUNCTION)
            && declaration_function_has_body(declaration))
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

        // All the rest of the tokens exept identifier. Since we want to still
        // enable implicit int at the top level
        default:
        {
            // Generic error for naughty tokens
            if (!is_typename_start(parser, current_token(parser)))
            {
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected top level declaration or definition");
                recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
                return;
            }
        }

        /* FALLTHROUGH */

        // Intentionlly exclude identifier from the list of declarations that
        // we might want to exclude. And also make sure that we include '*' and
        // '(' as these can both start declarations if they are implicit int.
        case TOKEN_STAR:
        case TOKEN_LPAREN:
        case TOKEN_IDENTIFIER:
            parse_declaration_or_definition(parser);
            return;
    }
    
    panic("unreachable");
}

static void parse_translation_unit_internal(Parser* parser)
{
    // Create our file scope which will be used throughout parsing the t-unit
    Scope file = scope_new_file();
    semantic_checker_push_scope(&parser->sc, &file);

    // Check for case of empty translation unit which is not allowed.
    if (is_match(parser, TOKEN_EOF))
    {
        diagnostic_warning_at(parser->dm, current_token_start_location(parser),
                "ISO C requires a translation unit to contain at least one "
                "declaration");
    }

    while (!is_match(parser, TOKEN_EOF))
    {
        parse_top_level(parser);
    }

    // Here we can pop and delete since all of our needed decl's are in the top
    // level delcaration vector.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&file);
}

void parse_translation_unit(DiagnosticManager* dm, Preprocessor* pp)
{
    Parser parser;
    parser.dm = dm;
    parser.pp = pp;
    preprocessor_advance_token(pp, &parser.token);
    parser.ast = ast_create();
    parser.sc = sematic_checker_create(dm, &pp->identifiers, &parser.ast);
    parser.paren_count = 0;
    parser.bracket_count = 0;
    parser.brace_count = 0;

    parse_translation_unit_internal(&parser);

    ast_delete(&parser.ast);
}
