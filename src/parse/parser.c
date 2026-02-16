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

#include "driver/lang.h"
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
static const TokenType type_qualifier[] = {TOK_const, TOK_restrict,
        TOK_volatile};
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
    preprocessor_peek_token(&parser->pp, &parser->peek_token);

    return &parser->peek_token;
}

static TokenType current_token_type(Parser* parser)
{
    return parser->token.type;
}

static TokenType next_token_type(Parser* parser)
{
    return preprocessor_peek_next_token_type(&parser->pp);
}

static Location current_token_location(Parser* parser)
{
    return parser->token.loc;
}

static Location current_token_end_location(Parser* parser)
{
    return parser->token.end;
}

static Location previous_token_end_location(Parser* parser)
{
    return parser->prev_token.end;
}

static Location consume(Parser* parser)
{
    // This method helps us to automatically track the braces and brackets!
    switch (current_token_type(parser))
    {   
        // Bail out this probably wasn't intended
        case TOK_EOF:
            panic("attempting to consume EOF token!");
            break;

        case TOK_LPAREN:
            parser->paren_count++;
            break;

        case TOK_RPAREN:
            if (parser->paren_count)
            {
                parser->paren_count--;
            }
            break;

        case TOK_LBRACKET:
            parser->bracket_count++;
            break;

        case TOK_RBRACKET:
            if(parser->bracket_count)
            {
                parser->bracket_count--;
            }
            break;

        case TOK_LCURLY:
            parser->brace_count++;
            break;

        case TOK_RCURLY:
            if(parser->brace_count)
            {
                parser->brace_count--;
            }
            break;

        default:
            break;
    }

    // Set the previous token to be the current token we want to refer to
    parser->prev_token = parser->token;

    // Get the location of the current token that we have so we can return that
    Location location = current_token_location(parser);

    // Advance the token to the next one
    preprocessor_advance_token(&parser->pp, &parser->token);

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
    return preprocessor_peek_next_token_type(&parser->pp) == type;
}

static void recover_many(Parser* parser, TokenType* types, size_t num_types,
        RecoverFlags flags)
{
    bool has_skipped = false;
    size_t paren_count = 0;
    size_t bracket_count = 0;
    size_t curly_count = 0;
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
            case TOK_EOF:
                return;

            // For each of our paren thesis types make sure we try to balance
            // then as best as possible
            case TOK_LPAREN:
                paren_count++;
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOK_RPAREN}, 1,
                        RECOVER_EAT_TOKEN);
                break;

            case TOK_LBRACKET:
                bracket_count++;
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOK_RBRACKET}, 1, 
                        RECOVER_EAT_TOKEN);
                break;

            case TOK_LCURLY:
                curly_count++;
                consume(parser);
                recover_many(parser, (TokenType[1]) {TOK_RCURLY}, 1, 
                        RECOVER_EAT_TOKEN);
                break;

            // For our closing types we need to do something different
            case TOK_RPAREN:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (paren_count)
                {
                    paren_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                consume(parser);
                break;

            case TOK_RBRACKET:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (bracket_count)
                {
                    bracket_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                consume(parser);
                break;
                
            case TOK_RCURLY:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (curly_count)
                {
                    curly_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                consume(parser);
                break;

            // Check if we were meant to stop at a semi
            case TOK_SEMI:
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

static Location eat_all(Parser* parser, TokenType type)
{
    assert(is_match(parser, type));

    Location first = current_token_location(parser);
    while (is_match(parser, type))
    {
        consume(parser);
    }

    return first;
}

static bool is_typename_start(Parser* parser, const Token* tok);
static bool is_expression_start(Parser* parser, const Token* tok);
static bool is_statement_start(Parser* parser, const Token* tok);
static bool is_initializer_start(Parser* parser, const Token* tok);

static void parse_attributes(Parser* parser);

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
static bool is_designation_start(Parser* parser);
static DesignatorList* parse_designator_list(Parser* parser);
static Initializer* parse_initializer(Parser* parser);
static Initializer* parse_initializer_list(Parser* parser);

static Declarator parse_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext ctx);
static DeclarationGroup parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context,
        Location* trailing_semi);

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

static Declaration* parse_static_assert_declaration(Parser* parser,
        DeclaratorContext context, Location* trailing_semi);
static DeclarationGroup parse_declaration(Parser* parser,
        DeclaratorContext context, Location* trailing_semi);
static QualifiedType parse_type_name(Parser* parser, bool* okay);

static bool is_typename_start(Parser* parser, const Token* tok)
{
    const TokenType type = tok->type;
    switch (type)
    {
        // Type speicifiers
        case TOK_void:
        case TOK_char:
        case TOK_short:
        case TOK_int:
        case TOK_long:
        case TOK_float:
        case TOK_double:
        case TOK_signed:
        case TOK_unsigned:
        case TOK__Bool:
        case TOK__Complex:
        case TOK__Imaginary:
        case TOK_struct:
        case TOK_union:
        case TOK_enum:
        case TOK_bool:
        case TOK__Decimal128:
        case TOK__Decimal32:
        case TOK__Decimal64:

        // Type qualifiers
        case TOK_const:
        case TOK_volatile:
        case TOK_restrict:
        case TOK__Atomic:

        // Function specifiers
        case TOK_inline:
        case TOK__Noreturn:

        // Storage classes
        case TOK_typedef:
        case TOK_extern:
        case TOK_static:
        case TOK_register:
        case TOK_auto:
        case TOK__Thread_local: // TODO: check storage spec
        case TOK_thread_local: // TODO: check storage spec
        case TOK_constexpr:

        // Alignment speicfiers
        case TOK__Alignas:
        case TOK_alignas:

        // Typeof something
        case TOK_typeof:
        case TOK_typeof_unqual:
            return true;

        case TOK_IDENTIFIER:
        {
            Identifier* identifier = tok->data.identifier;
            return semantic_checker_identifier_is_typename(&parser->sc,
                    identifier);
        }

        // Not a typename start but is a type of declaration
        case TOK_static_assert:
        case TOK__Static_assert:
            return true;
    
        default:
            return false;
    }
}

static bool is_expression_token(TokenType type)
{
    switch (type)
    {
        case TOK_NUMBER:
        case TOK_WIDE_CHARACTER:
        case TOK_WIDE_STRING:
        case TOK_CHARACTER:
        case TOK_STRING:
        case TOK_LPAREN:
        case TOK_PLUS_PLUS:
        case TOK_MINUS_MINUS:
        case TOK_AND:
        case TOK_STAR:
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_NOT:
        case TOK_TILDE:
        case TOK_sizeof:
        case TOK_IDENTIFIER:

        case TOK__Alignof:
        case TOK_alignof:
        case TOK_false:
        case TOK_true:
        case TOK_nullptr:
        case TOK__Generic:

        // Builtin identifiers
        case TOK___func__:

        // Builtin functions with special parsing
        case TOK___builtin_va_arg:
        case TOK___builtin_offsetof:
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

    if (is_expression_token(type) && type != TOK_IDENTIFIER)
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
        case TOK_LCURLY:
        case TOK_case:
        case TOK_default:
        case TOK_if:
        case TOK_switch:
        case TOK_while:
        case TOK_do:
        case TOK_for:
        case TOK_goto:
        case TOK_continue:
        case TOK_break:
        case TOK_return:
        case TOK_SEMI:
        case TOK_IDENTIFIER:

        case TOK_asm: // For inline assembly statements
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
    return is_expression_start(parser, tok) || token_is_type(tok, TOK_LCURLY);
}

// // We want to parse attributes like gcc so based on the comments in the GCC
// // C parser we parse attributes the same way.
// // TODO: look at the GCC parser since it accepts some keywords but not others?
// static bool parser_is_gnu_any_word(Parser* parser)
// {
//     switch (current_token_type(parser))
//     {
//         case TOK_IDENTIFIER:
//             return true;

//         default:
//             return false;
//     }
// }

// static void parse_gnu_attribute(Parser* parser)
// {
//     assert(is_match(parser, TOK___attribute__));

//     Location attribute = consume(parser);

//     Location lparen_loc_1;
//     if (!try_match(parser, TOK_LPAREN, &lparen_loc_1))
//     {
//         diagnostic_error_at(parser->dm, lparen_loc_1,
//                 "expected '(' after attribute");
//         recover(parser, TOK_RPAREN, RECOVER_STOP_AT_SEMI | RECOVER_EAT_TOKEN);
//         return;
//     }
    
//     Location lparen_loc_2;
//     if (!try_match(parser, TOK_LPAREN, &lparen_loc_2))
//     {
//         diagnostic_error_at(parser->dm, lparen_loc_2, "expected '(' after '('");
//         recover(parser, TOK_RPAREN, RECOVER_STOP_AT_SEMI | RECOVER_EAT_TOKEN);
//         return;
//     }

//     // Here we actually parse the attribute body itself
//     while (true)
//     {
//         // Match any identifier like thing
//         if (parser_is_gnu_any_word(parser))
//         {
//             consume(parser);
//             Location lparen_loc_attr = LOCATION_INVALID;
//             Location rparen_loc_attr = LOCATION_INVALID;
//             if (is_match(parser, TOK_LPAREN))
//             {
//                 lparen_loc_attr = consume(parser);
//             }
//         }

//         // Match the possible empty attribute
//         if (is_match(parser, TOK_COMMA))
//         {
//             consume(parser);
//             continue;
//         }

//         break;
//     }

//     Location rparen_loc_1;
//     if (!try_match(parser, TOK_RPAREN, &rparen_loc_1))
//     {
//         diagnostic_error_at(parser->dm, rparen_loc_1, "expected ')'");
//         recover(parser, TOK_RPAREN, RECOVER_STOP_AT_SEMI | RECOVER_EAT_TOKEN);
//         return;
//     }

//     Location rparen_loc_2;
//     if (!try_match(parser, TOK_RPAREN, &rparen_loc_2))
//     {
//         diagnostic_error_at(parser->dm, rparen_loc_2, "expected ')'");
//         recover(parser, TOK_RPAREN, RECOVER_STOP_AT_SEMI | RECOVER_EAT_TOKEN);
//         return;
//     }
// }

// static void parse_gnu_attributes(Parser* parser)
// {
//     assert(is_match(parser, TOK___attribute__));

//     while (is_match(parser, TOK___attribute__))
//     {
//         parse_gnu_attribute(parser);
//     }
// }

static bool is_string_token(Parser* parser, const Token* tok)
{
    (void) parser;

    switch (tok->type)
    {
        case TOK_STRING:
        case TOK_WIDE_STRING:
            return true;

        default:
            return false;
    }
}

static Expression* parse_parenthesised_expression(Parser* parser)
{
    assert(is_match(parser, TOK_LPAREN));

    Location lparen_loc = consume(parser);
    Expression* expr = parse_expression(parser);

    // Don't create the next expression at all, rather, return the
    // expression we just parsed so save us some trouble.
    Location rparen_loc = LOCATION_INVALID;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ')' after expression");
        return expr;
    }

    return semantic_checker_handle_parenthesis_expression(&parser->sc,
            lparen_loc, expr, rparen_loc);
}

static Expression* parse_reference_expression(Parser* parser)
{
    assert(is_match(parser, TOK_IDENTIFIER));

    Identifier* identifier = current_token(parser)->data.identifier;
    Location identifer_loc = consume(parser);

    bool is_function_call = is_match(parser, TOK_LPAREN);
    return semantic_checker_handle_reference_expression(&parser->sc,
            identifer_loc, identifier, is_function_call);
}

static Expression* parse_numeric_expression(Parser* parser)
{
    assert(is_match(parser, TOK_NUMBER));

    Token number_tok = *current_token(parser);
    Location loc = consume(parser);

    LiteralValue value = {0};
    bool success = parse_preprocessing_number(&value, parser->dm,
            &number_tok);
    
    return semantic_checker_handle_number_expression(&parser->sc,
            loc, value, success);
}

static Expression* parse_character_expression(Parser* parser)
{
    assert(is_match_two(parser, TOK_CHARACTER, TOK_WIDE_CHARACTER));

    bool wide = is_match(parser, TOK_WIDE_CHARACTER);
            
    Token char_token = *current_token(parser);
    Location loc = consume(parser);

    CharValue value = {0};
    bool success = parse_char_literal(&value, parser->dm, &char_token,
            wide);

    return semantic_checker_handle_char_expression(&parser->sc, loc,
            value, success);
}

static bool is_string_like_token(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOK_STRING:
        case TOK_WIDE_STRING:
            return true;

        default:
            return false;
    }
}

static Expression* parse_string_expression(Parser* parser, bool unevalulated)
{
    assert(is_string_like_token(parser));

    Location start_location = current_token_location(parser);

    // Track if the token is wide to make conversion easier
    TokenList strings = token_list(arena_new_default());
    do
    {
        Token string = *current_token(parser);
        token_list_push(&strings, string);
        consume(parser);
    }
    while (is_string_like_token(parser));

    // Attempt the conversion using the information we have here
    StringLiteral string;
    bool conversion = parse_string_literal(&parser->ast->ast_allocator,
            &string, parser->dm, parser->lang, &strings, unevalulated);

    // Make sure to free our token list since we are done with it
    token_list_free(&strings);

    // Finally, create our string expression.
    return semantic_checker_handle_string_expression(&parser->sc,
            start_location, string, conversion);
}

static Expression* parse_builtin_identifier(Parser* parser)
{
    assert(is_match(parser, TOK___func__));

    Location location = consume(parser);
    return semantic_checker_handle_builtin_identifier(&parser->sc,
            location);
}

static void recover_end_of_builtin(Parser* parser)
{
    recover(parser, TOK_RPAREN, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
}

static Expression* parse_builtin_va_arg(Parser* parser)
{
    assert(is_match(parser, TOK___builtin_va_arg));

    Location builtin_loc = consume(parser);

    Location lparen_loc;
    if (!try_match(parser, TOK_LPAREN, &lparen_loc))
    {
        diagnostic_error_at(parser->dm, lparen_loc, "expected '('");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    Expression* expr = parse_assignment_expression(parser);

    if (!try_match(parser, TOK_COMMA, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ','");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    if (!is_typename_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected a type");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    bool okay = true;
    QualifiedType type = parse_type_name(parser, &okay);

    Location rparen_loc;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    // TODO: properly create this expression.
    return semantic_checker_handle_error_expression(&parser->sc, builtin_loc);
}

static Expression* parse_builtin_offsetof(Parser* parser)
{
    assert(is_match(parser, TOK___builtin_offsetof));

    Location builtin_loc = consume(parser);

    Location lparen_loc;
    if (!try_match(parser, TOK_LPAREN, &lparen_loc))
    {
        diagnostic_error_at(parser->dm, lparen_loc, "expected '('");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    if (!is_typename_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected a type");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    bool okay = true;
    QualifiedType type = parse_type_name(parser, &okay);

    // Now try to get the membre designator for it
    if (!try_match(parser, TOK_COMMA, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ','");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    // Now try to get the member designator.
    if (!is_match(parser, TOK_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected identifier");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    Identifier* id = current_token(parser)->data.identifier;
    Location id_loc = consume(parser);

    while (true)
    {
        if (is_match(parser, TOK_DOT))
        {
            Location dot = consume(parser);

            if (!is_match(parser, TOK_IDENTIFIER))
            {
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "expected identifier");
                recover_end_of_builtin(parser);
                return semantic_checker_handle_error_expression(&parser->sc,
                        builtin_loc);
            }

            id = current_token(parser)->data.identifier;
            id_loc = consume(parser);
        }
        else if (is_match(parser, TOK_LBRACKET))
        {
            Location lbracket_loc = consume(parser);
            Expression* expr = parse_expression(parser);
            Location rbracket_loc = LOCATION_INVALID;
            if (!try_match(parser, TOK_RBRACKET, &rbracket_loc))
            {
                diagnostic_error_at(parser->dm, rbracket_loc, "expected ']'");
                recover_end_of_builtin(parser);
                return semantic_checker_handle_error_expression(&parser->sc,
                        builtin_loc);
            }
        }
        else
        {
            break;
        }
    }

    // Finally, eat the trailing right parenthesis.
    Location rparen_loc;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, lparen_loc, "expected ')'");
        recover_end_of_builtin(parser);
        return semantic_checker_handle_error_expression(&parser->sc,
                builtin_loc);
    }

    // TODO: create this expression properly
    return semantic_checker_handle_error_expression(&parser->sc, builtin_loc);
}

static Expression* parse_primary_expression(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOK_LPAREN:
            return parse_parenthesised_expression(parser);
        
        case TOK_IDENTIFIER:
            return parse_reference_expression(parser);

        case TOK_NUMBER:
            return parse_numeric_expression(parser);

        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
            return parse_character_expression(parser);

        case TOK_STRING:
        case TOK_WIDE_STRING:
            return parse_string_expression(parser, false);

        case TOK___func__:
            return parse_builtin_identifier(parser);

        // We imitate both GCC and Clang in that va_arg is the only va_bultin
        // declared as a token in its own right. Otherwise we parse and handle
        // the other required builtins elsewhere.
        case TOK___builtin_va_arg:
            return parse_builtin_va_arg(parser);

        case TOK___builtin_offsetof:
            return parse_builtin_offsetof(parser);
        
        default:
        {
            Location err_loc = current_token_location(parser);
            diagnostic_error_at(parser->dm, err_loc, "expected expression");
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
        expression_list_push(&parser->ast->ast_allocator, list, arg);
    }
    while (try_match(parser, TOK_COMMA, NULL));
}

static Expression* parse_postfix_ending(Parser* parser, Expression* start)
{
    static const TokenType operators[] = {TOK_LBRACKET, TOK_LPAREN,
            TOK_DOT, TOK_ARROW, TOK_PLUS_PLUS, TOK_MINUS_MINUS};
    static const size_t num_operators = countof(operators);

    Expression* expr = start;
    while (has_match(parser, operators, num_operators))
    {
        switch (current_token_type(parser))
        {
            case TOK_LBRACKET:
            {
                Location lbracket_loc = consume(parser);
                Expression* member = parse_expression(parser);

                Location rbracket_loc;
                if (!try_match(parser, TOK_RBRACKET, &rbracket_loc))
                {
                    diagnostic_error_at(parser->dm, rbracket_loc,
                            "expected ']'");
                    rbracket_loc = LOCATION_INVALID;
                }

                expr = semantic_checker_handle_array_expression(&parser->sc,
                        expr, lbracket_loc, member, rbracket_loc);
                break;
            }

            case TOK_LPAREN:
            {
                Location lparen_loc = consume(parser);

                ExpressionList list = expression_list_create();
                if  (!is_match(parser, TOK_RPAREN))
                {
                    parse_argument_expression_list(parser, &list);
                }

                Location rparen_loc;
                if (!try_match(parser, TOK_RPAREN, &rparen_loc))
                {
                    diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
                    rparen_loc = LOCATION_INVALID;
                    recover(parser, TOK_RPAREN,
                            RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);

                    expr = semantic_checker_handle_error_expression(&parser->sc,
                            lparen_loc);
                    break;
                }
                
                expr = semantic_checker_handle_call_expression(&parser->sc,
                        expr, lparen_loc, list, rparen_loc);
                break;
            }

            case TOK_DOT:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOK_IDENTIFIER))
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

            case TOK_ARROW:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOK_IDENTIFIER))
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

            case TOK_PLUS_PLUS:
            {
                Location op_loc = consume(parser);
                expr = semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_POST_INCREMENT, expr, op_loc);
                break;
            }

            case TOK_MINUS_MINUS:
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
        case TOK_PLUS_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_INCREMENT, expr, op_loc);
        }

        case TOK_MINUS_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            return semantic_checker_handle_increment_expression(&parser->sc,
                        EXPRESSION_UNARY_PRE_DECREMENT, expr, op_loc);
        }

        case TOK_AND:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_address_expression(&parser->sc, expr,
                    op_loc);
        }

        case TOK_STAR:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            return semantic_checker_handle_dereference_expression(&parser->sc,
                    expr, op_loc);
        }

        case TOK_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_PLUS, op_loc, expr);
        }

        case TOK_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_MINUS, op_loc, expr);
        }

        case TOK_TILDE:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
        }

        case TOK_NOT:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return semantic_checker_handle_unary_expression(&parser->sc,
                    EXPRESSION_UNARY_NOT, op_loc, expr);
        }

        case TOK_sizeof:
        {
            Location sizeof_loc = consume(parser);
            
            if (is_match(parser, TOK_LPAREN) && 
                    is_typename_start(parser, next_token(parser)))
            {
                Location lparen_loc = consume(parser);
                QualifiedType type = parse_type_name(parser, NULL);
                Location rparen_loc;
                if (!try_match(parser, TOK_RPAREN, &rparen_loc))
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

static Expression* parse_compound_literal(Parser* parser, Location lparen_loc,
        QualifiedType type, Location rparen_loc, bool okay)
{
    assert(is_match(parser, TOK_LCURLY));

    Initializer* initializer = parse_initializer(parser);
    
    // Create the compound literal and go on to parse the postfix ending.
    Expression* compound_literal = semantic_checker_handle_compound_literal(
            &parser->sc, lparen_loc, type, rparen_loc, okay, initializer);
    
    return parse_postfix_ending(parser, compound_literal);
}

static Expression* parse_cast_expression(Parser* parser)
{
    // If we cant possible have a cast expression just parse a unary expression
    if (!is_match(parser, TOK_LPAREN)
            || !is_typename_start(parser, next_token(parser)))
    {
        return parse_unary_expression(parser);
    }
    assert(is_match(parser, TOK_LPAREN));
    
    Location lparen_loc = consume(parser);

    bool okay = true;
    QualifiedType type = parse_type_name(parser, &okay);
    
    Location rparen_loc;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc,
                "expected ')' after type name");
    }

    // If we are parsing along and discover we in fact don't have a cast 
    // expression but rather a compound literal, make sure to parse our args
    // along and parse the compound literal. This will then correctly parse the
    // postfix expression after.
    if (is_match(parser, TOK_LCURLY))
    {
        return parse_compound_literal(parser, lparen_loc, type, rparen_loc,
                okay);
    }

    Expression* expr = parse_cast_expression(parser);

    // Finally, create the cast expression for parsing.
    return semantic_checker_handle_cast_expression(&parser->sc, lparen_loc,
            type, rparen_loc, expr);
}

static Expression* parse_multiplicative_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_STAR, TOK_SLASH, 
            TOK_PERCENT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_cast_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
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
        Location op_loc = consume(parser);
        Expression* rhs = parse_cast_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_additive_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_PLUS, TOK_MINUS};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_multiplicative_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOK_PLUS:
                type = EXPRESSION_BINARY_ADD;
                break;

            case TOK_MINUS:
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
    static const TokenType operators[] = {TOK_LT_LT, TOK_GT_GT};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_additive_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOK_LT_LT:
                type = EXPRESSION_BINARY_SHIFT_LEFT;
                break;

            case TOK_GT_GT:
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
    static const TokenType operators[] = {TOK_LT, TOK_GT, TOK_LT_EQUAL, 
            TOK_GT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_shift_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
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
        Location op_loc = consume(parser);
        Expression* rhs = parse_shift_expression(parser);

        expr = semantic_checker_handle_arithmetic_expression(&parser->sc, type,
                expr, op_loc, rhs);
    }

    return expr;
}

static Expression* parse_equality_expression(Parser* parser)
{
    static const TokenType operators[] = {TOK_EQUAL_EQUAL, TOK_NOT_EQUAL};
    static const size_t num_operators = countof(operators);

    Expression* expr = parse_relational_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        ExpressionType type;
        switch (current_token_type(parser))
        {
            case TOK_EQUAL_EQUAL:
                type = EXPRESSION_BINARY_EQUAL;
                break;

            case TOK_NOT_EQUAL:
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

    while (is_match(parser, TOK_AND))
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

    while (is_match(parser, TOK_XOR))
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

    while (is_match(parser, TOK_OR))
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

    while (is_match(parser, TOK_AND_AND))
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

    while (is_match(parser, TOK_OR_OR))
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

    if (is_match(parser, TOK_QUESTION))
    {
        Location question = consume(parser);

        Expression* true_expr = parse_expression(parser);

        // Note: both clang and gcc seem to act as if colon existed anyways
        Location colon;
        if (!try_match(parser, TOK_COLON, &colon))
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
    static const TokenType operators[] = {TOK_EQUAL, TOK_STAR_EQUAL, 
            TOK_SLASH_EQUAL, TOK_PERCENT_EQUAL, TOK_PLUS_EQUAL, 
            TOK_MINUS_EQUAL, TOK_LT_LT_EQUAL, TOK_GT_GT_EQUAL,
            TOK_AND_EQUAL, TOK_XOR_EQUAL, TOK_OR_EQUAL};
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
    return parse_conditional_expression(parser);
}

static Expression* parse_expression(Parser* parser)
{
    Expression* expr = parse_assignment_expression(parser);

    while (is_match(parser, TOK_COMMA))
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
    if (is_match(parser, TOK_RCURLY))
    {
        if (!lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, current_token_location(parser),
                    "%s at end of compound statement is a C23 extension", ctx);
        }
    
        // Return an empty statement so we can actually build the label without
        // error.
        return semantic_checker_handle_empty_statement(&parser->sc,
                LOCATION_INVALID);
    }
    else if (is_typename_start(parser, current_token(parser)))
    {
        if (!lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, current_token_location(parser),
                    "%s followed by a declaration is a C23 extension", ctx);
        }
        // Don't return and go on to parse a statement after.
    }
    else if (!is_statement_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected expression after %s", ctx);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    return parse_statement(parser, true);
}

static Statement* parse_label_statement(Parser* parser)
{
    assert(is_match(parser, TOK_IDENTIFIER));
    assert(is_next_match(parser, TOK_COLON));

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
    if (!is_match(parser, TOK_COLON))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ':' after '%s'", context);
        return LOCATION_INVALID;
    }

    return consume(parser);
}

static Statement* parse_case_statement(Parser* parser)
{
    assert(is_match(parser, TOK_case));

    Location case_loc = consume(parser);
    
    Expression* expr = parse_constant_expression(parser);
    if (!semantic_checker_check_case_expression(&parser->sc, &expr))
    {
        if (!is_match(parser, TOK_COLON))
        {
            recover_two(parser, TOK_COLON, TOK_RCURLY, RECOVER_STOP_AT_SEMI);
            return semantic_checker_handle_error_statement(&parser->sc);
        }
    }

    // Parse and error about unsupported GCC extension of case ranges. Note, 
    // that this is not even inputted into the handling of case statements and 
    // that an error statement is created if we encounter this.
    Location dots = LOCATION_INVALID;
    Expression* rhs = NULL;
    if (is_match(parser, TOK_ELIPSIS))
    {
        dots = consume(parser);
        diagnostic_error_at(parser->dm, dots,
                "GNU case range extension not supported");

        rhs = parse_constant_expression(parser);
        if (!semantic_checker_check_case_expression(&parser->sc, &rhs))
        {
            if (!is_match(parser, TOK_COLON))
            {
                recover_two(parser, TOK_COLON, TOK_RCURLY,RECOVER_STOP_AT_SEMI);
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
    assert(is_match(parser, TOK_default));

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
    assert(is_match(parser, TOK_LCURLY));

    // TODO: C90 compatibility, don't allow for mixing declarations and code
    Location l_curly = consume(parser);

    // bool c90_mode = !lang_opts_c99(parser->lang);
    // bool c90_decl_allowed = true;

    Statement* first = NULL;
    Statement* current = NULL;
    while (!is_match_two(parser, TOK_RCURLY, TOK_EOF))
    {
        // Attempt to parse a statement
        Statement* next = parse_statement(parser, true);

        // only in c90 mode do we try anything here
        // if (c90_mode)
        // {
        //     bool is_decl_stmt = statement_is(next, STATEMENT_DECLARATION);
            
        //     if (is_decl_stmt && !c90_decl_allowed)
        //     {
        //         Declaration* decl = statement_declaration_get(next);
        //         diagnostic_warning_at(parser->dm,
        //                 declaration_get_location(decl), "mixing declarations "
        //                 "and code is a C99 extension");
        //     }

        //     // Then if it's not a declaration statement disallow further 
        //     // declarations.
        //     if (!is_decl_stmt)
        //     {
        //         c90_decl_allowed = false;
        //     }
        // }

        // Check if we got a statement at all, if not just don't do the next
        // part
        if (next == NULL)
        {
            continue;
        }

        // Update the current statement and the first statement.
        if (first == NULL)
        {
            first = next;
            current = first;
        }
        else
        {
            statement_set_next(current, next);
            current = next;
        }
    }

    Location r_curly = LOCATION_INVALID;
    if (!try_match(parser, TOK_RCURLY, &r_curly))
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
    assert(is_match(parser, TOK_LCURLY));

    // Set-up the new scope for declarations
    Scope scope = scope_block(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* stmt = parse_compound_statement_internal(parser);

    // Make sure to pop the scope at the end.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return stmt;
}

static Location parse_trailing_semi(Parser* parser, const char* context)
{
    if (!is_match(parser, TOK_SEMI))
    {
        diagnostic_error_at(parser->dm, previous_token_end_location(parser) + 1,
                "expected ';' after %s", context);

        // Only attempt some recovery if we arent at the end of a compound stmt.
        // Note that if this is at top level it will emit an extraneous closing
        // brace warning which will need to be fixed anyways so I think this is
        // fine.
        recover(parser, TOK_RCURLY, RECOVER_STOP_AT_SEMI);

        // Finally, if we actually stopped on a semi consume it. We are already
        // in a bad error path so this extra check if fine to have.
        if (is_match(parser, TOK_SEMI))
        {
            consume(parser);
        }

        return LOCATION_INVALID;
    }

    return consume(parser);
}

static Statement* parse_expression_statement(Parser* parser)
{
    // NOTE: this can be achieved through being called to parse a statement and
    // mathing nothing, when we should be matching the end of a compound stmt
    if (is_match(parser, TOK_RCURLY))
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
        recover_two(parser, TOK_RCURLY, TOK_SEMI, RECOVER_NONE);

        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(is_expression_start(parser, current_token(parser)));

    Expression* expr = parse_expression(parser);

    // Another clang special case error message which we support.
    if (is_match(parser, TOK_RPAREN) && is_next_match(parser, TOK_SEMI))
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
    if (!is_match(parser, TOK_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected '(' after '%s'", context);
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);

        return false;
    }

    *lparen_loc = consume(parser);

    // Parse and check the condition is valid
    *cond = parse_expression(parser);
    *cond = semantic_checker_check_condition(&parser->sc, kw_location, *cond,
            is_switch, context);

    if (!is_match(parser, TOK_RPAREN))
    {
        // TODO: could better error recovery here be to check for a statement
        // TODO: start somehow and then only sometimes killing the parse
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ')' after condition");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
        return false;
    }

    *rparen_loc = consume(parser);

    // Clang will emit errors for exsessive paren usage. But continue on parsing
    // as if there was no error, since this error is likely only limited to one
    // paren anyways
    while (is_match(parser, TOK_RPAREN))
    {   
        Location extra_paren = consume(parser);
        diagnostic_error_at(parser->dm, extra_paren,
                "extraneous ')' after condition, expected a statement");
    }

    return true;
}

static Statement* parse_if_statement(Parser* parser)
{
    assert(is_match(parser, TOK_if));

    Statement* out = NULL;

    Location if_loc = consume(parser);

    Scope if_scope = scope_if(&parser->ast->ast_allocator);
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
    if (is_match(parser, TOK_else))
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
    assert(is_match(parser, TOK_switch));

    Statement* out = NULL;

    Location switch_loc = consume(parser);

    // Create and push the switch scope
    Scope switch_scope = scope_switch(&parser->ast->ast_allocator);
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

    semantic_checker_push_switch_stack(&parser->sc);

    Statement* body = parse_statement(parser, false);
    out = semantic_checker_handle_switch_statement(&parser->sc, switch_loc,
            lparen_loc, expr, rparen_loc, body);

    semantic_checker_pop_switch_stack(&parser->sc);

done:
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&switch_scope);

    return out;
}

static Statement* parse_while_statement(Parser* parser)
{
    assert(is_match(parser, TOK_while));

    Statement* out = NULL;

    Location while_loc = consume(parser);

    // Create and push the scope
    Scope while_scope = scope_while(&parser->ast->ast_allocator);
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
    assert(is_match(parser, TOK_do));

    Statement* out = NULL;

    // Consume the 'do' and then push the scope.
    Location do_loc = consume(parser);

    Scope do_while_scope = scope_do_while(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &do_while_scope);

    Statement* body = parse_statement(parser, false);
    
    // If the token is not a while just skip till we get a semi...
    if (!is_match(parser, TOK_while))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected 'while' in do/while loop");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);

        out = semantic_checker_handle_error_statement(&parser->sc);
        goto done;
    }

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
    assert(is_match(parser, TOK_for));

    Location for_loc = consume(parser);

    // Make sure we got a lparen after!
    if (!is_match(parser, TOK_LPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected '(' after 'for'");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Create and push the for scope.
    Scope for_scope = scope_for(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &for_scope);

    assert(is_match(parser, TOK_LPAREN));
    Location lparen_loc = consume(parser);

    DeclarationGroup init_declaration = decl_group_from_empty();
    Expression* init_expression = NULL;
    if (is_typename_start(parser, current_token(parser)))
    {
        // Warn if we're not in C99 mode about this extension.
        if (!lang_opts_c99(parser->lang))
        {
            diagnostic_warning_at(parser->dm, current_token_location(parser),
                    "variable declarations in for loop is a C99-specific "
                    "feature");
        }
        init_declaration = parse_declaration(parser, DECL_CTX_BLOCK, NULL);
    }
    else if (is_expression_start(parser, current_token(parser)))
    {
        init_expression = parse_expression(parser);
    }
    else if (!is_match(parser, TOK_SEMI))
    {
        // TODO: ensure this is adequete error recovery
        diagnostic_error_at(parser->dm, current_token_location(parser), 
                "expected expression");
        recover(parser, TOK_SEMI, RECOVER_NONE);
    }

    if (!try_match(parser, TOK_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* cond = NULL;
    if (!is_match(parser, TOK_SEMI))
    {
        cond = parse_expression(parser);
    }

    if (!try_match(parser, TOK_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* inc = NULL;
    if (!is_match(parser, TOK_RPAREN))
    {
        inc = parse_expression(parser);
    }

    Location rparen_loc = LOCATION_INVALID;
    if (!is_match(parser, TOK_RPAREN))
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
    assert(is_match(parser, TOK_goto));

    Location goto_loc = consume(parser);

    // Need to have an identifier next. GCC and clang have computed gotos as an
    // extension to the language but this is not supported here.
    if (!is_match(parser, TOK_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected identifier after 'goto'");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);

        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Get the identifier name and consume the identifier.
    Identifier* label_name = current_token(parser)->data.identifier;
    Location label_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "goto statement");

    return semantic_checker_handle_goto_statement(&parser->sc, goto_loc,
            label_name, label_loc, semi_loc);
}

static Statement* parse_continue_statement(Parser* parser)
{
    assert(is_match(parser, TOK_continue));

    Location continue_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "continue statement");

    return semantic_checker_handle_continue_statement(&parser->sc, continue_loc,
            semi_loc);
}

static Statement* parse_break_statement(Parser* parser)
{
    assert(is_match(parser, TOK_break));

    Location break_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "break statement");

    return semantic_checker_handle_break_statement(&parser->sc, break_loc,
            semi_loc);
}

static Statement* parse_return_statement(Parser* parser)
{
    assert(is_match(parser, TOK_return));

    Location return_loc = consume(parser);

    Expression* expr_opt = NULL;
    if (!is_match(parser, TOK_SEMI))
    {
        expr_opt = parse_expression(parser);
    }
    Location semi_loc = parse_trailing_semi(parser, "return statement");

    return semantic_checker_handle_return_statement(&parser->sc, return_loc,
            expr_opt, semi_loc);
}

static Statement* parse_declaration_statement(Parser* parser)
{
    Location semi = LOCATION_INVALID;
    DeclarationGroup decls = parse_declaration(parser, DECL_CTX_BLOCK, &semi);
    return semantic_checker_handle_declaration_statement(&parser->sc,
            decls, semi);
}

static Statement* parse_empty_statement(Parser* parser)
{
    assert(is_match(parser, TOK_SEMI));

    Location semi_loc = consume(parser);
    return semantic_checker_handle_empty_statement(&parser->sc, semi_loc);
}

static Statement* parse_error_statement(Parser* parser)
{
    recover_two(parser, TOK_RCURLY, TOK_SEMI, RECOVER_NONE);
    return semantic_checker_handle_error_statement(&parser->sc);
}

static Statement* parse_statement(Parser* parser, bool declaration_allowed)
{
    switch(current_token_type(parser))
    {
        case TOK_LCURLY:
            return parse_compound_statement(parser);

        case TOK_case:
            return parse_case_statement(parser);

        case TOK_default:
            return parse_default_statement(parser);

        case TOK_if:
            return parse_if_statement(parser);

        case TOK_switch:
            return parse_switch_statement(parser);

        case TOK_while:
            return parse_while_statement(parser);

        case TOK_do:
            return parse_do_while_statement(parser);

        case TOK_for:
            return parse_for_statement(parser);

        case TOK_goto:
            return parse_goto_statement(parser);

        case TOK_continue:
            return parse_continue_statement(parser);

        case TOK_break:
            return parse_break_statement(parser);

        case TOK_return:
            return parse_return_statement(parser);

        case TOK_SEMI:
            return parse_empty_statement(parser);

        case TOK_IDENTIFIER:
            if (is_next_match(parser, TOK_COLON))
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
    return has_match(parser, (TokenType[2]) {TOK_DOT, TOK_LBRACKET}, 2);
}

static Designator* parse_member_designator(Parser* parser)
{
    assert(is_match(parser, TOK_DOT));

    Location dot = consume(parser);

    if (!is_match(parser, TOK_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected a field designator, such as '.field = 4'");
        return NULL;
    }

    Identifier* identifier = current_token(parser)->data.identifier;
    Location identifier_location = consume(parser);

    return designator_create_member(&parser->ast->ast_allocator, dot,
            identifier_location, identifier);
}

static Designator* parse_array_designator(Parser* parser)
{
    assert(is_match(parser, TOK_LBRACKET));

    Location lbracket = consume(parser);
    Expression* expr = parse_constant_expression(parser);
    Location rbracket;
    if (!try_match(parser, TOK_RBRACKET, &rbracket))
    {
        diagnostic_error_at(parser->dm, rbracket,
                "expected ']' after designator");
        recover(parser, TOK_RBRACKET, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
    }

    // Create the array designator even if we didn't get the rbracket.
    return designator_create_array(&parser->ast->ast_allocator, lbracket, expr,
            rbracket);
}

static Designator* parse_designator(Parser* parser)
{
    assert(is_designation_start(parser));

    if (is_match(parser, TOK_DOT))
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
            recover(parser, TOK_RCURLY, RECOVER_STOP_AT_SEMI);
            return NULL;
        }

        // Create our list member that we are going to use.
        DesignatorList* l = designator_list_create(&parser->ast->ast_allocator,
                designator);
        
        // Finally, build our designator list.
        if (start == NULL)
        {
            start = l;
        }
        current = designator_list_set_next(current, l);
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

        if (!try_match(parser, TOK_EQUAL, &equal_loc))
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
    return initializer_list_member_create(&parser->ast->ast_allocator, list,
            equal_loc, init);
}

static Initializer* parse_initializer_list(Parser* parser)
{   
    assert(is_match(parser, TOK_LCURLY));

    Location lcurly = consume(parser);
    Location rcurly;

    // See if we match an empty initializer list, and if it is then create and
    // empty list returning from the function early.
    if (try_match(parser, TOK_RCURLY, &rcurly))
    {
        if (lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, lcurly,
                "use of an empty initializer is a C23 extension");
        }
        
        // TODO: i think this return can be removed here and change the
        // try_match to an is_match
        return semantic_checker_initializer_from_list(&parser->sc, lcurly,
                NULL, rcurly);
    }

    bool list_okay = true;
    InitializerListMember* first_member = NULL;
    InitializerListMember* current_member = NULL;
    do
    {
        // If we see the end of an initializer list then we are simply done.
        if (is_match(parser, TOK_RCURLY))
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
    while (try_match(parser, TOK_COMMA, NULL));

    // Try to match the end of the initializer
    if (!try_match(parser, TOK_RCURLY, &rcurly))
    {
        diagnostic_error_at(parser->dm, rcurly,
                "expected '}' after initializer");
        recover(parser, TOK_RCURLY, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
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
    if (is_match(parser, TOK_LCURLY))
    {
        return parse_initializer_list(parser);
    }
    return parse_initializer_assignment_expression(parser);
}

static void parse_declarator_internal(Parser* parser, Declarator* declarator)
{
    // If we get a pointer before our declarator do this first.
    if (is_match(parser, TOK_STAR))
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
            &parser->ast->ast_allocator);

    // If we don't allow bitfields or if we allow them and we don't match a ':'
    // parse a declarator how we would expect it to. Making sure to parse the
    // bitfield at the end if we need.
    bool bitfields = declarator_allowed_bitfields(&declarator);
    if (!bitfields || !is_match(parser, TOK_COLON))
    {
        parse_declarator_internal(parser, &declarator);
    }

    // To parse a bitfield we must first make sure that they area allowed then
    // we must also match a ':' to then try to parse one.
    if (bitfields && is_match(parser, TOK_COLON))
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
    Scope prototype = scope_function_prototype(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &prototype);

    while (is_typename_start(parser, current_token(parser)))
    {
        DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
                false);

        // It is an error if we do not get a complete declaration after a param
        Location semi_loc = LOCATION_INVALID;
        if (try_match(parser, TOK_SEMI, &semi_loc))
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
        while (try_match(parser, TOK_COMMA, NULL));

        // parse the trailing semi-colon at the end of the declaration list.
        Location semi = LOCATION_INVALID;
        if (!try_match(parser, TOK_SEMI, &semi))
        {
            diagnostic_error_at(parser->dm, semi,
                    "expected ';' after declaration");
            // Should the below be LCURLY???
            recover(parser, TOK_RCURLY, RECOVER_STOP_AT_SEMI);
            if (is_match(parser, TOK_SEMI))
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
    assert(is_match(parser, TOK_LCURLY));

    recover(parser, TOK_LCURLY, RECOVER_NONE);
}

static Declaration* parse_function_definition(Parser* parser,
        Declarator* declarator)
{
    assert(declarator_is_function(declarator));
    assert(!is_match(parser, TOK_SEMI));

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

    // Now process the declaration, and get the function definition that we are
    // going to use.
    Declaration* function = semantic_checker_process_declarator(&parser->sc,
            declarator);

    // If we parsed all our knr parameters but arent at a LCURLY we have a bit
    // of a problem. Otherwise, if were not a '{' we have a problem anyways.
    if (!is_match(parser, TOK_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser),
                "expected function body after function declarator");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    // Create and push our function scope before we try to add our parameters.
    FunctionScope func_scope = function_scope_create(function);
    sematic_checker_push_function_scope(&parser->sc, &func_scope);

    Scope function_body = scope_block(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &function_body);

    // Add all of our important function parameters into this scope.
    semantic_checker_add_function_parameters(&parser->sc, function);

    // Parse the compound statement of the function.
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
    assert(declarator_is_function(declarator));

    // Still true even if we're a knr function!
    if (is_match(parser, TOK_LCURLY))
    {
        return true;
    }

    // Need to know if we are knr function. If we are then the function piece
    // will be a knr type piece. If not then we should have a typename after it.
    DeclaratorPiece* piece = declarator_get_function_piece(declarator);
    if (declarator_piece_is_knr_function(piece))
    {
        return is_typename_start(parser, current_token(parser));
    }

    return false;
}

static bool has_declaration(Parser* parser)
{
    static const TokenType tokens[] = {TOK_EQUAL, TOK_COMMA, TOK_SEMI};
    return has_match(parser, tokens, countof(tokens));
}

static void parse_initializer_after_declarator(Parser* parser,
        Declaration* declaration, DeclaratorContext context)
{
    // If we don't have an initializer then we can simple return.
    if (!is_match(parser, TOK_EQUAL))
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
        recover(parser, TOK_COMMA, RECOVER_STOP_AT_SEMI);
        return;
    }

    Initializer* initializer = parse_initializer(parser);

    semantic_checker_declaration_add_initializer(&parser->sc, declaration, 
            context, equal_loc, initializer);
}

static Declaration* parse_declaration_after_declarator(Parser* parser,
        Declarator* declarator, DeclaratorContext context)
{
    // Set if we have an initializer so that we can give correct diagnostics
    // about tentative definitions and such.
    if (is_match(parser, TOK_EQUAL))
    {
        declarator_set_initializer(declarator);
    }

    // Process the declaration so that we can then parse the initializer after
    Declaration* decl = semantic_checker_process_declarator(&parser->sc,
            declarator);

    // If we get a null declaration that means a bad error has occured so bail
    // early and recover to a reasonable point. DO NOT eat the semi though, 
    // since if we are meant to the caller will handle the error, OR, it'll try
    // to get eaten in parse_declarator and issue another error.
    if (decl == NULL)
    {
        recover(parser, TOK_SEMI, RECOVER_NONE);
        return NULL;
    }

    // Parse the initializer if needed and finalize the declaration.
    parse_initializer_after_declarator(parser, decl, context);

    // Then finish the declaration and return it.
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
    if (declarator_is_function(declarator) && !has_declaration(parser))
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
                recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
                *declaration = NULL;
                return true;
            }

            *declaration = parse_function_definition(parser, declarator);
            return true;
        }
        else if (is_function_definition(parser, declarator))
        {
            // Only error if we get the start of a function definition. This is
            // where we diverge from clang and meet up with gcc in that we will
            // consider knr parameter lists here...
            // Otherwise we are not allowed to have a function at all, do 
            // not properly handle the declarator and instead error, recover
            // and return NULL
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "function definition is not allowed here");
            parse_skip_function_body(parser);
            *declaration = NULL;
            return true;
        }
    }

    return false;
}

static DeclarationGroup parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context,
        Location* trailing_semi)
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
        return decl_group_from_single(decl);
    }

    // Here since we know we don't have a function decl create a DeclList that
    // we can then use for a decl group
    DeclarationList list = declaration_list(&parser->ast->ast_allocator);

    // If the declarations specifiers had a declaration, then we should add that
    // onto the declaration list.
    // TODO: need to see what happens if the tag is not a definition at that
    // TODO: point in time and is an already defined tag etc...
    // if (declaration_specifiers_has_tag_declaration(specifiers))
    // {
    //     Declaration* d = declaration_specifiers_get_declaration(specifiers);
    //     declaration_list_push(&list, d);
    // }

    // Here we know that we shouldn't create a function definition and so we
    // can finally process the declarator and potentially try to parse an
    // initializer after the definition.
    decl = parse_declaration_after_declarator(parser, &declarator,
            context);
    if (decl != NULL)
    {
        declaration_list_push(&list, decl);
    }

    // Otherwise keep trying to parse declarations with an initializer until
    // we appear to be at the end of all of our declarations.
    while (try_match(parser, TOK_COMMA, NULL))
    {
        // Parse a fresh declarator and then a declaration after it.
        declarator = parse_declarator(parser, specifiers, context);
        decl = parse_declaration_after_declarator(parser, &declarator, context);
        if (decl != NULL)
        {
            declaration_list_push(&list, decl);
        }
    }

    // Now finally we want to match the semi-colon at the end if we were given
    // one to match
    if (trailing_semi)
    {
        const char* message = context == DECL_CTX_FILE ? "top level declarator"
                : "declaration";
        *trailing_semi = parse_trailing_semi(parser, message);
    }
    
    return decl_group_from_multiple(&list);
}

static void parse_identifier_list(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOK_LPAREN));
    assert(!lang_opts_c23(parser->lang));

    Location lparen_loc = consume(parser);
    Location rparen_loc = LOCATION_INVALID;

    // Create the list of parameter declarations that we will use to add our
    // 'declarations' too.
    DeclarationList parms = declaration_list(&parser->ast->ast_allocator);
    size_t num_parms = 0;

    // See if we got an empty parameter list. This is still an old style
    // declaration, however, we are just trying to skip doing a whole bunch of
    // work that we don't have to do
    if (is_match(parser, TOK_RPAREN))
    {
        rparen_loc = consume(parser);
        goto done;
    }

    do
    {
        if (!is_match(parser, TOK_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected identifier");
            recover(parser, TOK_RPAREN, RECOVER_NONE);
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
    while (try_match(parser, TOK_COMMA, NULL));

    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
        recover(parser, TOK_RPAREN, RECOVER_STOP_AT_SEMI);
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
    assert(is_match(parser, TOK_LPAREN));
    Location lparen_loc = consume(parser);

    // The location of the dots and if the function is variadic
    Location dots = LOCATION_INVALID;

    // The paramater declarations themselves
    DeclarationList parms = declaration_list(&parser->ast->ast_allocator);
    size_t num_parms = 0;

    // Parse the parameter list
    do
    {
        // If we are on the first parameter and we get nothing (in C23) then
        // we want to skip all of this.
        if (num_parms == 0 && is_match(parser, TOK_RPAREN))
        {
            break;
        }

        // First check if we have elipsis
        if (is_match(parser, TOK_ELIPSIS))
        {
            dots = consume(parser);

            // Check that we have a parameter. If not error, but we will try to
            // recover and ignore the error to be a bit better
            if (num_parms == 0 && !lang_opts_c23(parser->lang))
            {
                diagnostic_error_at(parser->dm, dots,
                        "ISO C requires a named parameter before '...'");
            }
            break;
        }

        // Also allow for implicit int here.
        if (!is_typename_start(parser, current_token(parser))
                && !is_match_two(parser, TOK_STAR, TOK_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected parameter declarator");
            recover_two(parser, TOK_COMMA, TOK_RPAREN, RECOVER_STOP_AT_SEMI);
            continue;
        }

        // Otherwise we will try to parse a declaration.
        Declaration* declaration = parse_paramater_declaration(parser);

        // Push the paramater into the list.
        declaration_list_push(&parms, declaration);
        num_parms++;
    }
    while (try_match(parser, TOK_COMMA, NULL));

    // Match the end of the parameter list
    Location rparen_loc;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
        recover(parser, TOK_RPAREN, RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
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
    Scope function_proto = scope_function_prototype(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &function_proto);

    // Note: we check for elipsis to help improve a possible bad parse's error
    // mesasage so that we parse it correctly as a function declarator. Also,
    // we check for a star to allow for implicit int e.g. void func(*a); But 
    // this should be the only specia case
    if (is_typename_start(parser, next_token(parser))
            || is_next_match(parser, TOK_ELIPSIS)
            || lang_opts_c23(parser->lang)
            || is_next_match(parser, TOK_STAR))
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
    assert(is_match(parser, TOK_LBRACKET));

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
    if (is_match(parser, TOK_static))
    {
        static_loc = consume(parser);
        is_static = true;
    }

    // Get the type qualifiers if any...
    TypeQualifiers qualifiers = parse_type_qualifier_list_opt(parser);

    // Test again for static, erroring if we got it twice
    if (is_match(parser, TOK_static) && !is_static)
    {
        static_loc = consume(parser);
        is_static = true;
    }
    else if (is_match(parser, TOK_static) && is_static)
    {
        Location duplicate = consume(parser);
        diagnostic_error_at(parser->dm, duplicate,
                "duplicate 'static' array qualifier");
        recover(parser, TOK_RBRACKET, RECOVER_STOP_AT_SEMI);
    }

    // Make sure we don't accidentally match the start of a dereference
    // expression. Since that would be important.
    bool is_star = false;
    if (is_match(parser, TOK_STAR) && is_next_match(parser, TOK_RBRACKET))
    {
        consume(parser);

        is_star = true;
    }

    Expression* expression = NULL;
    if (!is_match(parser, TOK_RBRACKET))
    {
        expression = parse_assignment_expression(parser);
    }

    Location rbracket_loc = LOCATION_INVALID;
    if (!try_match(parser, TOK_RBRACKET, &rbracket_loc))
    {
        diagnostic_error_at(parser->dm, rbracket_loc, "expected ']'");
        recover(parser, TOK_RBRACKET, RECOVER_STOP_AT_SEMI | RECOVER_EAT_TOKEN);
    }

    declarator_push_array(declarator, lbracket_loc, rbracket_loc, static_loc,
            qualifiers, expression, is_static, is_star);
}

static void parse_direct_declarator(Parser* parser, Declarator* declarator)
{
    if (is_match(parser, TOK_IDENTIFIER))
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
    else if (is_match(parser, TOK_LPAREN))
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
                || is_next_match(parser, TOK_RPAREN)
                || is_next_match(parser, TOK_ELIPSIS)))
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

        Location rparen_loc = LOCATION_INVALID;
        if (!try_match(parser, TOK_RPAREN, &rparen_loc))
        {
            // Error but don't return from the function in case we have a tail
            // end to parse.
            diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
            recover(parser, TOK_RPAREN,
                    RECOVER_EAT_TOKEN | RECOVER_STOP_AT_SEMI);
        }
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

        recover(parser, TOK_SEMI, RECOVER_NONE);

        declarator_set_invalid(declarator);
        return;
    }

    // Now finish parsing the declarator.
parse_func_array:
    while (true)
    {
        if (is_match(parser, TOK_LPAREN))
        {
            parse_function_declarator(parser, declarator);
            continue;
        }
        
        if (is_match(parser, TOK_LBRACKET))
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
            case TOK_const: qualifier = QUALIFIER_CONST; break;
            case TOK_restrict: qualifier = QUALIFIER_RESTRICT; break;
            case TOK_volatile: qualifier = QUALIFIER_VOLATILE; break;
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

    if (is_match_two(parser, TOK__Static_assert, TOK_static_assert))
    {
        // Location semi = LOCATION_INVALID;
        parse_static_assert_declaration(parser, DECL_CTX_STRUCT, NULL);
        goto parse_semi;
    }

    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);

    // Struct declarations will always need to have a name to be useful. Unless
    // we implement c11's anonymous struct/union injection into the scope. But,
    // we are currently a C99 only compiler and barely implement structs as is,
    // so this is a little far off for use.
    if (is_match(parser, TOK_SEMI))
    {
        // TODO: handle C11's anonymous struct / union member injection...
        Location semi = consume(parser);
        if (!lang_opts_c11(parser->lang))
        {
            diagnostic_warning_at(parser->dm, semi,
                    "declaration does not declare anything");
        }
        return;
    }

    // Main loop of parsing all of the declarators for this declaration and 
    // adding them into the structure if they are what we are looking for.
    do
    {
        Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_STRUCT);
        Declaration* member = semantic_checker_process_struct_declarator(
                &parser->sc, decl, &d);

        if (member)
        {
            declaration_struct_add_member(decl, member);
        }
    }
    while (try_match(parser, TOK_COMMA, NULL));

    // Finally parse the trailing semi-colon at the end of the list. Only 
    // issuing a warning about the GNU extension of allowing no semi at the
    // end of a structure.
parse_semi:;
    Location semi = LOCATION_INVALID;
    if (is_match(parser, TOK_RCURLY))
    {
        diagnostic_warning_at(parser->dm, current_token_location(parser),
                "expected ';' after %s declarator",
                declaration_is(decl, DECLARATION_STRUCT) ? "struct" : "union");
    }
    else
    {
        semi = parse_trailing_semi(parser, "struct declarator");
    }
}

static void parse_struct_declaration_list(Parser* parser, Declaration* decl,
        bool is_struct)
{
    assert(is_match(parser, TOK_LCURLY));
    
    Location l_curly = consume(parser);
    if (is_match(parser, TOK_RCURLY))
    {
        diagnostic_warning_at(parser->dm, declaration_get_location(decl),
                "empty %s is a GNU extension", is_struct ? "struct" : "union");
    }

    // Create and push our member scope.
    Scope member_scope = scope_member(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &member_scope);

    while (!is_match_two(parser, TOK_RCURLY, TOK_EOF))
    {
        // Consume any extra ';' inside the struct or union
        if (is_match(parser, TOK_SEMI))
        {
            Location sem = eat_all(parser, TOK_SEMI);
            diagnostic_warning_at(parser->dm, sem, "extra ';' inside a struct");
            continue;
        }

        // Expect the start of a typename not allowing implicit int.
        if (!is_typename_start(parser, current_token(parser)))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "type name requires a specifier or qualifier");
            recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
            continue;
        }

        // Otherwise we can parse a struct declaration.
        parse_struct_declaration(parser, decl);
    }

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&member_scope);

    // Try to match the closign '}'
    Location r_curly = LOCATION_INVALID;
    if (!try_match(parser, TOK_RCURLY, &r_curly))
    {
        Location current = current_token_location(parser);
        diagnostic_error_at(parser->dm, current, "expected '}'");
        recover(parser, TOK_SEMI, RECOVER_NONE);
    }

    semantic_checker_finish_struct_declaration(&parser->sc, decl);
}

static void parse_struct_or_union_specifier(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    assert(is_match_two(parser, TOK_struct, TOK_union));

    bool is_struct = is_match(parser, TOK_struct);
    DeclarationType type = is_struct ? DECLARATION_STRUCT : DECLARATION_UNION;

    Location tag_loc = consume(parser);

    Declaration* declaration = NULL;

    if (!is_match_two(parser, TOK_IDENTIFIER, TOK_LCURLY))
    {
        const char* const tag_name = is_struct ? "struct" : "union";
        diagnostic_error_at(parser->dm, tag_loc, "declaration of anonymous %s "
                "must be a definition", tag_kind_to_name(type));
        recover(parser, TOK_SEMI, RECOVER_NONE);

        // Add the error type and exit as there is nothing else to parse.
        declaration_specifiers_add_type(parser, specifiers,
                TYPE_SPECIFIER_ERROR, NULL, tag_loc);
        return;
    }

    Identifier* identifier = NULL;
    Location identifier_loc = LOCATION_INVALID;
    if (is_match(parser, TOK_IDENTIFIER))
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
    bool is_definition = is_match(parser, TOK_LCURLY);
    declaration = semantic_checker_handle_tag(&parser->sc, type, tag_loc,
            identifier, identifier_loc, is_definition);
    assert(declaration != NULL);

    // Parse the definition if we have one to parse.
    if (is_definition)
    {
        parse_struct_declaration_list(parser, declaration, is_struct);
    }

    // Finally, we will need to add the struct / union specifiers and 
    // declaration to the declspec.
    TypeSpecifierType type_spec = is_struct
            ? TYPE_SPECIFIER_STRUCT
            : TYPE_SPECIFIER_UNION;
    declaration_specifiers_add_type(parser, specifiers, type_spec,
            declaration, tag_loc);
}

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl)
{
    assert(enum_decl && declaration_is(enum_decl, DECLARATION_ENUM));
    assert(is_match(parser, TOK_LCURLY));

    // Here we can actually parse the enum
    Location opening_curly = consume(parser);

    // Check for an empty enum, setting it to be a complete declaration if so
    Location closing_curly = LOCATION_INVALID;
    if (try_match(parser, TOK_RCURLY, &closing_curly))
    {
        diagnostic_error_at(parser->dm, closing_curly, "use of empty enum");
        declaration_enum_set_entries(enum_decl, NULL, 0);
        return;
    }

    // Create our list and get our number of entries
    DeclarationList enumerators = declaration_list(&parser->ast->ast_allocator);
    size_t num_entries = 0;

    Declaration* previous = NULL;
    while (!is_match(parser, TOK_EOF))
    {
        // Here parse the enumerator declaration placing it in the symbol table
        if (!is_match(parser, TOK_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected identifier");
            recover_three(parser, TOK_COMMA, TOK_RCURLY, TOK_IDENTIFIER,
                    RECOVER_STOP_AT_SEMI);

            if (is_match(parser, TOK_COMMA))
            {
                consume(parser);
                continue;
            }
                
            if (is_match(parser, TOK_RCURLY))
            {
                break;
            }
        }

        // Due to previous potential error recovery
        assert(is_match(parser, TOK_IDENTIFIER));

        Identifier* identifier = current_token(parser)->data.identifier;
        Location identifier_loc = consume(parser);

        // Get the expresison first since we cannot build the enum value with
        // the identifier itself
        Location equal_loc = LOCATION_INVALID;
        Expression* expression = NULL;
        if (is_match(parser, TOK_EQUAL))
        {
            equal_loc = consume(parser);
            expression = parse_constant_expression(parser);
        }

        // Create the declaration. Note, this handles any redefinitions that
        // we might get which is nice and convenient.
        Declaration* constant_decl = semantic_checker_handle_enum_constant(
                &parser->sc, identifier_loc, identifier, equal_loc, expression,
                previous);

        // Add to our list, increase number of enumerators and update the 
        // previous declaration to the new one. If any of the declarations were
        // invalid then don't include it howver.
        if (declaration_is_valid(constant_decl))
        {
            declaration_list_push(&enumerators, constant_decl);
            num_entries++;
            previous = constant_decl;
        }
        else
        {
            previous = NULL;
        }
       

        // Now see if we are at the end and finish the definition  
        if (!is_match_two(parser, TOK_RCURLY, TOK_COMMA))
        {
            if (is_match(parser, TOK_IDENTIFIER))
            {
                // Common error, no recovery needed, just restart loop
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "missing ',' between enumerators");
                continue;
            }
            
            if (is_match(parser, TOK_SEMI) && expression == NULL)
            {
                diagnostic_error_at(parser->dm, current_token_location(parser),
                        "expected '= constant-expression' or end of "
                        "enumerator definition");
                recover(parser, TOK_RCURLY, RECOVER_NONE);
                break;
            }
            
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected ',' or '}' after enumerator");
            recover_two(parser, TOK_COMMA, TOK_RCURLY, RECOVER_NONE);
        }

        if (is_match(parser, TOK_COMMA))
        {
            consume(parser);
        }

        if (is_match(parser, TOK_RCURLY))
        {
            break;
        }
    }

    // Finally, finish our our enum with all of the entries that are in it.
    declaration_enum_set_entries(enum_decl, declaration_list_iter(&enumerators),
            num_entries);

    closing_curly = LOCATION_INVALID;
    if (!try_match(parser, TOK_RCURLY, &closing_curly))
    {
        diagnostic_error_at(parser->dm, closing_curly, "expected '}'");
        recover(parser, TOK_SEMI, RECOVER_NONE);
    }
}

static void parse_enum_specificier(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    assert(is_match(parser, TOK_enum));

    // Track if we got a fatal error whilst parsing the enum. and pre-init the
    // declaration.
    Declaration* declaration = NULL;
    bool error = false;

    Location enum_location = consume(parser);

    // We need to have a match for one of these here otherwise we have a problem
    if (!is_match_two(parser, TOK_IDENTIFIER, TOK_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_location(parser), 
                "expected identifier or '{' after 'enum'");
        recover(parser, TOK_SEMI, RECOVER_NONE);
        error = true;
        goto finish;
    }

    Identifier* identifier = NULL;
    Location identifier_loc = LOCATION_INVALID;
    if (is_match(parser, TOK_IDENTIFIER))
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
    bool is_definition = is_match(parser, TOK_LCURLY);
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

static QualifiedType parse_type_name(Parser* parser, bool* okay)
{
    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);
    Declarator d = parse_declarator(parser, &specifiers, DECL_CTX_TYPE_NAME);
    QualifiedType type = semantic_checker_process_typename(&parser->sc, &d);

    if (okay && declarator_is_invalid(&d))
    {
        *okay = false;
    }

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
    // Do the check here instead to clean up parsing of decl-spec
    if (specifiers->type_spec_width == WIDTH_SPECIFIER_LONG
            && width == WIDTH_SPECIFIER_LONG)
    {
        width = WIDTH_SPECIFIER_LONG_LONG;
    }

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

static bool parse_typedef_name(Parser* parser, DeclarationSpecifiers* dspec)
{
    assert(is_match(parser, TOK_IDENTIFIER));

    // If were not allowed a typename were are likely done
    if (!declaration_specifiers_allow_typename(dspec))
    {
        return false;
    }

    // Get the current token and the Identifier from it so we can try a typename
    // lookup.
    Token current = *current_token(parser);
    Identifier* ident = current.data.identifier;

    // Try to get the typename and if it is not null then add it.
    Declaration* decl = semantic_checker_get_typename(&parser->sc, ident);
    if (decl != NULL)
    {
        declaration_specifiers_add_type(parser, dspec, TYPE_SPECIFIER_TYPENAME,
                decl, current_token_location(parser));
        return true;
    }

    return false;
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
            case TOK_typedef:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_TYPEDEF, spec_qual_only, location);
                break;

            case TOK_extern:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_EXTERN, spec_qual_only, location);
                break;

            case TOK_static:
               declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_STATIC, spec_qual_only, location);
                break;

            case TOK_auto:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_AUTO, spec_qual_only, location);
                break;

            case TOK_register:
                declaration_specifiers_add_storage(parser, &specifiers,
                        STORAGE_REGISTER, spec_qual_only, location);
                break;

            // Qualifiers
            case TOK_const:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_CONST, location);
                break;

            case TOK_volatile:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_VOLATILE, location);
                break;

            case TOK_restrict:
                declaration_specifiers_add_qualifier(parser, &specifiers,
                        QUALIFIER_RESTRICT, location);
                break;
                
            // Function specifier
            case TOK_inline:
                declaration_specifiers_add_function(parser, &specifiers,
                        FUNCTION_SPECIFIER_INLINE, spec_qual_only, location);
                break;

            // Width specifiers
            case TOK_short:
                declaration_specifiers_add_width(parser, &specifiers,
                        WIDTH_SPECIFIER_SHORT, location);
                break;

            case TOK_long:
                declaration_specifiers_add_width(parser, &specifiers,
                        WIDTH_SPECIFIER_LONG, location);
                break;

            // Sign specifiers
            case TOK_signed:
                declaration_specifiers_add_sign(parser, &specifiers,
                        SIGN_SPECIFIER_SIGNED, location);
                break;

            case TOK_unsigned:
                declaration_specifiers_add_sign(parser, &specifiers,
                        SIGN_SPECIFIER_UNSIGNED, location);
                break;

            // Complex specifiers here
            case TOK__Complex:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        COMPLEX_SPECIFIER_COMPLEX, location);
                break;

            case TOK__Imaginary:
                declaration_specifiers_add_complex(parser, &specifiers, 
                        COMPLEX_SPECIFIER_IMAGINAIRY, location);
                break;

            // normal specifiers are below
            case TOK_void:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_VOID, NULL, location);
                break;

            case TOK_char:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_CHAR, NULL, location);
                break;

            case TOK_int:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_INT, NULL, location);
                break;

            case TOK_float:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_FLOAT, NULL, location);
                break;
            
            case TOK_double:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_DOUBLE, NULL, location);
                break;

            case TOK__Bool:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_BOOL, NULL, location);
                break;

            // All of our tag types below. Note, we continue for them since, if
            // we were to break we would consume the current token, but we have 
            // already dealt with that :)
            case TOK_struct:
            case TOK_union:
                parse_struct_or_union_specifier(parser, &specifiers);
                continue;
                    
            case TOK_enum:
                parse_enum_specificier(parser, &specifiers);
                continue;
            
            // Special case of identifier since we could have a typedef.
            case TOK_IDENTIFIER:
                if (parse_typedef_name(parser, &specifiers))
                {
                    break;
                }

                /* FALLTHROUGH */
            
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

static bool should_eat_semi_after_decl(Parser* parser, Declaration* decl)
{
    // All non-function declarations should have a semi after
    if (!declaration_is(decl, DECLARATION_FUNCTION))
    {
        return true;
    }

    // Otherwise we should only have a semi if we don't have a body
    return !declaration_function_has_body(decl);
}

static Declaration* parse_static_assert_declaration(Parser* parser,
        DeclaratorContext context, Location* trailing_semi)
{
    assert(is_match_two(parser, TOK__Static_assert, TOK_static_assert));

    bool old_token = is_match(parser, TOK__Static_assert);

    Location sa_loc = consume(parser);

    Location lparen_loc = LOCATION_INVALID;
    Expression* ice = NULL;
    Expression* string = NULL;
    Location rparen_loc = LOCATION_INVALID;
    bool c23_sa = false;

    if (!try_match(parser, TOK_LPAREN, &lparen_loc))
    {
        diagnostic_error_at(parser->dm, lparen_loc, "expected '('");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    // Now try to parse an expression
    if (!is_expression_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, lparen_loc, "expected expression");
        recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
        return NULL;
    }

    // Parse the constant expression.
    ice = parse_constant_expression(parser);

    // If we're in c23 mode then we are allowed to have a static assert with
    // no string message. Also allow this is C11 mode as an extension. For both
    // note that there was no string message given.
    if (is_match(parser, TOK_RPAREN))
    {
        if (!lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, current_token_location(parser),
                    "'_Static_assert' with no message is a C23 extension");
        }
        c23_sa = true;
    }
    else
    {
        if (!try_match(parser, TOK_COMMA, &lparen_loc))
        {
            diagnostic_error_at(parser->dm, lparen_loc, "expected ','");
            recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
            return NULL;
        }

        if (!is_string_like_token(parser))
        {
            diagnostic_error_at(parser->dm, current_token_location(parser),
                    "expected string literal for diagnostic message in "
                    "static_assert");
            recover(parser, TOK_SEMI, RECOVER_EAT_TOKEN);
            return NULL;    
        }
        string = parse_string_expression(parser, true);
    }

    bool should_parse_semi = true;
    if (!try_match(parser, TOK_RPAREN, &rparen_loc))
    {
        diagnostic_error_at(parser->dm, rparen_loc, "expected ')'");
        recover(parser, TOK_SEMI, RECOVER_NONE);
        should_parse_semi = false;
    }
        
    // If we had an error on the closing paren then we should not try to parse
    // a semi. If we do we could get cascasing errors.
    if (should_parse_semi && trailing_semi)
    {
        *trailing_semi = parse_trailing_semi(parser,
                old_token ? "'_Static_assert'" : "'static_assert'");
    }

    // Attempt to create our static assert declaration
    return semantic_checker_process_static_assert(&parser->sc, sa_loc,
            lparen_loc, ice, string, rparen_loc, old_token, c23_sa);
}

static DeclarationGroup parse_declaration(Parser* parser,
        DeclaratorContext context, Location* trailing_semi)
{
    // Special case of static assert since they are a very special case of what
    // we can have for a declaration;
    if (is_match_two(parser, TOK__Static_assert, TOK_static_assert))
    {
        Declaration* decl = parse_static_assert_declaration(parser, context,
                trailing_semi);
        return decl_group_from_single(decl);
    }

    // First we need to get our declaration specifiers here
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser,
            false);

    // Check for a possibly empty declaration with a token and pass it to
    // the semantic checker to deal with
    if (is_match(parser, TOK_SEMI))
    {
        if (trailing_semi)
        {
            *trailing_semi = consume(parser);
        }

        // Process the declaration specifiers and get the enclosing decl if any
        // This would mean for example we parsed a struct. We also know that if
        // we have a ';' we MUST be done with the declaration specifiers.
        Declaration* decl_opt = semantic_checker_process_specifiers(&parser->sc,
                &specifiers);
        return decl_group_from_single(decl_opt);
    }
    
    return parse_init_declarator_list(parser, &specifiers, context,
            trailing_semi);
}

static void parse_declaration_or_definition(Parser* parser)
{
    // We can ignore the return value of parse declaration here since all top
    // levels, are added auto-matically to the translation unit at the end. We
    // still need the trailing semi however, as we need to parse it if needed.
    Location trailing_semi = LOCATION_INVALID;
    parse_declaration(parser, DECL_CTX_FILE, &trailing_semi);
}

// The definitions of the functions we will use for parsing. Return false if we
// hit the end of the file. Otherwise, return true. Even if we did not actually
// parse a declaration.
static void parse_top_level(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOK_EOF:
            return;

        case TOK_SEMI:
        {
            diagnostic_warning_at(parser->dm, consume(parser),
                    "extra ';' outside of a function");
            return;
        }

        case TOK_RCURLY:
        {
            diagnostic_error_at(parser->dm, consume(parser),
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
    Scope externals = scope_extern(&parser->ast->ast_allocator);
    semantic_checker_push_externals(&parser->sc, &externals);

    Scope file = scope_file(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &file);

    // Check for case of empty translation unit which is not allowed. Note, this
    // occurs when we try to parse the first top level declaration and instead
    // get an end of file token (no useful input).
    if (is_match(parser, TOK_EOF))
    {
        diagnostic_warning_at(parser->dm, current_token_location(parser),
                "ISO C requires a translation unit to contain at least one "
                "declaration");
    }

    // Otherwise parse the rest of the translation unit until we hit the end of
    // the file.
    while (!is_match(parser, TOK_EOF))
    {
        parse_top_level(parser);
    }

    // Finally, after EOF, we can check all of our external definitions.
    semantic_checker_check_externals(&parser->sc);

    // Ast the last thing we do, set our top level and external declarations so
    // that we are able to keep using these.
    DeclarationList top_level_decls = scope_get_declarations(&file);
    ast_set_top_level_decls(parser->ast, top_level_decls);
    
    DeclarationList external_decls = scope_get_declarations(&externals);
    ast_set_external_decls(parser->ast, external_decls);

    // Here we can pop and delete since all of our needed decl's are in the top
    // level delcaration vector.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&file);

    semantic_checker_pop_externals(&parser->sc);
    scope_delete(&externals);
}

bool parser_create_for_translation_unit(Parser* parser, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids, Ast* ast)
{
    
    parser->dm = dm;
    parser->lang = opts;
    if (!preprocessor_create(&parser->pp, dm, opts, sm, main_file, ids))
    {
        return false;
    }
    parser->token = (Token) {0};
    parser->peek_token = (Token) {0};
    parser->paren_count = 0;
    parser->bracket_count = 0;
    parser->brace_count = 0;
    parser->ast = ast;
    parser->sc = sematic_checker_create(dm, opts, ids, ast);
    
    return true;
}

void parser_delete(Parser* parser)
{
    preprocessor_delete(&parser->pp);
}

void parse_translation_unit(Parser* parser)
{
    // Advance the token initially to ensure that we have something
    preprocessor_advance_token(&parser->pp, &parser->token);

    // Now we can go and parse the translation unit.
    parse_translation_unit_internal(parser);
}
