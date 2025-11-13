#include "parser.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "parse/scope.h"
#include "parse/semantic.h"
#include "util/panic.h"
#include "util/ptr_set.h"
#include "util/str.h"

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
#include "parse/ast_allocator.h"
#include "parse/symbol.h"
#include "parse/ast.h"

#include "driver/diagnostic.h"

#define countof(array) (sizeof(array) / sizeof(array[0]))

typedef enum RecoverFlags {
    RECOVER_NONE = 0, // No flags
    RECOVER_EAT_TOKEN = 1 << 0, // Do we eat the token we want to stop on?
    RECOVER_STOP_AT_SEMI = 1 << 0, // Do we stop at semi even if it's not the
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


// Parser methods for managing error recovory. We are mainly using recover
// tokens for recovery since it is a simple yet powerful method.
static void add_recover_token(Parser* parser, TokenType type);
static void remove_recover_token(Parser* parser, TokenType type);
static void add_recover_tokens(Parser* parser, const TokenType* types,
        size_t count);
static void remove_recover_tokens(Parser* parser, const TokenType* types,
        size_t count);
static bool is_in_recover_set(Parser* parser, TokenType type);

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

static void add_recover_token(Parser* parser, TokenType type)
{
    assert(type && type < TOKEN_LAST);
    assert(parser->recover_set[type] != SIZE_MAX);
    // If above assertion is triggered, then there we basically have infinite
    // memory, so realistically it should never happen...

    parser->recover_set[type]++;
}

static void remove_recover_token(Parser* parser, TokenType type)
{
    assert(type && type < TOKEN_LAST);
    assert(parser->recover_set[type] != 0);

    parser->recover_set[type]--;
}

static void add_recover_tokens(Parser* parser, const TokenType* types, size_t count)
{
    for (size_t i = 0; i < count; i++)
    {
        add_recover_token(parser, types[i]);
    }
}

static void remove_recover_tokens(Parser* parser, const TokenType* types, size_t count)
{
    for (size_t i = 0; i < count; i++)
    {
        remove_recover_token(parser, types[i]);
    }
}

static bool is_in_recover_set(Parser* parser, TokenType type)
{
    return (parser->recover_set[type] != 0);
}

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

static bool try_match(Parser* parser, TokenType type)
{
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
                recover_many(parser, (TokenType[]) {TOKEN_RPAREN}, 1,
                        RECOVER_EAT_TOKEN);
                break;

            case TOKEN_LBRACKET:
                consume(parser);
                recover_many(parser, (TokenType[]) {TOKEN_RBRACKET}, 1, 
                        RECOVER_EAT_TOKEN);
                break;

            case TOKEN_LCURLY:
                consume(parser);
                recover_many(parser, (TokenType[]) {TOKEN_RCURLY}, 1, 
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
    recover_many(parser, (TokenType[]) {type}, 1, flags);
}

static void recover_two(Parser* parser, TokenType type1, TokenType type2,
        RecoverFlags flags)
{
    recover_many(parser, (TokenType[]) {type1, type2}, 2, flags);
}

static void recover_three(Parser* parser, TokenType type1, TokenType type2,
        TokenType type3, RecoverFlags flags)
{
    recover_many(parser, (TokenType[]) {type1, type2, type3}, 3, flags);
}

static bool is_typename_start(Parser* parser, const Token* tok);
static bool is_expression_start(Parser* parser, const Token* tok);
static bool is_statement_start(Parser* parser, const Token* tok);

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_primary_expression(Parser* parser);
static Expression* parse_postfix_expression(Parser* parser);
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
static Statement* parse_statement(Parser* parser);

// All of our functions for parsing declarations / definitions
// TODO: maybe all of these don't need to return a declaration type???
static Initializer* parse_designation(Parser* parser);
static Initializer* parse_designator_list(Parser* parser);
static Initializer* parse_designator(Parser* parser);
static Initializer* parse_initializer(Parser* parser);
static Initializer* parse_initializer_list(Parser* parser);

static void parse_declarator(Parser* parser, Declarator* declarator);

static Declaration* parse_init_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context);
static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context);

static void parse_enumerator_list(Parser* parser, Declaration* enum_decl);
static Declaration* parse_enum_specificer(Parser* parser);
// TODO: condense below into 2 funcs
static void parse_direct_declarator(Parser* parser, Declarator* declarator);
static void parse_direct_abstract_declarator(Parser* parser,
        Declarator* declarator, DeclarationSpecifiers* specifiers);
static void parse_abstract_declarator(Parser* parser, Declarator* declarator,
        DeclarationSpecifiers* specifiers);

static Declaration* parse_declarator_new(Parser* parser, bool maybe_abstract);
static Declaration* parse_direct_declarator_new(Parser* parser, 
        bool maybe_abstract);

static void parse_pointer(Parser* parser, Declarator* declarator);

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

static Declaration* parse_struct_declarator(Parser* parser);
static void parse_struct_declarator_list(Parser* parser,
        DeclarationSpecifiers* specififers);
static Declaration* parse_struct_declaration(Parser* parser);
static void parse_struct_declaration_list(Parser* parser, Declaration* decl);
static Declaration* parse_struct_or_union_specifier(Parser* parser);

static Declaration* parse_declaration(Parser* parser, DeclaratorContext ctx);

static QualifiedType parse_type_name(Parser* parser);

// The definitions of the functions we will use for pasing

// Some functions for creating errors

// TODO: add things for error recovery and whatnot to make parsing better

// TODO: should I add the below into this function? It could be helpful but
// some of the parse will have to be redone. E.g. mostly parsing typenames...
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
            Declaration* decl = scope_lookup_ordinairy(parser->sc.scope,
                     identifier, true);

            return declaration_is(decl, DECLARATION_TYPEDEF);
        }

        default:
            return false;
    }
}

static Type* get_typename(Parser* parser, const Token* tok)
{
    assert(token_is_type(tok, TOKEN_IDENTIFIER));

    Identifier* id = tok->data.identifier;

    Declaration* decl = semantic_checker_lookup_ordinairy(&parser->sc, id,
            true);
    
    assert(declaration_is(decl, DECLARATION_TYPEDEF));

    return decl->tdef.new_type;
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

            // Don't automatically poison this node, since really it is quite
            // recoverable and can still be checked without affecting too much.
            //  Also don't advace the stream since this may lead to some weird
            // and unusual behaviour in subsequent errors.
            Location rparen_loc = LOCATION_INVALID;
            if (!is_match(parser, TOKEN_RPAREN))
            {
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected ')'");
            }
            else
            {
                rparen_loc = consume(parser);
            }

            return expression_create_parenthesised(&parser->ast.ast_allocator,
                    lparen_loc, rparen_loc, expr);
        }
        
        case TOKEN_IDENTIFIER:
        {
            Identifier* identifier = current_token(parser)->data.identifier;
            Location identifer_loc = consume(parser);

            // Attempt to get the declaration that this corrosponds to.
            Declaration* declaration = semantic_checker_lookup_ordinairy(
                    &parser->sc, identifier, true);

            // Check that we either got a declaration, and make sure that it
            // isn't a typedef declaration.
            if (declaration == NULL)
            {
                diagnostic_error_at(parser->dm, identifer_loc,
                        "use of undeclared identifier '%s'",
                        identifier->string.ptr);
                return expression_create_error(&parser->ast.ast_allocator);
            }
            else if (declaration_is(declaration, DECLARATION_TYPEDEF))
            {
                diagnostic_error_at(parser->dm, identifer_loc,
                        "unexpected type name '%s': expected expression",
                        identifier->string.ptr);
                return expression_create_error(&parser->ast.ast_allocator);
            }

            return expression_create_reference(&parser->ast.ast_allocator,
                    identifier, identifer_loc, declaration);
        }

        case TOKEN_NUMBER:
        {
            Token number_tok = *current_token(parser);
            Location loc = consume(parser);

            LiteralValue value = {0};
            bool success = parse_preprocessing_number(&value, parser->dm,
                    &number_tok);
            
            if (!success)
            {
                return expression_create_error(&parser->ast.ast_allocator);
            }
            else
            {
                return expression_create_number(&parser->ast.ast_allocator,
                        loc, value);
            }
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

            if (!success)
            {
                return expression_create_error(&parser->ast.ast_allocator);
            }
            else
            {
                return expression_create_character(&parser->ast.ast_allocator,
                        loc, value);
            }
        }

        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
        {
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

            return NULL;
        }

        default:
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected expression");

            // Do not do error recovery since the function calling this one 
            // could be pretty much anything. So We will just create and error
            // and move on. Since the ast will get poisoned anyways.
            return expression_create_error(&parser->ast.ast_allocator);
    }
}

static Expression* parse_postfix_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

    /* Note: here '(' type-name ')' { ... } is not handled */

    Expression* expr = parse_primary_expression(parser);

    while (has_match(parser, operators, num_operators))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_LBRACKET:
            {
                Location lbracket_loc = consume(parser);
                Expression* member = parse_expression(parser);
                Location rbracket_loc = match(parser, TOKEN_RBRACKET);
                expr = expression_create_array(&parser->ast.ast_allocator,
                        lbracket_loc, rbracket_loc, expr, member);
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
                (void) expr_list;
                Location rparen_loc = match(parser, TOKEN_RPAREN);
                
                break;
            }

            case TOKEN_DOT:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOKEN_IDENTIFIER))
                {
                    diagnostic_error_at(parser->dm,
                            current_token_start_location(parser),
                            "expected identifier after '.'");
                    return expression_create_error(&parser->ast.ast_allocator);
                }

                Identifier* identifier = current_token(parser)->data.identifier;
                Location identifier_loc = consume(parser);

                // Create the member expression.
                break;
            }

            case TOKEN_ARROW:
            {
                Location op_loc = consume(parser);

                if (!is_match(parser, TOKEN_IDENTIFIER))
                {
                    diagnostic_error_at(parser->dm,
                            current_token_start_location(parser),
                            "expected identifier after '->'");
                    recover(parser, TOKEN_SEMI, RECOVER_NONE);
                    return expression_create_error(&parser->ast.ast_allocator);
                }

                Identifier* identifier = current_token(parser)->data.identifier;
                Location identifier_loc = consume(parser);

                // Create the member expression.
                break;
            }

            case TOKEN_PLUS_PLUS:
            {
                Location op_loc = consume(parser);
                expr = expression_create_unary(&parser->ast.ast_allocator, 
                        EXPRESSION_UNARY_POST_INCREMENT, op_loc, expr);
                break;
            }

            case TOKEN_MINUS_MINUS:
            {
                Location op_loc = consume(parser);
                expr = expression_create_unary(&parser->ast.ast_allocator, 
                        EXPRESSION_UNARY_POST_DECREMENT, op_loc, expr);
                break;
            }
        }
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_argument_expression_list(Parser* parser)
{
    // TODO: figure out how we want to represent this...

    Expression* expr = parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        Location comma_loc = consume(parser);
        parse_assignment_expression(parser);
    }

    return NULL;
}

static Expression* parse_unary_expression(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOKEN_PLUS_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_PRE_INCREMENT, op_loc, expr);
        }

        case TOKEN_MINUS_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_unary_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_PRE_DECREMENT, op_loc, expr);
        }

        case TOKEN_AND:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_ADDRESS, op_loc, expr);
        }

        case TOKEN_STAR:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_DEREFERENCE, op_loc, expr);
        }

        case TOKEN_PLUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_PLUS, op_loc, expr);
        }

        case TOKEN_MINUS:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_MINUS, op_loc, expr);
        }

        case TOKEN_TILDE:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
                    EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
        }

        case TOKEN_NOT:
        {
            Location op_loc = consume(parser);
            Expression* expr = parse_cast_expression(parser);
            
            return expression_create_unary(&parser->ast.ast_allocator,
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
                
                Location rparen_loc = LOCATION_INVALID;
                if (!is_match(parser, TOKEN_RPAREN))
                {
                    diagnostic_error_at(parser->dm,
                            current_token_start_location(parser),
                            "expected ')'");
                }
                else
                {
                    rparen_loc = consume(parser);
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
            return parse_postfix_expression(parser);
    }
}

static Expression* parse_cast_expression(Parser* parser)
{
    /* ( type-name ) cast-expression */
    while (is_match(parser, TOKEN_LPAREN) && 
            is_typename_start(parser, next_token(parser)))
    {
        consume(parser);

        QualifiedType type = parse_type_name(parser);
        
        match(parser, TOKEN_RPAREN);

        /* ( type-name ) { initializer-list }
         * ( type-name ) { initializer-list , }
         * 
         * Although this is technically a postfix expression we cannot handle it
         * there is we eat all of the typenames so handle it here.
         */
        if (is_match(parser, TOKEN_LCURLY))
        {
            Location l_curly = consume(parser);
            
            Initializer* initializer = parse_initializer_list(parser);

            Location r_curly = LOCATION_INVALID;
            if (!is_match(parser, TOKEN_RCURLY))
            {
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected '}'");
            }
            else
            {
                r_curly = consume(parser);
            }

            return NULL;
        }
    }

    Expression* expr = parse_unary_expression(parser);

    return expr;
}

static Expression* parse_multiplicative_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_STAR, TOKEN_SLASH, 
            TOKEN_PERCENT};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

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

        expr = expression_create_binary(&parser->ast.ast_allocator, type, 
                op_loc, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_additive_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_PLUS, TOKEN_MINUS};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

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

        expr = expression_create_binary(&parser->ast.ast_allocator, type, 
                op_loc, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_shift_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LT_LT, TOKEN_GT_GT};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

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

        expr = expression_create_binary(&parser->ast.ast_allocator, type,
                op_loc, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_relational_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LT, TOKEN_GT, TOKEN_LT_EQUAL, 
            TOKEN_GT_EQUAL};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

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

        expr = expression_create_binary(&parser->ast.ast_allocator, type,
                op_loc, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_equality_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

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

        expr = expression_create_binary(&parser->ast.ast_allocator, type, 
                op_loc, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_and_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_AND);

    Expression* expr = parse_equality_expression(parser);

    while (is_match(parser, TOKEN_AND))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_equality_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator,
                EXPRESSION_BINARY_AND, op_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_AND);

    return expr;
}

static Expression* parse_exclusive_or_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_XOR);

    Expression* expr = parse_and_expression(parser);

    while (is_match(parser, TOKEN_XOR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_and_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator,
                EXPRESSION_BINARY_XOR, op_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_XOR);

    return expr;
}

static Expression* parse_inclusive_or_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_OR);

    Expression* expr = parse_exclusive_or_expression(parser);

    while (is_match(parser, TOKEN_OR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_exclusive_or_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator,
                EXPRESSION_BINARY_OR, op_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_OR);

    return expr;
}

static Expression* parse_logical_and_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_AND_AND);

    Expression* expr = parse_inclusive_or_expression(parser);

    while (is_match(parser, TOKEN_AND_AND))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_inclusive_or_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator, 
                EXPRESSION_BINARY_LOGICAL_AND, op_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_AND_AND);

    return expr;
}

static Expression* parse_logical_or_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_OR_OR);

    Expression* expr = parse_logical_and_expression(parser);

    while (is_match(parser, TOKEN_OR_OR))
    {
        Location op_loc = consume(parser);
        Expression* rhs = parse_logical_and_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator,
                EXPRESSION_BINARY_LOGICAL_OR, op_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_OR_OR);

    return expr;
}

static Expression* parse_conditional_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_QUESTION);

    Expression* expr = parse_logical_or_expression(parser);

    remove_recover_token(parser, TOKEN_QUESTION);

    if (is_match(parser, TOKEN_QUESTION))
    {
        Location question = consume(parser);

        add_recover_token(parser, TOKEN_COLON);

        Expression* true_expr = parse_expression(parser);

        remove_recover_token(parser, TOKEN_COLON);

        Location colon = match(parser, TOKEN_COLON);

        Expression* false_expr = parse_conditional_expression(parser);

        // TODO: create the conditional expression
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

    add_recover_tokens(parser, operators, num_operators);

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
        expr = expression_create_binary(&parser->ast.ast_allocator,
                type, op_location, expr, rhs);
    }

    remove_recover_tokens(parser, operators, num_operators);

    return expr;
}

static Expression* parse_constant_expression(Parser* parser)
{
    return parse_conditional_expression(parser);
}

static Expression* parse_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_COMMA);

    Expression* expr = parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        Location comma_loc = consume(parser);

        Expression* rhs = parse_assignment_expression(parser);

        expr = expression_create_binary(&parser->ast.ast_allocator,
                EXPRESSION_COMMA, comma_loc, expr, rhs);
    }

    remove_recover_token(parser, TOKEN_COMMA);

    return expr;
}

// For parsing statements

static Statement* parse_expected_statement(Parser* parser, const char* ctx)
{
    if (!is_statement_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected expression after %s", ctx);
        return statement_create_error(&parser->ast.ast_allocator);
    }

    return parse_statement(parser);
}

static Statement* parse_label_statement(Parser* parser)
{
    // Get the identifier from the current token.
    Identifier* identifier = current_token(parser)->data.identifier;
    Location label_loc = consume(parser);
    Location colon_loc = consume(parser);

    // If semantic checker errors, do not continue.
    Declaration* label_decl = semantic_checker_act_on_label(&parser->sc,
            identifier, label_loc);
    if (!label_decl)
    {
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Statement* body = parse_expected_statement(parser, "label");

    return statement_create_label(&parser->ast.ast_allocator, label_loc,
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

    // TODO: will need to eventually fold the constant expression
    IntegerValue value = (IntegerValue) {0};
    /* evaluate_constant_expression(expr);*/

    if (!ast_context_current_switch(&parser->current_context))
    {
        diagnostic_error_at(parser->dm, case_loc,
                "'case' statement not in switch statement");
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Statement* body = parse_expected_statement(parser, "case label");

    return statement_create_case(&parser->ast.ast_allocator, case_loc,
            colon_loc, expr, (IntegerValue) {0}, body,
            ast_context_current_switch(&parser->current_context));
}

static Statement* parse_default_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_DEFAULT));

    // Parse the statement
    Location default_loc = consume(parser);
    Location colon_loc = parse_expected_colon(parser, "default");

    if (!ast_context_current_switch(&parser->current_context))
    {
        diagnostic_error_at(parser->dm, default_loc,
                "'default' statement not in switch statement");
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Statement* stmt = parse_expected_statement(parser, "default label");

    return statement_create_default(&parser->ast.ast_allocator, default_loc,
            colon_loc, stmt,
            ast_context_current_switch(&parser->current_context));
}

static Statement* parse_function_body(Parser* parser)
{
    // Create and push our function scope
    FunctionScope func_scope = function_scope_create();
    sematic_checker_push_function_scope(&parser->sc, &func_scope);

    Statement* stmt = parse_compound_statement(parser);

    // Finish the function by checking all of our labels...
    sematic_checker_act_on_end_of_function(&parser->sc);

    // Delete our function scope
    sematic_checker_pop_function_scope(&parser->sc);
    function_scope_delete(&func_scope);

    return stmt;
}

static Statement* parse_compound_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_LCURLY));

    // Set-up the new scope for declarations
    Scope scope = scope_new_block();
    semantic_checker_push_scope(&parser->sc, &scope);

    Location l_curly = consume(parser);

    StatementVector stmts = statement_vector_create(32);
    while (!is_match(parser, TOKEN_RCURLY) && !is_match(parser, TOKEN_EOF))
    {
        Statement* stmt = parse_statement(parser);
        statement_vector_push(&stmts, stmt);
    }

    Location r_curly = LOCATION_INVALID;
    if (!is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected '}'");
    }
    else
    {
        r_curly = consume(parser);
    }

    // Make sure to pop the scope at the end.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return statement_create_compound(&parser->ast.ast_allocator, l_curly,
            r_curly, &stmts);
}

static Location parse_trailing_semi(Parser* parser, const char* context)
{
    if (!is_match(parser, TOKEN_SEMI))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' after %s", context);
        return LOCATION_INVALID;
    }

    return consume(parser);
}

static Statement* parse_expression_statement(Parser* parser)
{
    assert(is_expression_start(parser, current_token(parser)));

    Expression* expr = parse_expression(parser);
    Location semi_loc = parse_trailing_semi(parser, "expression");
    
    return statement_create_expression(&parser->ast.ast_allocator, semi_loc,
            expr);
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
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Statement* if_body = parse_statement(parser);

    Location else_loc = LOCATION_INVALID;
    Statement* else_body = NULL;
    if (is_match(parser, TOKEN_ELSE))
    {
        else_loc = consume(parser);
        else_body = parse_statement(parser);
    }

    return statement_create_if(&parser->ast.ast_allocator, if_loc, lparen_loc, 
            rparen_loc, else_loc, cond, if_body, else_body);
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
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Statement* switch_stmt = statement_create_switch(&parser->ast.ast_allocator,
            switch_loc, lparen_loc, rparen_loc, expr);

    // Create the switch statement and set the current breakable to be the
    const AstContext old_ctx = ast_context_push_switch(&parser->current_context, 
            switch_stmt);

    Statement* body = parse_statement(parser);

    statement_switch_set_body(switch_stmt, body);

    // Restore the ast context
    ast_context_pop(&parser->current_context, old_ctx);

    return switch_stmt;
}

static Statement* parse_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_WHILE));

    Location while_loc = consume(parser);

    Location lparen_loc;
    Expression* cond;
    Location rparen_loc;
    if (!parse_expression_for_statement(parser, &lparen_loc, &cond, &rparen_loc,
            "while"))
    {
        return statement_create_error(&parser->ast.ast_allocator);
    }

    // Create the statement and push the context
    Statement* while_stmt = statement_create_while(&parser->ast.ast_allocator,
            while_loc, lparen_loc, rparen_loc, cond);

    const AstContext old_ctx = ast_context_push_while(&parser->current_context, 
            while_stmt);

    // Parse the body and set the inner while stmt
    Statement* body = parse_statement(parser);

    statement_while_set_body(while_stmt, body);

    // Restore the ast context
    ast_context_pop(&parser->current_context, old_ctx);

    return while_stmt;
}

static Statement* parse_do_while_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_DO));

    Location do_loc = consume(parser);

    // Create statement and push context
    Statement* do_stmt = statement_create_do_while(&parser->ast.ast_allocator,
            do_loc);

    const AstContext old_ctx = ast_context_push_do_while(
            &parser->current_context, do_stmt);

    Statement* body = parse_statement(parser);

     // Restore the ast context
    ast_context_pop(&parser->current_context, old_ctx);
    
    // If the token is not a while just skip till we get a semi...
    if (!is_match(parser, TOKEN_WHILE))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected 'while' in do/while loop");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return statement_create_error(&parser->ast.ast_allocator);
    }

    // make sure we capture the while part.
    Location while_loc = consume(parser);

    Location lparen_loc;
    Expression* cond;
    Location rparen_loc;
    if (!parse_expression_for_statement(parser, &lparen_loc, &cond, &rparen_loc,
            "do/while"))
    {
        return statement_create_error(&parser->ast.ast_allocator);
    }

    Location semi_loc = parse_trailing_semi(parser, "do/while statement");

    statement_do_while_set_body(do_stmt, while_loc, lparen_loc, rparen_loc, 
            cond, body);

    return do_stmt;
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

        return statement_create_error(&parser->ast.ast_allocator);
    }

    Location lparen_loc = consume(parser);

    // Below is where it gets a little tricky :)
    Statement* init = NULL;
    // TODO: i think below should be has declaration specifiers...
    if (is_typename_start(parser, current_token(parser)))
    {
        init = parse_declaration_statement(parser);
    }
    else if (is_expression_start(parser, current_token(parser)))
    {
        init = parse_expression_statement(parser);
    }
    else if (is_match(parser, TOKEN_SEMI))
    {
        init = parse_empty_statement(parser);
    }
    else
    {
        // TODO: ensure this is adequete error recovery also change the message
        // to possibly 'expected expression'
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "bad initialisation in for statement");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        init = statement_create_error(&parser->ast.ast_allocator);
    }

    Expression* cond = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        cond = parse_expression(parser);
    }

    // TODO: do we need this semis location?
    match(parser, TOKEN_SEMI);

    Expression* inc = NULL;
    if (!is_match(parser, TOKEN_RPAREN))
    {
        inc = parse_expression(parser);
    }

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    // Create the for statement and then parse and set the body
    Statement* for_stmt = statement_create_for(&parser->ast.ast_allocator,
            for_loc, lparen_loc, rparen_loc, init, cond, inc);

    // Push the for stmt into the current context
    const AstContext old_ctx = ast_context_push_for(&parser->current_context, 
            for_stmt);
    
    // TODO: change parse statement to allow / not allow declarations?
    Statement* body = parse_statement(parser);
    if (body != NULL && body->base.type == STATEMENT_DECLARATION) {
        diagnostic_error_at(parser->dm,
                body->declaration_stmt.declaration->base.location,
                "declaration not allowed here; create a compound "
                "statement {...}");
    }

    statement_for_set_body(for_stmt, body);

    // Restore the current context
    ast_context_pop(&parser->current_context, old_ctx);

    return for_stmt;
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

        return statement_create_error(&parser->ast.ast_allocator);
    }

    // Get the identifier name and consume the identifier.
    Identifier* label_name = current_token(parser)->data.identifier;
    Location label_location = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "goto statement");
    
    // Get the label from the semantic checker. This should never be null as
    // we will implicitly create a label if it does not already exist.
    // TODO: we can also use this call to help diagnose unused labels too!
    Declaration* label = semantic_checker_act_on_goto(&parser->sc, label_name,
            label_location);

    return statement_create_goto(&parser->ast.ast_allocator, goto_loc,
            semi_loc, label);
}

static Statement* parse_continue_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_SEMI));

    Location continue_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "continue statement");

    if (!ast_context_current_iterable(&parser->current_context))
    {
        diagnostic_error_at(parser->dm, continue_loc, 
                "'continue' statement not in loop statement");
        return statement_create_error(&parser->ast.ast_allocator);
    }

    return statement_create_contine(&parser->ast.ast_allocator, continue_loc, 
            semi_loc, ast_context_current_iterable(&parser->current_context));
}

static Statement* parse_break_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_BREAK));

    Location break_loc = consume(parser);
    Location semi_loc = parse_trailing_semi(parser, "break statement");

    if (!ast_context_current_breakable(&parser->current_context))
    {
        diagnostic_error_at(parser->dm, break_loc,
                "'break' statement not in loop or switch statement");
        return statement_create_error(&parser->ast.ast_allocator);
    }

    return statement_create_break(&parser->ast.ast_allocator, break_loc, 
            semi_loc, ast_context_current_breakable(&parser->current_context));
}

static Statement* parser_return_statement(Parser* parser)
{
    assert(is_match(parser, TOKEN_RETURN));

    Location return_loc = consume(parser);

    Expression* expr_opt = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        expr_opt = parse_expression(parser);
    }
    Location semi_loc = parse_trailing_semi(parser, "return statement");

    return statement_create_return(&parser->ast.ast_allocator, return_loc,
            semi_loc, expr_opt);
}

static Statement* parse_declaration_statement(Parser* parser)
{
    // Choose block since we are known to be in one and can parse declarations
    // inside of it.
    Declaration* decl = parse_declaration(parser, DECLARATION_CONTEXT_BLOCK);
    Location semi_loc = parse_trailing_semi(parser, "declaration");

    return statement_create_declaration(&parser->ast.ast_allocator, semi_loc,
            decl);
}

static Statement* parse_empty_statement(Parser* parser)
{
    Location semi_loc = consume(parser);

    return statement_create_empty(&parser->ast.ast_allocator, semi_loc);
}

static Statement* parse_error_statement(Parser* parser)
{
    recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

    return statement_create_error(&parser->ast.ast_allocator);
}

static Statement* parse_statement(Parser* parser)
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
            return parser_return_statement(parser);

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
                return parse_declaration_statement(parser);
            }
            else if (is_expression_start(parser, current_token(parser)))
            {
                return parse_expression_statement(parser);
            }
            else
            {
                // We got a really bad token here. Eat until a semi
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected expression");
                return parse_error_statement(parser);
            }
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

static void parse_declarator(Parser* parser, Declarator* declarator)
{
    // If we get a pointer before our declarator do this first.
    if (is_match(parser, TOKEN_STAR))
    {
        Location star_location = consume(parser);
        TypeQualifiers qualifiers = parse_type_qualifier_list_opt(parser);

        // Hold off on pushing the pointer declarator. So that the correct
        // precedence of the declarators can be achieved. Instead we will just
        // recurse so that we can parse other declarators first.
        parse_declarator(parser, declarator);

        declarator_push_pointer(declarator, qualifiers);

        return;
    }
    
    parse_direct_declarator(parser, declarator);
}

static Declaration* parse_init_declarator(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context)
{
    Declarator declarator = declarator_create(specifiers, context);
    
    parse_declarator(parser, &declarator);

    // Then parse the initializer if there is one
    Initializer* initializer = NULL;
    Location equal_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_EQUAL))
    {
        equal_loc = consume(parser);
        initializer = parse_initializer(parser);
    }

    // Now create the declaration
    Declaration* decl = semantic_checker_process_declarator(&parser->sc, 
            &declarator, equal_loc, initializer);

    // TODO: we will need to add the declaration into the symbol table before
    // TODO: we delete the declarator, and before we parse the initializer since
    // TODO: self-initialisation is allowed (but stupid).

    declarator_delete(&declarator);

    return NULL;
}

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers, DeclaratorContext context)
{
    // TODO: make a declaration vector and turn it into a list of declarations
    // TODO: if needed...

    Declaration* decl = NULL;
    do
    {
        decl = parse_init_declarator(parser, specifiers, context);
    }
    while (try_match(parser, TOKEN_COMMA));
 
    return decl;
}

static void parse_identifier_list(Parser* parser, Declarator* declarator)
{
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

        Identifier* identifier = current_token(parser)->data.identifier;
        Location identifier_loc = consume(parser);

        // If we get a typename, diagnose and continue.
        if (/*is_typename(current_token(parser))*/0)
        {
            diagnostic_error_at(parser->dm, identifier_loc,
                    "unexpected type name '%s', expected identifier",
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
    while (try_match(parser, TOKEN_COMMA));

    pointer_set_delete(&identifier_set);

    // TODO: will need to use this identifier vector to create an array of
    // declarators that we can later fill when we get the function definition.
    identifier_vector_free(&vec, NULL);
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    // TODO: Will eventually need to allow for abstract declarations

    // Parse declaration specifiers
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);

    // Parse the function paramater declarator
    Declarator declarator = declarator_create(&specifiers,
            DECLARATION_CONTEXT_FUNCTION_PARAM);
    parse_declarator(parser, &declarator);

    // Finish processing the declaration
    Declaration* declaration = semantic_checker_process_function_param(
            &parser->sc, &declarator);

    // Free the declarator
    declarator_delete(&declarator);

    return declaration;
}

static void parse_paramater_type_list(Parser* parser, Declarator* declarator)
{
    Scope function_proto = scope_new_function_prototype();
    semantic_checker_push_scope(&parser->sc, &function_proto);

    DeclarationVector params = declaration_vector_create(1);

    Location dots = LOCATION_INVALID;
    bool is_variadic = false;
    do
    {
        // First check if we have elipsis
        if (is_match(parser, TOKEN_ELIPSIS))
        {
            consume(parser);

            // Check that we have a parameter. If not error, but we will try to
            // recover and ignore the error to be a bit better
            if (declaration_vector_size(&params) == 0)
            {
                diagnostic_error_at(parser->dm, dots,
                        "parameter required before '...'");
            }
            
            is_variadic = true;
            break;
        }

        // Otherwise we will try to parse a declaration.
        Declaration* declaration = parse_paramater_declaration(parser);

        // TODO: instead of this I think we should use a scope instead to check
        // TODO: if we got a parameter redefinition, especially since we will
        // TODO: have to use the scope later anyways.

        // check if we got a duplicate paramater name.
        if (declaration_has_identifier(declaration))
        {
            Declaration* prev = scope_lookup_ordinairy(&function_proto,
                    declaration->base.identifier, false);

            if (prev != NULL)
            {
                diagnostic_error_at(parser->dm, declaration->base.location,
                        "redefinition of parameter '%s'",
                        declaration->base.identifier->string.ptr);
                continue;
            }
        }

        // TODO: how do we deal with parameters with no names? As right now
        // TODO: the pointer set will not accept the identifier since it will
        // TODO: be NULL
        scope_insert_ordinairy(&function_proto, declaration);
        declaration_vector_push(&params, declaration);
    }
    while (try_match(parser, TOKEN_COMMA));

    // Push the function type onto the end of the declarator
    declarator_push_function(declarator, &parser->ast.ast_allocator, &params,
            is_variadic);

    // Cleaup our declarations and our function scopes
    declaration_vector_free(&params, NULL);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&function_proto);
}

static void parse_function_declarator(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LPAREN));

    Location lparen_loc = consume(parser);

    if (!is_match(parser, TOKEN_RPAREN))
    {
        if (is_match(parser, TOKEN_IDENTIFIER) &&
                !is_typename_start(parser, current_token(parser)))
        {
            parse_identifier_list(parser, declarator);
        }
        else
        {
            parse_paramater_type_list(parser, declarator);
        }
    }

    if (!is_match(parser, TOKEN_RPAREN))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ')'");
        recover(parser, TOKEN_RPAREN, RECOVER_NONE);
    }

    Location rparen_loc = LOCATION_INVALID;
    if (is_match(parser, TOKEN_RPAREN))
    {
        rparen_loc = consume(parser);
    }

    // TODO: create our function declarator here.
}

static void parse_array_declarator(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LBRACKET));

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

    // We should never get the static keyword twice...
    if (!is_static && is_match(parser, TOKEN_STATIC))
    {
        consume(parser);

        is_static = true;
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

    if (!is_match(parser, TOKEN_RBRACKET))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ']'");
        recover(parser, TOKEN_RBRACKET, RECOVER_STOP_AT_SEMI);
        return;
    }

    Location rbracket_loc = consume(parser);

    declarator_push_array(declarator, lbracket_loc, rbracket_loc, qualifiers,
            expression, is_static, is_star);
}

static void parse_direct_declarator(Parser* parser, Declarator* declarator)
{
    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        Identifier* identifier = current_token(parser)->data.identifier;
        Location loc = consume(parser);
        
        declarator_set_identifier(declarator, identifier, loc);
    }
    else if (is_match(parser, TOKEN_LPAREN))
    {
        Location lparen_loc = consume(parser);

        parse_declarator(parser, declarator);
        
        if (!is_match(parser, TOKEN_RPAREN))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected ')'");
            recover(parser, TOKEN_RPAREN, RECOVER_STOP_AT_SEMI);
            return;
        }

        Location rparen_loc = consume(parser);
    }
    else
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected identifier or '('");
        
        // Make sure we never attempt to use it
        declarator_set_invalid(declarator);

        // GCC and clang both seem to recover on the semi based on diagnostics
        // alone but this is what I am going to try...
        recover(parser, TOKEN_COMMA, RECOVER_STOP_AT_SEMI);
        return;
    }

    // Try to match our declarators
    while (true)
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            parse_function_declarator(parser, declarator);
        }
        else if (is_match(parser, TOKEN_LBRACKET))
        {
            parse_array_declarator(parser, declarator);
        }
        else
        {
            break;;
        }
    }
}

static void parse_direct_abstract_declarator(Parser* parser,
        Declarator* declarator, DeclarationSpecifiers* specifiers)
{
    // if we get '(' abstract-declarator ')' note that abstract declarator can
    // only start with a star, l-paren or l-bracket
    if (is_match(parser, TOKEN_LPAREN) && (is_next_match(parser, TOKEN_STAR)
            || is_next_match(parser, TOKEN_LPAREN) 
            || is_next_match(parser, TOKEN_LBRACKET)))
    {
        Location lparen_loc = consume(parser);
        parse_abstract_declarator(parser, declarator, specifiers);
        Location rparen_loc = match(parser, TOKEN_RPAREN);
    }

    while (is_match(parser, TOKEN_LPAREN) || is_match(parser, TOKEN_LBRACKET))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            consume(parser);
            parse_paramater_type_list(parser, declarator);
            match(parser, TOKEN_RPAREN);        
        }
        else if (is_match(parser, TOKEN_LBRACKET))
        {
            Location left_bracket = consume(parser);

            if (is_match(parser, TOKEN_STAR) &&
                    is_next_match(parser, TOKEN_RBRACKET))
            {
                Location star_location = consume(parser);
            }
            else if (is_expression_start(parser, current_token(parser)))
            {
                Expression* expr = parse_assignment_expression(parser);
            }
            else if (!is_match(parser, TOKEN_RBRACKET))
            {
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "exprected expression or ']'");
            }

            Location right_bracket = match(parser, TOKEN_RBRACKET);
        }
    }
}

static void parse_abstract_declarator(Parser* parser, Declarator* declarator, 
        DeclarationSpecifiers* specifiers)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser, declarator);
    }
    
    parse_direct_abstract_declarator(parser, declarator, specifiers);
}

// TODO: should I eliminate the recursion here?
// TODO: should I even keep this function around???
static void parse_pointer(Parser* parser, Declarator* declarator)
{
    Location star_location = match(parser, TOKEN_STAR);

    TypeQualifiers qualifiers = parse_type_qualifier_list_opt(parser);

    declarator_push_pointer(declarator, qualifiers);
    
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser, declarator);
    }
}

static TypeQualifiers parse_type_qualifier_list(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    // Set up a declaration spec with no type qualifiers initially
    DeclarationSpecifiers qualifiers = { .qualifiers = TYPE_QUALIFIER_NONE };
    do
    {
        TypeQualifiers qualifier = TYPE_QUALIFIER_NONE; // Quiet clang
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

static Declaration* parse_struct_declarator(Parser* parser)
{   
    // Do we have a bitfield by itself?
    if (is_match(parser, TOKEN_COLON))
    {
        consume(parser);
        parse_constant_expression(parser);

        return NULL;
    }

    // TODO: will need to change this function and actuall pass in the declspec
    DeclarationSpecifiers specifiers = {0};
    
    // Otherwise, we want to create and parse our declarator. 
    Declarator declarator = declarator_create(&specifiers,
            DECLARATION_CONTEXT_STRUCT);

    parse_declarator(parser, &declarator);

    // Then let's check if we have a bitfield after wards.
    Location colon_loc = LOCATION_INVALID;
    Expression* expr = NULL;
    if (is_match(parser, TOKEN_COLON))
    {
        colon_loc = consume(parser);
        expr = parse_constant_expression(parser);
    }

    // TODO: add in our struct declarator somehow below we create it all

    declarator_delete(&declarator);

    return NULL;
}

static void parse_struct_declarator_list(Parser* parser,
        DeclarationSpecifiers* specififers)
{
    do
    {
        Declaration* decl = parse_struct_declarator(parser);
    }
    while (try_match(parser, TOKEN_COMMA));
}

static Declaration* parse_struct_declaration(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);

    if (is_match(parser, TOKEN_SEMI))
    {
        Location loc = consume(parser);
        diagnostic_warning_at(parser->dm, loc,
                "declaration does not declare anything");
        return NULL;
    }
    
    parse_struct_declarator_list(parser, &specifiers);

    if (!is_match(parser, TOKEN_SEMI))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' after struct declaration");
        
        // We could be at end of struct definition!
        if (!is_match(parser, TOKEN_RCURLY))
        {
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
        }

        return NULL;
    }

    assert(is_match(parser, TOKEN_SEMI));

    // Consume the ';'
    consume(parser);

    return NULL;
}

static void parse_struct_declaration_list(Parser* parser, Declaration* decl)
{
    assert(is_match(parser, TOKEN_LCURLY));
    
    Location l_curly = consume(parser);

    if (is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "use of empty struct where it is not supported");
        consume(parser);
        return;
    }

    Scope members = scope_new_member();
    semantic_checker_push_scope(&parser->sc, &members);

    while (!is_match(parser, TOKEN_EOF))
    {
        if (!is_typename_start(parser, current_token(parser)))
        {
            Location loc = current_token_start_location(parser);
            diagnostic_error_at(parser->dm, loc,
                    "expected type specifier or qualifier");
            recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);
            continue;
        }

        // Otherwise we can parse a struct declaration.
        parse_struct_declaration(parser);

        // Check if we are done parsing a struct
        if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }
    }

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&members);

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

    parse_struct_declaration_list(parser, declaration);
    
    return NULL;
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
                "cannot have an empty enum");
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
        Declaration* constant_decl = semantic_checker_create_enum_constant(
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
                // TODO: handle the struct declaration...
                break;
            }

            case TOKEN_UNION:
            {
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_UNION);
                Declaration* union_decl = 
                        parse_struct_or_union_specifier(parser);
                // TODO: handle the union declaration...
                break;
            }
            
            case TOKEN_ENUM:
            {
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_ENUM);
                Declaration* enum_decl = parse_enum_specificier(parser);
                specifiers.declaration = enum_decl;
                break;
            }
            
            // Special case of identifier since we could have a typedef.
            case TOKEN_IDENTIFIER:
            {
                // If we have not had any information about the type yet then we
                // should check if we have a typedef.
                if (specifiers.type_spec_sign == TYPE_SPECIFIER_SIGN_NONE &&
                    specifiers.type_spec_width == TYPE_SPECIFIER_WIDTH_NONE &&
                    specifiers.type_spec_complex == TYPE_SPECIFIER_COMPLEX_NONE
                    && specifiers.type_spec_type == TYPE_SPECIFIER_TYPE_NONE &&
                        is_typename_start(parser, current_token(parser)))
                {
                    assert(specifiers.type == NULL);
                    specifiers.type = get_typename(parser,
                            current_token(parser));
                    
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

    if (is_match(parser, TOKEN_SEMI))
    {
        return NULL;
    }

    // Check for a valid declaration returning early if we don't get anything
    // if (is_match(parser, TOKEN_SEMI))
    // {
    //     // TODO: this is incorrect. Think about the case below:
    //     // 
    //     // struct s;
    //     //
    //     // this does declare something, but it a bit harder to see that it
    //     // actually does.
    //     diagnostic_warning_at(parser->dm, specifiers.location,
    //             "declaration does not declare anything");
    //     return NULL;
    // }

    return parse_init_declarator_list(parser, &specifiers, context);
}

static QualifiedType parse_type_name(Parser* parser)
{
    const TokenType operators[] = {TOKEN_STAR, TOKEN_LPAREN, TOKEN_LBRACKET};
    const size_t num_operators = countof(operators);

    // TODO: this should be parse specifier qualifier list... but idk...
    DeclarationSpecifiers specifiers = parse_specifier_qualifier_list(parser);
    Declarator declarator = declarator_create(&specifiers,
            DECLARATION_CONTEXT_TYPE_NAME);

    if (has_match(parser, operators, num_operators))
    {
        parse_abstract_declarator(parser, &declarator, &specifiers);
    }

    // QualifiedType type = semantic_checker_process_declarator(&parser->sc,
    //         &declarator);

    declarator_delete(&declarator);

    return (QualifiedType) {0};
}

// The definitions of the functions we will use for pasing

static Declaration* parse_top_level(Parser* parser)
{
    // Check that we are okay to start a typename bailing out if this is invalid
    if (!is_typename_start(parser, current_token(parser)))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected top level declaration or definition");
        recover(parser, TOKEN_SEMI, RECOVER_EAT_TOKEN);

        return declaration_create_error(&parser->ast.ast_allocator,
                current_token_start_location(parser));
    }

    // Here we actually do the work!
    Declaration* decl = parse_declaration(parser, DECLARATION_CONTEXT_FILE);

    // TODO: we will need to check for redeclarations

    if (/*declaration_is(decl, DECLARATION_FUNCTION) && */
            is_match(parser, TOKEN_LCURLY))
    {
        // If it already have a body error but still parse the function body
        bool set_body = true;
        if (/*declaration_function_has_body(decl)*/0)
        {
            set_body = false;

            diagnostic_error_at(parser->dm, decl->base.location,
                    "redefinition of '%s'", decl->base.identifier->string.ptr);
        }

        // Parse the body and set if approperiate
        Statement* body = parse_function_body(parser);

        if (/*set_body*/0)
        {
            declaration_function_set_body(decl, body);
        }
    }
    else
    {
        parse_trailing_semi(parser, "top level declaration");
    }

    // TODO: need to check redeclaration of symbol

    return decl;
}

static void parse_translation_unit_internal(Parser* parser)
{
    add_recover_token(parser, TOKEN_EOF);

    // Create our file scope which will be used throughout parsing the t-unit
    Scope file = scope_new_file();
    semantic_checker_push_scope(&parser->sc, &file);

    while (current_token_type(parser) != TOKEN_EOF)
    {
        Declaration* decl = parse_top_level(parser);

        // Can be null if we just got a type name followed by a ;
        if (decl == NULL)
        {
            continue;
        }

        declaration_vector_push(&parser->ast.top_level_decls, decl);
    }

    // Here we can pop and delete since all of our needed decl's are in the top
    // level delcaration vector.
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&file);

    remove_recover_token(parser, TOKEN_EOF);
}

void parse_translation_unit(DiagnosticManager* dm, Preprocessor* pp)
{
    Parser parser;
    parser.dm = dm;
    parser.pp = pp;
    preprocessor_advance_token(pp, &parser.token);
    memset(parser.recover_set, 0, sizeof(parser.recover_set));
    parser.ast = ast_create();
    parser.current_context = (AstContext) {0};
    parser.sc = sematic_checker_create(dm, &pp->identifiers, &parser.ast);
    parser.paren_count = 0;
    parser.bracket_count = 0;
    parser.brace_count = 0;

    parse_translation_unit_internal(&parser);

    ast_delete(&parser.ast);
}
