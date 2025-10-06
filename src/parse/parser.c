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

    diagnostic_error_at(parser->dm, parser->token.loc,
            "expected '%s' but got '%s'", token_type_get_name(type), 
            token_type_get_name(current_token_type(parser)));

    return LOCATION_INVALID;
}

static Location consume(Parser* parser)
{
    Location location = current_token_start_location(parser);

    preprocessor_advance_token(parser->pp, &parser->token);

    return location;
}

static bool try_match(Parser* parser, TokenType type)
{
    if (current_token_type(parser) == type)
    {
        consume(parser);

        return true;
    }

    return false;
}

static bool is_match(Parser* parser, TokenType type)
{
    return current_token_type(parser) == type;
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

static void eat_until(Parser* parser, TokenType type)
{
    assert(!is_match(parser, type));

    while (!is_match(parser, type) && !is_match(parser, TOKEN_EOF))
    {
        consume(parser);
    }
}

static void eat_until_v(Parser* parser, const TokenType *types, size_t count)
{
    while (!has_match(parser, types, count))
    {
        consume(parser);
    }
}

static void eat_until_and(Parser* parser, TokenType type)
{
    assert(!is_match(parser, type));

    while (!is_match(parser, type) && !is_match(parser, TOKEN_EOF))
    {
        consume(parser);
    }

    if (is_match(parser, type))
    {
        consume(parser);
    }
}

static void eat_until_recover(Parser* parser)
{
    while (true)
    {
        TokenType type = current_token_type(parser);

        if (is_in_recover_set(parser, type))
        {
            break;
        }

        consume(parser);
    }
}

// A parsing method to synchronise the stream of tokens in the event of an error
// will go until we hit the a token in the recovery set. This is using panic mode
// recovery and this should work pretty well in c...
static void recover(Parser* parser, TokenType type)
{
    eat_until(parser, type);

    if (!is_match(parser, TOKEN_EOF))
    {
        consume(parser);
    }
}

static bool is_typename_start(Parser* parser, const Token* tok);
static bool is_expression_start(Parser* parser, const Token* tok);
static bool is_statement_start(Parser* parser, const Token* tok);

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_integer_constant(Parser* parser);
static Expression* parse_floating_constant(Parser* parser);
static Expression* parse_character_constant(Parser* parser);

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
        DeclarationSpecifiers* specifiers);
static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers);

static Declaration* parse_typedef_name(Parser* parser);
static Declaration* parse_enumerator_list(Parser* parser);
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

static bool has_declaration_specifier(Parser* parser, const Token* tok);
static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser);

static TypeQualifiers parse_type_qualifier_list(Parser* parser);
static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser);

static Declaration* parse_struct_declarator(Parser* parser);
static Declaration* parse_struct_declarator_list(Parser* parser);
static Declaration* parse_struct_declaration(Parser* parser);
static Declaration* parse_struct_declaration_list(Parser* parser);
static Declaration* parse_struct_or_union_specifier(Parser* parser);

static Declaration* parse_declaration(Parser* parser);

static Declaration* parse_type_name(Parser* parser);

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
        case TOKEN_EXTERN:
        case TOKEN_STATIC:
        case TOKEN_TYPEDEF:
        case TOKEN_INLINE:
        case TOKEN_CONST:
        case TOKEN_VOLATILE:
        case TOKEN_REGISTER:
        case TOKEN_AUTO:
            return true;

        case TOKEN_IDENTIFIER:
        {
            Identifier* identifier = tok->data.identifier;
            // if (!symbol_table_contains(NULL /*table=current*/, identifier))
            // {
            //     return false;
            // }

            // Declaration* declaration = symbol_table_lookup(NULL
            //         /*table=current*/, identifier);
            // if (declaration->base.declaration_type == DECLARATION_TYPEDEF)
            // {
            //     return true;
            // }
        
            return false;
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
            // TODO: check for typedefs here


            return true;
        
        default:
            return false;
    }
}

static bool is_statement_start(Parser* parser, const Token* tok)
{
    panic("TODO");

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
    Token* current = current_token(parser);
    switch (current_token_type(parser))
    {
        case TOKEN_LPAREN:
        {
            Location lparen_loc = consume(parser);
            Expression* expr = parse_expression(parser);
            Location rparen_loc = match(parser, TOKEN_RPAREN);

            return expression_create_parenthesised(&parser->ast.ast_allocator,
                    lparen_loc, rparen_loc, expr);
        }
        
        case TOKEN_IDENTIFIER:
        {
            Identifier* identifier = current->data.identifier;
            Location location = consume(parser);

            // TODO: when parsing an initializer, we should add the symbol to
            // TODO: the symbol table before looking for it.
            if (!symbol_table_lookup(&parser->symbols, identifier))
            {
                diagnostic_error_at(parser->dm, location,
                        "unknown identifier '%s'", identifier->string.ptr);
            }

            // TODO: here we will have to check if the identifier is a typename
            // TODO: will also have to check if the identifier is in the symbol
            // TODO: table at all

            return NULL;
        }

        case TOKEN_NUMBER:
        {
            IntegerValue value = {0};
            const bool success = parse_integer_literal(&value, current);

            Location loc = consume(parser);

            Expression* expr;
            if (!success)
            {
                diagnostic_error_at(parser->dm, loc,
                        "integer literal conversion failed");

                // expr = create_error_expression(current);
            }
            else
            {
                // expr = create_integer_expression(current, &value);
            }

            return NULL;
        }

        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
        {
            Location start_loc = current_token_start_location(parser);

            size_t string_count = 0;
            while (is_string_token(parser, current_token(parser)))
            {
                consume(parser);
                string_count++;
            }

            diagnostic_error_at(parser->dm, start_loc,
                    "string concatenation and conversion not implented");

            panic("above");

            return NULL;
        }
        
        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
        {
            CharValue value = {0};
            const bool success = parse_char_literal(&value, current);

            Location loc = consume(parser);

            Expression* expr;
            if (!success)
            {
                diagnostic_error_at(parser->dm, loc,
                        "character conversion failed");

                // expr = create_error_expression(current);
            }
            else
            {
                // expr = create_character_expression(current, &value);
            }

            return NULL;
        }

        default:
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected expression");

            return expression_create_error(&parser->ast.ast_allocator);
    }
}

static Expression* parse_postfix_expression(Parser* parser)
{
    static const TokenType operators[] = {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS};
    static const size_t num_operators = countof(operators);

    add_recover_tokens(parser, operators, num_operators);

    /* Note: here '(' type-name ')' { ...} is not handled */

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
                match(parser, TOKEN_IDENTIFIER);
                break;
            }

            case TOKEN_ARROW:
            {
                Location op_loc = consume(parser);
                match(parser, TOKEN_IDENTIFIER);
                break;
            }

            case TOKEN_PLUS_PLUS:
            {
                Location op_loc = consume(parser);
                expr = expression_create_unary(&parser->ast.ast_allocator, 
                        EXPRESSION_UNARY_PRE_INCREMENT, op_loc, expr);
                break;
            }

            case TOKEN_MINUS_MINUS:
            {
                Location op_loc = consume(parser);
                expr = expression_create_unary(&parser->ast.ast_allocator, 
                        EXPRESSION_UNARY_PRE_DECREMENT, op_loc, expr);
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
                    EXPRESSION_UNARY_BIT_NOT, op_loc, expr);
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

                Declaration* decl = parse_type_name(parser);

                if (!is_match(parser, TOKEN_RPAREN))
                {
                    diagnostic_error_at(parser->dm,
                            current_token_start_location(parser),
                            "expected ')'");
                }
                else
                {
                    consume(parser);
                }

                return NULL;
            }
            else
            {
                return parse_unary_expression(parser);
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

        Declaration* type = parse_type_name(parser);
        
        match(parser, TOKEN_RPAREN);

        /* ( type-name ) { initializer-list }
         * ( type-name ) { initializer-list , }
         * 
         * Although this is technically a postfix expression we cannot handle it
         * there is we eat all of the typenames so handle it here.
         */
        if (is_match(parser, TOKEN_LCURLY))
        {
            consume(parser);
            
            parse_initializer_list(parser);

            if (is_match(parser, TOKEN_COMMA))
            {
                consume(parser);
            }

            match(parser, TOKEN_RCURLY);

            return NULL;
        }
    }

    Expression* expr = parse_unary_expression(parser);

    return NULL;
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

            case TOKEN_LT_LT_EQUAL: 
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

static Statement* parse_label_statement(Parser* parser)
{
    // Get the identifier from the current token.
    Identifier* identifier = current_token(parser)->data.identifier;
    Location label_loc = consume(parser);
    Location colon_loc = consume(parser);

    // TODO: this is not correct as if the label is used implicitly this won't
    // TODO: work as intended!!
    if (symbol_table_lookup(&parser->symbols, identifier))
    {
        diagnostic_error_at(parser->dm, label_loc,
                "label '%s' already exisists", identifier->string.ptr);

        return statement_create_error(&parser->ast.ast_allocator);
    }

    // Create and insert the label into the symbol table so that we can referece
    // it for later.
    Declaration* label_decl = declaration_create_label(
            &parser->ast.ast_allocator, identifier, label_loc, false);
    symbol_table_insert(&parser->symbols, label_decl);

    // TODO: need to fix parsing here since labels should have a statement after
    // them. So will need some kind of statement start function and then have
    // errors / warnings after
    // Statement* body = parse_statement(parser);

    return statement_create_label(&parser->ast.ast_allocator, label_loc,
            colon_loc, label_decl, NULL);
}

static Statement* parse_case_statement(Parser* parser)
{    
    Location case_loc = consume(parser);

    Statement* current_switch = ast_context_current_switch(&parser->current_context);
    if (!current_switch)
    {
        diagnostic_error_at(parser->dm, case_loc,
                "case statement not in switch statement");
    }
    
    // Get the experssion and the statment
    Expression* expr = parse_constant_expression(parser);
    
    // Get the colon and the rest of the statment
    Location colon_loc = match(parser, TOKEN_COLON);

    Statement* body = parse_statement(parser);

    return statement_create_case(&parser->ast.ast_allocator, case_loc,
            colon_loc, expr, (IntegerValue) {0}, body, current_switch);
}

static Statement* parse_default_statement(Parser* parser)
{
    // Parse the statement
    Location default_loc = consume(parser);
    Location colon_loc = match(parser, TOKEN_COLON);

    // Get the current switch context for later
    Statement* current_switch = 
            ast_context_current_switch(&parser->current_context);
    if (!current_switch)
    {
        diagnostic_error_at(parser->dm, default_loc,
                "default statement not in switch statement");
    }

    Statement* stmt = parse_statement(parser);

    return statement_create_default(&parser->ast.ast_allocator, default_loc,
            colon_loc, stmt, current_switch);
}

static Statement* parse_compound_statement(Parser* parser)
{
    // TODO: push new thino context like...

    Location l_curly = match(parser, TOKEN_LCURLY);

    StatementVector stmts = statement_vector_create(4);
    while (!is_match(parser, TOKEN_RCURLY))
    {
        Statement* stmt = parse_statement(parser);
        
        statement_vector_push(&stmts, stmt);
    }

    Location r_curly = match(parser, TOKEN_RCURLY);

    return statement_create_compound(&parser->ast.ast_allocator, l_curly,
            r_curly, &stmts);
}

static Statement* parse_expression_statement(Parser* parser)
{
    // Need some way to check if we can start and expression
    // like idk might need some kind of expression start set
    Expression* expr = parse_expression(parser);
    
    Location semi_loc = match(parser, TOKEN_SEMI);

    return statement_create_expression(&parser->ast.ast_allocator, semi_loc,
            expr);
}

static Statement* parse_if_statement(Parser* parser)
{
    Location if_loc = consume(parser);
    Location lparen_loc = match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    Location rparen_loc = match(parser, TOKEN_RPAREN);

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
    Location switch_loc = consume(parser);

    Location lparen_loc = match(parser, TOKEN_LPAREN);

    Expression* expr = parse_expression(parser);

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    Statement* switch_stmt = statement_create_switch(&parser->ast.ast_allocator,
            switch_loc, lparen_loc, rparen_loc, expr);

    // Create the switch statement and set the current breakable to be the
    AstContext old_ctx = ast_context_push_switch(&parser->current_context, 
            switch_stmt);

    Statement* body = parse_statement(parser);

    statement_switch_set_body(switch_stmt, body);

    // Restore the ast context
    ast_context_pop(&parser->current_context, old_ctx);

    return switch_stmt;
}

static Statement* parse_while_statement(Parser* parser)
{
    Location while_loc = consume(parser);

    Location lparen_loc = match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    // Create the statement and push the context
    Statement* while_stmt = statement_create_while(&parser->ast.ast_allocator,
            while_loc, lparen_loc, rparen_loc, cond);

    AstContext old_ctx = ast_context_push_while(&parser->current_context, 
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
    Location do_loc = consume(parser);

    // Create statement and push context
    Statement* do_stmt = statement_create_do_while(&parser->ast.ast_allocator,
            do_loc);

    AstContext old_ctx = ast_context_push_do_while(&parser->current_context, 
            do_stmt);

    Statement* body = parse_statement(parser);

     // Restore the ast context
    ast_context_pop(&parser->current_context, old_ctx);
    
    // If the token is not a while just skip till we get a semi...
    if (current_token_type(parser) != TOKEN_WHILE)
    {
        // TODO: should this location be of the current token instead?
        diagnostic_error_at(parser->dm, do_loc,
                "expected while in do/while loop");

        eat_until_and(parser, TOKEN_SEMI);

        return statement_create_error(&parser->ast.ast_allocator);
    }

    // make sure we capture the while part.
    Location while_loc = match(parser, TOKEN_WHILE);

    Location lparen_loc = match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    Location semi_loc = match(parser, TOKEN_SEMI);

    statement_do_while_set_body(do_stmt, while_loc, lparen_loc, rparen_loc, 
            cond, body);

    return do_stmt;
}

static Statement* parse_for_statement(Parser* parser)
{
    Location for_loc = consume(parser);
    Location lparen_loc = match(parser, TOKEN_LPAREN);

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
        // TODO: ensure this is adequete error recovery
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "bad initialisation in for statement");

        eat_until_and(parser, TOKEN_SEMI);

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
    AstContext saved_ctx = ast_context_push_for(&parser->current_context, 
            for_stmt);
    
    // TODO: change parse statement to allow / not allow declarations?
    Statement* body = parse_statement(parser);
    if (body != NULL && body->base.type == STATEMENT_DECLARATION) {
        diagnostic_error_at(parser->dm,
                body->declaration_stmt.declaration->base.location,
                "declaration not allowed here; create a compound"
                "statement {...}");
    }

    statement_for_set_body(for_stmt, body);

    // Restore the current context
    ast_context_pop(&parser->current_context, saved_ctx);

    return for_stmt;
}

static Statement* parse_goto_statement(Parser* parser)
{
    Location goto_loc = consume(parser);

    if (current_token_type(parser) != TOKEN_IDENTIFIER)
        ; // TODO: in trouble but don't know how to fix...
    // TODO: I think to recover we just create an error expression and move on
    // with our lives...

    // TODO: create the label if it does not already exist
    const Token* label_name = current_token(parser);
    match(parser, TOKEN_IDENTIFIER);

    Location semi_loc = match(parser, TOKEN_SEMI);

    return statement_create_goto(&parser->ast.ast_allocator, goto_loc,
            semi_loc, NULL);
}

static Statement* parse_continue_statement(Parser* parser)
{
    Location continue_loc = consume(parser);
    Location semi_loc = match(parser, TOKEN_SEMI);

    Statement* current_iteration = 
            ast_context_current_iterable(&parser->current_context);
    if (!current_iteration)
    {
        diagnostic_error_at(parser->dm, continue_loc, 
                "continue statement not in loop statement");
    }

    return statement_create_contine(&parser->ast.ast_allocator, continue_loc, 
            semi_loc, current_iteration);
}

static Statement* parse_break_statement(Parser* parser)
{
    Location break_loc = consume(parser);
    Location semi_loc = match(parser, TOKEN_SEMI);

    Statement* current_breakable = parser->current_context.current_breakable;
    if (!current_breakable)
    {
        diagnostic_error_at(parser->dm, break_loc,
                "break statement not in loop or switch statement");
    }

    return statement_create_break(&parser->ast.ast_allocator, break_loc, 
            semi_loc, current_breakable);
}

static Statement* parser_return_statement(Parser* parser)
{
    Location return_loc = consume(parser);

    Expression* expr_opt = NULL;
    if (!is_match(parser, TOKEN_SEMI))
    {
        expr_opt = parse_expression(parser);
    }

    Location semi_loc = match(parser, TOKEN_SEMI);

    return statement_create_return(&parser->ast.ast_allocator, return_loc,
            semi_loc, expr_opt);
}

static Statement* parse_declaration_statement(Parser* parser)
{
    Declaration* decl = parse_declaration(parser);

    Location semi_loc = match(parser, TOKEN_SEMI);

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
    eat_until_and(parser, TOKEN_SEMI);

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
            const Token* current = current_token(parser);
            if (is_typename_start(parser, current))
            {
                return parse_declaration_statement(parser);
            }
            else if (is_expression_start(parser, current))
            {
                return parse_expression_statement(parser);    
            }
            else
            {
                // TODO: might have to eventually change this sometime...
                diagnostic_error_at(parser->dm,
                        current_token_start_location(parser),
                        "expected expression");

                return parse_error_statement(parser);
            }
        }
    }
}

// Below is things for declarations

static Declaration* parse_typedef_name(Parser* parser)
{
    Token* current = current_token(parser);

    if (!is_match(parser, TOKEN_IDENTIFIER)) {
        // TODO: not a typedef for sure
    }

    if (/*get_typename(get_identifier(current)) == NULL*/0) {
        // TODO: not a typedef either
    }

    // TODO: fix this later
    match(parser, TOKEN_IDENTIFIER);
 
    return NULL;
}

static Initializer* parse_designation(Parser* parser)
{
    // TODO: im not sure were this comes from!

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
        DeclarationSpecifiers* specifiers)
{
    Declarator declarator = declarator_create();
    parse_declarator(parser, &declarator);

    // TODO: determine the actual type of the declaration from the specifiers
    // TODO: and declarator so that we can then do the next part.


    declarator_delete(&declarator);

    Initializer* initializer = NULL;
    if (is_match(parser, TOKEN_EQUAL))
    {
        consume(parser);
        
        initializer = parse_initializer(parser);
    }

    // TODO: we will need to properly determine the type from the declarator...
    // Declaration* declaration = declaration_create_variable(
    //         &parser->ast.ast_allocator, declarator.identifier_location,
    //         declarator.identifier, (QualifiedType) {0}, 
    //         specifiers->storage_spec, initializer);

    // if (symbol_table_contains(&parser->symbols, declarator.identifier))
    // {
    //     // Here we give and error for our duplicate definition and note where
    //     // the preivious one is to be helpful.
    //     Declaration* symbol = symbol_table_lookup(&parser->symbols,
    //             declarator.identifier);
    //     diagnostic_error_at(parser->dm, declaration->base.location,
    //             "redefinition of '%s'",
    //             declarator.identifier->string.ptr);
    //     diagnostic_help_at(parser->dm, symbol->base.location,
    //             "previous definition is here");
    // }
    // else
    // {
    //     symbol_table_insert(&parser->symbols, declaration);
    // }
    
    return NULL;
}

static Declaration* parse_init_declarator_list(Parser* parser,
        DeclarationSpecifiers* specifiers)
{
    Declaration* decl = NULL;
    do
    {
        decl = parse_init_declarator(parser, specifiers);
    } while (try_match(parser, TOKEN_COMMA));
 
    return decl;
}

static void parse_identifier_list(Parser* parser, Declarator* declarator)
{
    Location lparen_loc = consume(parser);

    // Have some set of declarations up here or something?

    while (true)
    {
        if (!is_match(parser, TOKEN_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected identifier");
            break;
            
            // TODO: maybe push back an empty array thing or idk??
        }

        Identifier* identifier = current_token(parser)->data.identifier;
        if (/*is_typedef(parser...)*/0)
        {
            // parse_error(parser, "identifier cannot be a typedef name");
        }

        Location identifier_loc = consume(parser);

        // TODO: add to some sort of set or vector or something

        if (!is_match(parser, TOKEN_COMMA))
        {
            break;
        }

        consume(parser);
    }

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    // TODO: cleanup here...
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);

    // TODO: will need to change the parsing to where we can have an optional
    // identifier in the declarator and then diagnose later if we needed one but
    // didnt have one. Since abstract vs normal declaration happens at a later
    // stage then the first token

    Declarator declarator = declarator_create();
    parse_declarator(parser, &declarator);

    // TODO: turn into declaration...

    declarator_delete(&declarator);

    return NULL;
}

static void parse_empty_function_declarator(Parser* parser, 
        Declarator* declarator)
{
    Location lparen_loc = consume(parser);
    Location rparen_loc = consume(parser);

    // TODO: push back on the declarator
}

static void parse_paramater_type_list(Parser* parser, Declarator* declarator)
{
    Location lparen_loc = consume(parser);

    bool is_variadic = false;
    while (true)
    {
        Declaration* declaration = parse_paramater_declaration(parser);

        if (is_match(parser, TOKEN_COMMA) && 
                is_next_match(parser, TOKEN_ELIPSIS))
        {
            consume(parser);
            consume(parser);

            // TODO: set that is it variadic and we can break

            break;
        }
        else if (is_match(parser, TOKEN_COMMA))
        {
            consume(parser);
        }
        else
        {
            break;
        }
    }

    Location rparen_loc = match(parser, TOKEN_RPAREN);

    // TODO: then we have to finish all of these off and add an function
    // declarator to the end.
}

static void parse_function_declarator(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LPAREN));

    if (is_next_match(parser, TOKEN_RPAREN))
    {
        parse_empty_function_declarator(parser, declarator);
    }
    else if (is_next_match(parser, TOKEN_IDENTIFIER))
    {
        parse_identifier_list(parser, declarator);
    }
    else
    {
        // TODO: should we instead check here to see if we can actually start
        // TODO: declaration instead?
        parse_paramater_type_list(parser, declarator);
    }
}

static void parse_array_declarator(Parser* parser, Declarator* declarator)
{
    assert(is_match(parser, TOKEN_LBRACKET));

    Location lbracket_loc = consume(parser);
    
    bool is_static = false;
    if (is_match(parser, TOKEN_STATIC))
    {
        consume(parser);

        is_static = true;
    }

    TypeQualifiers qualifiers = TYPE_QUALIFIER_NONE;
    if (has_match(parser, type_qualifier, type_qualifier_count))
    {
        qualifiers = parse_type_qualifier_list(parser);
    }

    // We should never get the static keyword twice...
    if (!is_static && is_match(parser, TOKEN_STATIC))
    {
        consume(parser);

        is_static = true;
    }
    
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

    // TODO: error recovery here on bad input...

    Location rbracket_loc = match(parser, TOKEN_RBRACKET);

    declarator_push_array(declarator, qualifiers, expression, is_static,
            is_star);
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
        Location rparen_loc = match(parser, TOKEN_RPAREN);
    }
    else 
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),\
                "expected identifier or '('");
        return;
    }

    while (is_match(parser, TOKEN_LPAREN) || is_match(parser, TOKEN_LBRACKET))
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

    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
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
        else
        {
            panic("unreachable");
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

// TODO: I REALLY WANT TO REMOVE THIS BUT DONT KNOW HOW. SHOULD I MAKE LIKE AN
// INCREMENTAL PARSING FOR DECLARATION SPECIFIERS????
static TypeQualifiers parse_type_qualifier(Parser* parser)
{
    consume(parser);

    return TYPE_QUALIFIER_NONE;
}

static TypeQualifiers parse_type_qualifier_list(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    TypeQualifiers qualifers = TYPE_QUALIFIER_NONE;
    do
    {
        TypeQualifiers new_qualifier = parse_type_qualifier(parser);
        qualifers |= new_qualifier;

        // TODO: should we maybe warn if we get it again? TODO: this would
        // require us keeping track of the location of the different qualifiers
    }
    while (has_match(parser, type_qualifier, type_qualifier_count)); 

    return qualifers;
}

static TypeQualifiers parse_type_qualifier_list_opt(Parser* parser)
{
    if (!has_match(parser, type_qualifier, type_qualifier_count))
    {
        return TYPE_QUALIFIER_NONE;
    }

    return parse_type_qualifier_list(parser);
}

static Declaration* parse_struct_declarator(Parser* parser)
{   
    // TODO: this is a really bad way to do this 

    // TODO: and turn looking for a comma token into a colon token

    // If we get a bit field off the bar
    if (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
        parse_constant_expression(parser);

        return NULL;
    }

    // TODO: not sure yet. But this is needed to avoid a segfault...
    Declarator declarator = declarator_create();

    parse_declarator(parser, &declarator);

    declarator_delete(&declarator);

    // If we maybe have a bitfield after
    if (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
        parse_constant_expression(parser);

        return NULL;
    }

    return NULL;
}

static Declaration* parse_struct_declarator_list(Parser* parser)
{
    parse_struct_declarator(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
        parse_struct_declarator(parser);
    }
 
    return NULL;
}

static Declaration* parse_struct_declaration(Parser* parser)
{
    DeclarationSpecifiers specifier = parse_declaration_specifiers(parser);

    parse_struct_declarator_list(parser);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Declaration* parse_struct_declaration_list(Parser* parser)
{
    while (current_token_type(parser) != TOKEN_RCURLY)
    {
        parse_struct_declaration(parser);
    }

    return NULL;
}

static Declaration* parse_struct_or_union_specifier(Parser* parser)
{
    assert(has_match(parser, (TokenType[]) {TOKEN_STRUCT, TOKEN_UNION}, 2));

    consume(parser);

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);

        // We matched an identifier so continuing the struct defn is optional...
        if (!is_match(parser, TOKEN_LCURLY))
        {
            return NULL;
        }
    }
    
    if (is_match(parser, TOKEN_LCURLY))
    {
        match(parser, TOKEN_LCURLY);
        parse_struct_declaration_list(parser);
        match(parser, TOKEN_RCURLY);
    }
    
    return NULL;
}

static Type* type_from_declaration_specifiers(Parser* parser)
{
    // TODO: we will need to get the type fro mthe typekind
    return NULL;
}

static Declaration* parse_enumerator_list(Parser* parser)
{
    while (true)
    {
        // Here parse the enumerator declaration placing it in the symbol table
        if (!is_match(parser, TOKEN_IDENTIFIER))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected identifier");

            eat_until(parser, TOKEN_RCURLY);
            break;
        }

        Identifier* identifier = current_token(parser)->data.identifier;
        Location identifier_loc = consume(parser);

        Location equal_loc = LOCATION_INVALID;
        Expression* expression = NULL;
        if (is_match(parser, TOKEN_EQUAL))
        {
            equal_loc = consume(parser);
            expression = parse_constant_expression(parser);
        }

        // Here we should make sure to actually build the enumeration and note
        // that we will also need to keep track of the previous declaration so
        // that we can correctly assign numbers to it. We will also need to
        // check to ensure that the identifier is not currently in the normal
        // namespace.


        // Now see if we are at the end and finish the definition        
        if (!is_match(parser, TOKEN_RCURLY) && !is_match(parser, TOKEN_COMMA))
        {
            diagnostic_error_at(parser->dm,
                    current_token_start_location(parser),
                    "expected ',' after enumerator");

            // Eat until a comma or a right curly brace. We could possibly
            // include IDENTIFIER here but this is good for now.
            eat_until_v(parser, (TokenType[]) {TOKEN_COMMA, TOKEN_RCURLY}, 2);

            break;
        }

        // Here we can consume the comma.
        if (is_match(parser, TOKEN_COMMA))
        {
            consume(parser);
        }

        // Finally check for a trailing '}'
        if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }
    }

    return NULL;
}

// TODO: i think we should change the return type of this here. It probably
// shouldn't be a full declaration. Maybe just a
static Declaration* parse_enum_specificier(Parser* parser)
{
    Location enum_location = consume(parser);

    // We need to have a match for one of these here otherwise we have a problem
    if (!is_match(parser, TOKEN_IDENTIFIER) && !is_match(parser, TOKEN_LCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "expected identifier or '{'");

        eat_until(parser, TOKEN_SEMI);

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

    // If we had an identifier we will need to check it is not currently in the
    // tag namespace. Also should add the identifier into the tag namespace if 
    // we get it.
    if (identifier)
    {
        // TODO: check...
    }

    // TODO: create an enumeration declaration, so that we can add the 
    // enumerators to the enum as we parse then (if needed)

    // Here we should match a left curly
    Location opening_curly = match(parser, TOKEN_LCURLY);

    // Check for this common mistake.
    if (is_match(parser, TOKEN_RCURLY))
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser), 
                "cannot have an empty enum");

        consume(parser);

        return declaration_create_error(&parser->ast.ast_allocator,
                enum_location);
    }

    parse_enumerator_list(parser);

    Location closing_curly = match(parser, TOKEN_RCURLY);

    return NULL;
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
                "cannot combine '%s' with previous '%s' stoage specifier",
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
        diagnostic_error_at(parser->dm, location,
                "cannot compbine '%s' with previous '%s' type specifier",
                type_specifier_to_name(type),
                type_specifier_to_name(specifiers->type_spec_type));
    }
}

static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser)
{
    // Create our base declaration specifiers to use.
    DeclarationSpecifiers specifiers = (DeclarationSpecifiers)
    {
        .type = NULL,
        .storage_spec = TYPE_STORAGE_SPECIFIER_NONE,
        .qualifiers = TYPE_QUALIFIER_NONE,
        .function_spec = TYPE_FUNCTION_SPECIFIER_NONE,
        .type_spec_type = TYPE_SPECIFIER_TYPE_NONE,
        .type_spec_width = TYPE_SPECIFIER_WIDTH_NONE,
        .type_spec_sign = TYPE_SPECIFIER_SIGN_NONE,
        .type_spec_complex = TYPE_SPECIFIER_COMPLEX_NONE
    };

    while (true)
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
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_STRUCT);
                parse_struct_or_union_specifier(parser);
                break;

            case TOKEN_UNION:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_UNION);
                parse_struct_or_union_specifier(parser);
                break;
                    
            case TOKEN_ENUM:
                declaration_specifiers_add_type(parser, &specifiers,
                        TYPE_SPECIFIER_TYPE_ENUM);
                parse_enum_specificier(parser);
                break;
            
            // Special case of identifier since we could have a typedef.
            case TOKEN_IDENTIFIER:
                // If any of the below are true this identifier is considiered
                // to be part of the direct declarator, even if it is a typedef
                // Also if this token is not a typename anyways we are also done
                if (specifiers.type_spec_sign != TYPE_SPECIFIER_SIGN_NONE ||
                    specifiers.type_spec_width != TYPE_SPECIFIER_WIDTH_NONE ||
                    specifiers.type_spec_complex != TYPE_SPECIFIER_COMPLEX_NONE
                    || specifiers.type_spec_type != TYPE_SPECIFIER_TYPE_NONE ||
                        /*!is_typename(current_token(parser))*/0)
                {
                    goto done_specifiers;
                }

                // break; // TODO: uncomment this later.

                /* FALLTHROUGH */

            default:
                goto done_specifiers;
        }
    }

done_specifiers:;
    // TODO: we now need to re-add how to determine the final type kind...

    return specifiers;
}

static Declaration* parse_declaration(Parser* parser)
{
    // First we need to get our declaration specifiers here
    DeclarationSpecifiers decl_spec = parse_declaration_specifiers(parser);

    if (!is_match(parser, TOKEN_SEMI))
    {
        parse_init_declarator_list(parser, &decl_spec);
    }
    else
    {
        // diagnostic_warning_at(parser->dm, current_token_start_location(parser),
        //         "declaration does not declarate anything");

        // parse_error(parser, "declaration does not declare anything");
        // TODO: maybe a warning about how we didn't really declare anything
    }

    return NULL;
}

static Declaration* parse_type_name(Parser* parser)
{
    const TokenType operators[] = {TOKEN_STAR, TOKEN_LPAREN, TOKEN_LBRACKET};
    const size_t num_operators = countof(operators);

    // TODO: this should be parse specifier qualifier list... but idk...
    DeclarationSpecifiers specifiers = parse_declaration_specifiers(parser);
    Declarator declarator = declarator_create();

    if (has_match(parser, operators, num_operators))
    {
        parse_abstract_declarator(parser, &declarator, &specifiers);
    }

    declarator_delete(&declarator);

    return NULL;
}

// The definitions of the functions we will use for pasing

static Declaration* parse_top_level_declaration_or_definition(Parser* parser)
{
    Declaration* decl = parse_declaration(parser);

    if (is_match(parser, TOKEN_SEMI))
    {
        consume(parser);

        return decl;
    }

    if (is_match(parser, TOKEN_LCURLY) && 
            declaration_is(decl, DECLARATION_FUNCTION))
    {
        Statement* body = parse_compound_statement(parser);
    }
    else
    {
        diagnostic_error_at(parser->dm, current_token_start_location(parser),
                "expected ';' after declaration");

        eat_until_and(parser, TOKEN_SEMI);
    }

    return decl;
}

static void parse_translation_unit_internal(Parser* parser)
{
    add_recover_token(parser, TOKEN_EOF);

    while (current_token_type(parser) != TOKEN_EOF)
    {
        Declaration* decl = parse_top_level_declaration_or_definition(parser);

        declaration_vector_push(&parser->ast.top_level_decls, decl);
    }

    remove_recover_token(parser, TOKEN_EOF);

    declaration_vector_free(&parser->ast.top_level_decls, NULL);
    ast_allocator_delete(&parser->ast.ast_allocator);
    symbol_table_delete(&parser->symbols);
}

void parse_translation_unit(DiagnosticManager* dm, Preprocessor* pp)
{
    Parser parser;
    parser.dm = dm;
    parser.pp = pp;
    preprocessor_advance_token(pp, &parser.token);
    memset(parser.recover_set, 0, sizeof(parser.recover_set));
    parser.ast = ast_create();
    parser.symbols = symbol_table_create(SYMBOL_NAMESPACE_NONE);

    parse_translation_unit_internal(&parser);
}
