#include "parser.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "files/line_map.h"
#include "files/location.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

#include "parse/literal_parser.h"
#include "parse/type.h"
#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"
#include "parse/ast_allocator.h"
#include "parse/ast.h"

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

static bool is_valid_stream_position(TokenStream* stream)
{
    return stream->current_token < stream->count;
}

// Return the type of the current token in a token stream
static TokenType token_stream_current_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));
    return stream->tokens[stream->current_token].type;
}

static TokenType token_stream_next_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));
    return stream->tokens[stream->current_token + 1].type;
}

static Token* token_stream_current(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));
    return &stream->tokens[stream->current_token];
}

static Token* token_stream_next(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));
    return &stream->tokens[stream->current_token + 1];
}

static void token_stream_advance(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));
    assert(token_stream_current_type(stream) != TOKEN_EOF);
    stream->current_token++;
}

// Now we are below the token stream methods we have the parser methods. These
// are the actual onces which should be called and acted upon

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

static void parse_error(Parser* parser, const char* fmt, ...)
{
    Token* tok = current_token(parser);   

    ResolvedLocation loc = line_map_resolve_location(parser->map, tok->loc);
    fprintf(stderr, "%u:%u\n", loc.line, loc.col);
    va_list args;
    va_start(args, fmt);
    diag_verror(fmt, args);
    va_end(args);
}

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
    return token_stream_current(parser->stream);
}

static Token* next_token(Parser* parser)
{
    return token_stream_next(parser->stream);
}

static TokenType current_token_type(Parser* parser)
{
    return token_stream_current_type(parser->stream);
}

static TokenType next_token_type(Parser* parser)
{
    return token_stream_next_type(parser->stream);
}

static Location current_token_start_location(Parser* parser)
{
    Token* current = token_stream_current(parser->stream);
    
    return current->loc;
}

static Location current_token_end_location(Parser* parser)
{
    Token* current = token_stream_current(parser->stream);
    
    return current->end;
}

// Methods for mathing a token type or unconditionally consuming a token.
static Location match(Parser* parser, TokenType type)
{
    if (token_stream_current_type(parser->stream) == type)
    {
        const Token* current = current_token(parser);

        token_stream_advance(parser->stream);

        return current->loc;
    }

    parse_error(parser, "expected '%s' but got '%s'", 
            token_type_get_name(type), 
            token_type_get_name(current_token_type(parser))
        );

    // panic("failed to match token... TODO: finish this method");

    return LOCATION_INVALID;
}

static Location consume(Parser* parser)
{
    const Token* current = current_token(parser);

    token_stream_advance(parser->stream);

    return current->loc;
}

static bool is_match(Parser* parser, TokenType type)
{
    return token_stream_current_type(parser->stream) == type;
}

static bool has_match(Parser* parser, const TokenType* types, size_t count)
{
    const TokenType current = token_stream_current_type(parser->stream);

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
    return token_stream_next_type(parser->stream) == type;
}

static void eat_until(Parser* parser, TokenType type)
{
    assert(!is_match(parser, type));

    while (!is_match(parser, type) && !is_match(parser, TOKEN_EOF))
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

// TODO: we definitely want to add methods like for example the below
static bool is_typename_start(Parser* parser, const Token* tok);

static bool is_expression_start(Parser* parser, const Token* tok);
// TODO: however I think we will need a symbol table and stuff for the next part
// TODO: for sure though

// TODO: implement below so we can do important stuff
static bool is_declorator_start(Parser* parser, const Token* tok);
static bool is_abstract_declarator_start(Parser* parser, const Token* tok);

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_integer_constant(Parser* parser);
static Expression* parse_floating_constant(Parser* parser);
static Expression* parse_enumeration_constant(Parser* parser);
static Expression* parse_character_constant(Parser* parser);
static Expression* parse_constant(Parser* parser);

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
static Declaration* parse_designation(Parser* parser);
static Declaration* parse_designator_list(Parser* parser);
static Declaration* parse_designator(Parser* parser);
static Declaration* parse_initializer(Parser* parser);
static Declaration* parse_initializer_list(Parser* parser);
static Declaration* parse_declarator(Parser* parser);
static Declaration* parse_init_declarator(Parser* parser);
static Declaration* parse_init_declarator_list(Parser* parser);

static Declaration* parse_typedef_name(Parser* parser);
static Declaration* parse_enumerator_list(Parser* parser);
static Declaration* parse_enum_specificer(Parser* parser);
// TODO: condense below into 2 funcs
static Declaration* parse_direct_declarator(Parser* parser);
static Declaration* parse_declarator(Parser* parser);
static Declaration* parse_direct_abstract_declarator(Parser* parser);
static Declaration* parse_abstract_declarator(Parser* parser);

static Declaration* parse_declarator_new(Parser* parser, bool maybe_abstract);
static Declaration* parse_direct_declarator_new(Parser* parser, 
        bool maybe_abstract);
// TODO: end above
static Declaration* parse_pointer(Parser* parser);
static Declaration* parse_identifier_list(Parser* parser);
static Declaration* parse_paramater_declaration(Parser* parser);
static Declaration* parse_paramater_list(Parser* parser);
static Declaration* parse_paramater_type_list(Parser* parser);
static Declaration* parse_type_specificer(Parser* parser);

// Functions for us getting our declaration specifiers
static TypeFunctionSpecifier parse_function_specificer(Parser* parser);
static TypeQualifiers parse_type_qualifier(Parser* parser);
static TypeStorageSpecifier parse_storage_class_specifier(Parser* parser);
static TypeSpecifier parse_type_specifier(Parser* parser);

static bool has_declaration_specifier(Parser* parser, const Token* tok);
static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser);

static Declaration* parse_type_qualifier_list(Parser* parser);
static Declaration* parse_specifier_qualifier_list(Parser* parser);
static Declaration* parse_struct_declarator(Parser* parser);
static Declaration* parse_struct_declarator_list(Parser* parser);
static Declaration* parse_struct_declaration(Parser* parser);
static Declaration* parse_struct_declaration_list(Parser* parser);
static Declaration* parse_struct_or_union(Parser* parser);
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
        // case TOKEN_EXTERN:
        // case TOKEN_STATIC:
        // case TOKEN_TYPEDEF:
        // case TOKEN_INLINE:
        // case TOKEN_CONST:
        // case TOKEN_VOLATILE:
        // case TOKEN_REGISTER:
        // case TOKEN_AUTO:
            return true;

        case TOKEN_IDENTIFIER:
            // This is hard to get right but very very important!
            
            // TODO: this requires switching identifier tokens to a Identifier*

            return false;

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

            return expr;
        }
        
        case TOKEN_IDENTIFIER:
        {
            // TODO: here we could be a few possible things so we need to
            // TODO: check for that...

            consume(parser);

            return NULL;
        }

        case TOKEN_NUMBER:
        {
            consume(parser);

            IntegerValue value = {0};
            const bool success = parse_integer_literal(&value, current);

            Expression* expr;
            if (!success)
            {
                diag_error("integer literal conversion failed");

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
            size_t string_count = 0;
            while (is_string_token(parser, current_token(parser)))
            {
                consume(parser);
                string_count++;
            }

            StringLiteral string = {0};
            const bool success = parse_string_literal(&string, current, string_count);

            Expression* expr;
            if (!success)
            {
                diag_error("string concatenation and conversion failed");

                // expr = create_error_expression(current);
            }
            else
            {
                // expr = create_string_expression(current, &string);
            }

            return NULL;
        }
        
        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
        {
            consume(parser);

            CharValue value = {0};
            const bool success = parse_char_literal(&value, current);

            Expression* expr;
            if (!success)
            {
                diag_error("character conversion failed");

                // expr = create_error_expression(current);
            }
            else
            {
                // expr = create_character_expression(current, &value);
            }

            return NULL;
        }

        default:
            parse_error(parser, "expected expression");

            // Location start = current_token_start_location(parser);

            // // TODO: eat some bad tokens may only be one or two...

            // Location end = current_token_end_location(parser);

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
                Location rparen_loc = match(parser, TOKEN_RPAREN);
                // TODO: create sizeof expression
                return NULL;
            }
            else
            {
                Expression* expr = parse_unary_expression(parser);
                
                // TODO: create sizeof expression
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

static ExpressionType assignment_operator_to_type(TokenType type)
{
    switch (type) {
        case TOKEN_EQUAL: return EXPRESSION_BINARY_ASSIGN;
        case TOKEN_STAR_EQUAL: return EXPRESSION_BINARY_TIMES_ASSIGN;
        case TOKEN_SLASH_EQUAL: return EXPRESSION_BINARY_DIVIDE_ASSIGN;
        case TOKEN_PERCENT_EQUAL: return EXPRESSION_BINARY_MODULO_ASSIGN;
        case TOKEN_PLUS_EQUAL: return EXPRESSION_BINARY_ADD_ASSIGN;
        case TOKEN_MINUS_EQUAL: return EXPRESSION_BINARY_SUBTRACT_ASSIGN;
        case TOKEN_LT_LT_EQUAL: return EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN;
        case TOKEN_GT_GT_EQUAL: return EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN;
        case TOKEN_AND_EQUAL: return EXPRESSION_BINARY_AND_ASSIGN;
        case TOKEN_XOR_EQUAL: return EXPRESSION_BINARY_XOR_ASSIGN;
        case TOKEN_OR_EQUAL: return EXPRESSION_BINARY_OR_ASSIGN;
        default: panic("unreachable"); return EXPRESSION_ERROR;
    }
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
        // Determine what type of expression we have
        ExpressionType type = 
                assignment_operator_to_type(current_token_type(parser));
        
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
    Token* label = current_token(parser);

    // TODO: create the declaration of the label

    Location label_loc = consume(parser);
    Location colon_loc = consume(parser);

    // TODO: need to fix parsing here since labels should have a statement after
    // them. So will need some kind of statement start function and then have
    // errors / warnings after
    // Statement* body = parse_statement(parser);

    return statement_create_label(&parser->ast.ast_allocator, label_loc,
            colon_loc, NULL, NULL);
}

static Statement* parse_case_statement(Parser* parser)
{
    Statement* current_switch = 
            ast_context_current_switch(&parser->current_context);
    if (!current_switch)
    {
        parse_error(parser, "case statement not in switch statement");
    }
    
    Location case_loc = consume(parser);
    
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
    // Get the current switch context for later
    Statement* current_switch = 
            ast_context_current_switch(&parser->current_context);
    if (!current_switch)
    {
        parse_error(parser, "default statement not in switch statement");
    }

    // Parse the statement
    Location default_loc = consume(parser);
    Location colon_loc = match(parser, TOKEN_COLON);

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
        parse_error(parser, "expected while in do/while loop");

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
        parse_error(parser, "bad initialisation in for statement");

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
        parse_error(parser, "declaration not allowed here; create a compound "
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
    Statement* current_iteration = 
            ast_context_current_iterable(&parser->current_context);
    if (!current_iteration)
    {
        parse_error(parser, "continue statement not in loop statement");
    }

    Location continue_loc = consume(parser);
    Location semi_loc = match(parser, TOKEN_SEMI);

    return statement_create_contine(&parser->ast.ast_allocator, continue_loc, 
            semi_loc, current_iteration);
}

static Statement* parse_break_statement(Parser* parser)
{
    Statement* current_breakable = parser->current_context.current_breakable;
    if (!current_breakable)
    {
        parse_error(parser, "break statement not in loop or switch statement");
    }

    Location break_loc = consume(parser);
    Location semi_loc = match(parser, TOKEN_SEMI);

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
    eat_until_and(parser, TOKEN_SEMI);eat_until_and(parser, TOKEN_SEMI);

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
            if (next_token_type(parser) == TOKEN_COLON)
            {
                return parse_label_statement(parser);
            }
            
            /* FALLTHROUGH */

        default: 
        {
            const Token* current = current_token(parser);
            if (has_declaration_specifier(parser, current))
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
                parse_error(parser, "expected expression");

                return parse_error_statement(parser);
            }
        }
    }
}

// Below is things for declarations

static Declaration* parse_designation(Parser* parser)
{
    // TODO: im not sure were this comes from!

    parse_designator_list(parser);

    match(parser, TOKEN_EQUAL);
 
    return NULL;
}

static Declaration* parse_designator_list(Parser* parser)
{
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_DOT}, 2))
    {
        parse_designator(parser);
    }
 
    return NULL;
}

static Declaration* parse_designator(Parser* parser)
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

static Declaration* parse_initializer(Parser* parser)
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

static Declaration* parse_initializer_list(Parser* parser)
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

static Declaration* parse_declarator(Parser* parser)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }

    parse_direct_declarator(parser);
 
    return NULL;
}

static Declaration* parse_init_declarator(Parser* parser)
{
    parse_declarator(parser);

    if (is_match(parser, TOKEN_EQUAL))
    {
        consume(parser);
        parse_initializer(parser);
    }

    return NULL;
}

static Declaration* parse_init_declarator_list(Parser* parser)
{
    parse_init_declarator(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);

        parse_init_declarator(parser);
    }
 
    return NULL;
}

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

static Declaration* parse_enumerator(Parser* parser)
{
    Token* current = current_token(parser);
    
    match(parser, TOKEN_IDENTIFIER);
    
    if (is_match(parser, TOKEN_EQUAL))
    {
        Location equal_location = consume(parser);
        parse_constant_expression(parser);
    }

    return NULL;
}

static Declaration* parse_enumerator_list(Parser* parser)
{
    parse_enumerator(parser);

    while (is_match(parser, TOKEN_COMMA) && !is_next_match(parser, TOKEN_RCURLY))
    {
        consume(parser);

        parse_enumerator(parser);
    }
 
    return NULL;
}

static Declaration* parse_enum_specificer(Parser* parser)
{
    Location enum_location = consume(parser);

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        const Token* token = current_token(parser);
        Location identifier_loc = consume(parser);
    }

    if (!is_match(parser, TOKEN_LCURLY))
    {
        return NULL;
    }

    // Here we should match a left curly
    match(parser, TOKEN_LCURLY);

    parse_enumerator_list(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
    }

    match(parser, TOKEN_RCURLY);

    return NULL;
}

static Declaration* parse_direct_declarator(Parser* parser)
{   
    // Parse the first part
    switch (current_token_type(parser))
    {
        case TOKEN_IDENTIFIER: {
            Token* current = current_token(parser);
            consume(parser);

            // TODO: get the identifier from the current token...
            break;
        }
            
        case TOKEN_LPAREN: {
            Location lparen_loc = consume(parser);
            parse_declarator(parser);
            Location rparen_loc = match(parser, TOKEN_RPAREN);
            break;
        }

        default:
            parse_error(parser, "bad parse_direct_declarator");

            panic("bad parse_direct_declarator");
            break;
    }

    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
    {
        // // TODO: make this cleaner
        // if (is_match(parser, TOKEN_LPAREN))
        // {
        //     Location lparen_loc = consume(parser);

        //     parse_paramater_type_list(parser);

        //     Location rparen_loc = match(parser, TOKEN_RPAREN);
        // }


        switch (current_token_type(parser))
        {
            case TOKEN_LPAREN:
                consume(parser);

                if (is_match(parser, TOKEN_IDENTIFIER))
                {
                    parse_identifier_list(parser);
                }
                else if (is_match(parser, TOKEN_RPAREN))
                {
                    /* Do nothing */
                }
                else
                {
                    parse_paramater_type_list(parser);
                }

                consume(parser);
                break;

            case TOKEN_LBRACKET:
            match(parser, TOKEN_LBRACKET);

            if (is_match(parser, TOKEN_STAR))
            {
                consume(parser);
            }
            else if (has_match(parser, type_qualifier, type_qualifier_count))
            {
                parse_type_qualifier_list(parser);

                if (is_match(parser, TOKEN_STATIC))
                {
                    consume(parser);
                    parse_assignment_expression(parser);
                }
                else if (is_match(parser, TOKEN_STAR))
                {
                    consume(parser);
                }
                else
                {
                    /* should be parsing an assignment expression otherwise
                     * but I don't currently have the tools to confirm this...
                     * TODO: add check to make sure its an assignment expression
                     */
                    parse_assignment_expression(parser);
                }
            }
            else if (is_match(parser, TOKEN_STATIC))
            {
                match(parser, TOKEN_STATIC);

                if (has_match(parser, type_qualifier, type_qualifier_count))
                {
                    parse_type_qualifier_list(parser);
                }

                parse_assignment_expression(parser);
            }
            else if (is_match(parser, TOKEN_NUMBER))
            {
                match(parser, TOKEN_NUMBER);
            }

            match(parser, TOKEN_RBRACKET);
            break;

            default:
                panic("unreachable");
                break;
        }
    }
 
    return NULL;
}

static Declaration* parse_direct_abstract_declarator(Parser* parser)
{
    // if we get '(' abstract-declarator ')' note that abstract declarator can
    // only start with a star, l-paren or l-bracket
    if (is_match(parser, TOKEN_LPAREN) && (is_next_match(parser, TOKEN_STAR)
            || is_next_match(parser, TOKEN_LPAREN) 
            || is_next_match(parser, TOKEN_LBRACKET)))
    {
        Location lparen_loc = consume(parser);
        parse_abstract_declarator(parser);
        Location rparen_loc = match(parser, TOKEN_RPAREN);
    }

    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            consume(parser);
            parse_paramater_type_list(parser);
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
                parse_error(parser, "exprected expression or ']'");
            }

            Location right_bracket = match(parser, TOKEN_RBRACKET);
        }
        else
        {
            panic("unreachable");
        }
    }

    return NULL;
}

static Declaration* parse_abstract_declarator(Parser* parser)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }
    
    parse_direct_abstract_declarator(parser);

    return NULL;
}

static Declaration* parse_declarator_new(Parser* parser, bool maybe_abstract)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }

    // We need to figure out if we parse a direct_declarator or skip!
    parse_direct_declarator_new(parser, maybe_abstract);

    return NULL;
}

static Declaration* parse_direct_declarator_new(Parser* parser, 
        bool maybe_abstract)
{
    // Base cases
    if (is_match(parser, TOKEN_LPAREN))
    {
        consume(parser);
        parse_declarator_new(parser, maybe_abstract);
        match(parser, TOKEN_LPAREN);
    }
    else if (is_match(parser, TOKEN_IDENTIFIER))
    {
        // TODO: add maybe_abstract part in...
        consume(parser);
    }
    else if (maybe_abstract)
    {
        // Can do nothing here...
    }
    else
    {
        parse_error(parser, "expected identifier or '('");
    }

    // The suffix part
    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            consume(parser);

            if (is_match(parser, TOKEN_IDENTIFIER))
            {
                parse_identifier_list(parser);
            }
            else
            {
                parse_paramater_type_list(parser);
            }

            match(parser, TOKEN_RPAREN);
        }
        else if (is_match(parser, TOKEN_LBRACKET))
        {

            match(parser, TOKEN_RBRACKET);
        }
        else
        {
            panic("unreachable");
        }
    }

    return NULL;
}

// TODO: eliminate recursion here
static Declaration* parse_pointer(Parser* parser)
{
    Location star_location = match(parser, TOKEN_STAR);

    TypeQualifiers qualifiers = TYPE_QUALIFIER_NONE;
    while (has_match(parser, type_qualifier, type_qualifier_count))
    {
        TypeQualifiers new_qualifier = parse_type_qualifier(parser);

        qualifiers |= new_qualifier;
    }
    (void) qualifiers;

    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }

    return NULL;
}

static Declaration* parse_identifier_list(Parser* parser)
{
    Location lparen_loc = match(parser, TOKEN_LPAREN);
    
    Token* current = current_token(parser);
    consume(parser);
    
    while (is_match(parser, TOKEN_COMMA))
    {
        Location comma_loc = consume(parser);

        current = current_token(parser);
        match(parser, TOKEN_IDENTIFIER);
    }
    (void) current; // Shutup clang

    match(parser, TOKEN_RPAREN);
 
    return NULL;
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    /*
        declaration-specifiers declarator
        declaration-specifiers abstract-declarator opt
        
                               ^^^^^^^^^^^^^^^^^^^^^^^

        in the above rule we are currently ignoring that abstract declarator
        bs since I cba determining first sets and such
    */

    // TODO: determine how to do the above

    DeclarationSpecifiers decl_spec = parse_declaration_specifiers(parser);

    // TODO: in the case of 'void' this parses the declaration specifiers then
    // has to kill itself since there is only ')' after it. So make the next
    // part options

    // TODO: leave this true for now then we will just fix it up later hopefully
    // TODO: need some function to determine if we're starting a declaration or not
    if (true)
    {
        parse_declarator(parser);
    }
 
    return NULL;
}

static Declaration* parse_paramater_list(Parser* parser)
{
    parse_paramater_declaration(parser);
    // Make sure we don't eat the , ... part if it exists
    while (is_match(parser, TOKEN_COMMA) && next_token_type(parser) != TOKEN_ELIPSIS)
    {
        consume(parser);

        parse_paramater_declaration(parser);
    }

    return NULL;
}

static Declaration* parse_paramater_type_list(Parser* parser)
{
    // parse_paramater_list(parser);

    // if (is_match(parser, TOKEN_COMMA))
    // {
    //     match(parser, TOKEN_COMMA);
    //     match(parser, TOKEN_ELIPSIS);
    // }

    parse_paramater_declaration(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);

        if (is_match(parser, TOKEN_ELIPSIS))
        {
            consume(parser);

            break;
        }
        else
        {
            parse_paramater_declaration(parser);
        }
    }
    
    return NULL;
}

static Declaration* parse_type_qualifier_list(Parser* parser)
{
    parse_type_qualifier(parser);

    while (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }
 
    return NULL;
}

static Declaration* parse_specifier_qualifier_list(Parser* parser)
{
    if (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }
    else if (has_match(parser, type_specifier, type_specifier_count))
    {
        parse_type_specifier(parser);
    }
    else
    {
        match(parser, TOKEN_EOF);

        panic("expected specifier or qualifier");
    }

    // TODO: this could be a bit cleaner????

    while (has_match(parser, type_qualifier, type_qualifier_count)
            || has_match(parser, type_specifier, type_specifier_count))
    {
        if (has_match(parser, type_qualifier, type_qualifier_count))
        {
            parse_type_qualifier(parser);
        }
        else if (has_match(parser, type_specifier, type_specifier_count))
        {
            parse_type_specifier(parser);
        }
    }

    return NULL;
}

static Declaration* parse_struct_declarator(Parser* parser)
{   
    // TODO: this is a really bad way to do this

    // If we get a bit field off the bar
    if (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
        parse_constant_expression(parser);

        return NULL;
    }

    parse_declarator(parser);

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
    parse_specifier_qualifier_list(parser);
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

static Declaration* parse_struct_or_union(Parser* parser)
{
    if (is_match(parser, TOKEN_STRUCT))
    {
        consume(parser);
    }
    else if (is_match(parser, TOKEN_UNION))
    {
        consume(parser);
    }
    else
    {
        panic("expected struct or union");
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

static TypeFunctionSpecifier parse_function_specificer(Parser* parser)
{
    assert(has_match(parser, function_specificer, function_specificer_count));

    consume(parser);
    
    return TYPE_FUNCTION_SPECIFIER_INLINE;
}

static TypeQualifiers parse_type_qualifier(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    TokenType type = current_token_type(parser);
    consume(parser);

    switch (type) 
    {
        case TOKEN_CONST: return TYPE_QUALIFIER_CONST;
        case TOKEN_RESTRICT: return TYPE_QUALIFIER_RESTRICT;
        case TOKEN_VOLATILE: return TYPE_QUALIFIER_VOLATILE;

        default: panic("unreachable"); return TYPE_QUALIFIER_NONE;
    }
}

static TypeStorageSpecifier parse_storage_class_specifier(Parser* parser)
{
    assert(has_match(parser, storage_class, storage_class_count));

    TokenType type = current_token_type(parser);
    consume(parser);

    switch (type)
    {
        case TOKEN_TYPEDEF: return TYPE_STORAGE_SPECIFIER_TYPEDEF;
        case TOKEN_EXTERN: return TYPE_STORAGE_SPECIFIER_EXTERN;
        case TOKEN_STATIC: return TYPE_STORAGE_SPECIFIER_STATIC;
        case TOKEN_AUTO: return TYPE_STORAGE_SPECIFIER_AUTO;
        case TOKEN_REGISTER: return TYPE_STORAGE_SPECIFIER_REGISTER;
        
        default: panic("unreachable"); return TYPE_STORAGE_SPECIFIER_NONE;
    }
}

static bool is_builtin_type_token(Parser* parser)
{
    static const TokenType builtin_type_tokens[] = {TOKEN_VOID, TOKEN_CHAR,
            TOKEN_SHORT, TOKEN_INT, TOKEN_LONG, TOKEN_FLOAT, TOKEN_DOUBLE,
            TOKEN_SIGNED, TOKEN_UNSIGNED, TOKEN__BOOL, TOKEN__COMPLEX,
            TOKEN__IMAGINARY};
    const size_t builtin_type_tokens_size = countof(builtin_type_tokens);

    return has_match(parser, builtin_type_tokens, builtin_type_tokens_size);
}

static Type* parse_builtin_type(Parser* parser)
{
    while (is_builtin_type_token(parser))
    {
        consume(parser);
    }

    return NULL;
}

// TODO: I think this here should return a type
static TypeSpecifier parse_type_specifier(Parser* parser)
{
    assert(is_typename_start(parser, current_token(parser)));

    TokenType type = current_token_type(parser);
    consume(parser);

    switch (type)
    {
        case TOKEN_VOID: return TYPE_SPECIFIER_VOID;
        case TOKEN_CHAR: return TYPE_SPECIFIER_CHAR;
        case TOKEN_SHORT: return TYPE_SPECIFIER_SHORT;
        case TOKEN_INT: return TYPE_SPECIFIER_INT;
        case TOKEN_LONG: return TYPE_SPECIFIER_LONG;
        case TOKEN_FLOAT: return TYPE_SPECIFIER_FLOAT;
        case TOKEN_DOUBLE: return TYPE_SPECIFIER_DOUBLE;
        case TOKEN_SIGNED: return TYPE_SPECIFIER_SIGNED;
        case TOKEN_UNSIGNED: return TYPE_SPECIFIER_UNSIGNED;
        case TOKEN__BOOL: return TYPE_SPECIFIER_BOOL;
        case TOKEN__COMPLEX: return TYPE_SPECIFIER_COMPLEX;
        case TOKEN__IMAGINARY: return TYPE_SPECIFIER_IMAGINAIRY;

        default: panic("unreachable"); return TYPE_SPECIFIER_NONE;
    }
}

// Temporaily turn off -Wswitch diagnostic...
// TODO: i don't like this but I don't know a nicer way to do what I want
// to do below that doesn't involve alot of if's since they can be in any order
// and or have for example a const in between
// e.g. unsigned const int ... is valid

static TypeKind determine_type_kind(TypeSpecifier specifiers)
{
    // TODO: what do we do about _Complex and _Imaginairy
    switch (specifiers) 
    {
        case TYPE_SPECIFIER_VOID:
            return TYPE_VOID;

        case TYPE_SPECIFIER_CHAR:
            return TYPE_CHAR;

        case TYPE_SPECIFIER_CHAR | TYPE_SPECIFIER_SIGNED:
            return TYPE_S_CHAR;

        case TYPE_SPECIFIER_CHAR | TYPE_SPECIFIER_UNSIGNED:
            return TYPE_U_CHAR;

        case TYPE_SPECIFIER_SHORT:
        case TYPE_SPECIFIER_SHORT | TYPE_SPECIFIER_SIGNED:
        case TYPE_SPECIFIER_SHORT | TYPE_SPECIFIER_INT:
        case TYPE_SPECIFIER_SHORT | TYPE_SPECIFIER_SIGNED | TYPE_SPECIFIER_INT:
            return TYPE_S_SHORT;

        case TYPE_SPECIFIER_SHORT | TYPE_SPECIFIER_UNSIGNED:
        case TYPE_SPECIFIER_SHORT | TYPE_SPECIFIER_UNSIGNED | TYPE_SPECIFIER_INT:
            return TYPE_U_SHORT;

        case TYPE_SPECIFIER_INT:
        case TYPE_SPECIFIER_SIGNED:
        case TYPE_SPECIFIER_INT | TYPE_SPECIFIER_SIGNED:
            return TYPE_S_INT;
        
        case TYPE_SPECIFIER_UNSIGNED:
        case TYPE_SPECIFIER_INT | TYPE_SPECIFIER_UNSIGNED:
            return TYPE_U_INT;

        case TYPE_SPECIFIER_LONG:
        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_SIGNED:
        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_INT:
        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_SIGNED | TYPE_SPECIFIER_INT:
            return TYPE_S_LONG;

        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_UNSIGNED:
        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_UNSIGNED | TYPE_SPECIFIER_INT:
            return TYPE_U_LONG;

        // NOTE: for both ll and ull types we need the long part at the end
        // since we will need to give accurate error messages too!
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_LONG:
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_SIGNED | TYPE_SPECIFIER_LONG:
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_INT | TYPE_SPECIFIER_LONG:
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_INT | TYPE_SPECIFIER_SIGNED
                | TYPE_SPECIFIER_LONG:
            return TYPE_S_LONG_LONG;

        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_UNSIGNED | TYPE_SPECIFIER_LONG:
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_UNSIGNED
                | TYPE_SPECIFIER_INT | TYPE_SPECIFIER_LONG:
            return TYPE_U_LONG_LONG;

        case TYPE_SPECIFIER_FLOAT:
            return TYPE_FLOAT;

        case TYPE_SPECIFIER_DOUBLE:
            return TYPE_DOUBLE;

        case TYPE_SPECIFIER_LONG | TYPE_SPECIFIER_DOUBLE:
            return TYPE_LONG_DOUBLE;

        case TYPE_SPECIFIER_BOOL:
            return TYPE_BOOL;

        default:
            // TODO: basically unlimited ammount of error messages we could
            // create but lets not do that just yet
            diag_error("invalid type specifier combination");

            return TYPE_ERROR;
    }
}

static Type* type_from_declaration_specifiers(Parser* parser, TypeSpecifier specifiers)
{
    TypeKind kind = determine_type_kind(specifiers);
    
    // TODO: we will need to get the type fro mthe typekind

    return NULL;
}

static bool has_declaration_specifier(Parser* parser, const Token* tok)
{
    return has_match(parser, storage_class, storage_class_count)
            || has_match(parser, type_qualifier, type_qualifier_count)
            || has_match(parser, function_specificer, function_specificer_count)
            || is_typename_start(parser, tok);
}

static DeclarationSpecifiers parse_declaration_specifiers(Parser* parser)
{
    // Variables to help us build the declaration specifiers
    TypeStorageSpecifier storage_spec = TYPE_STORAGE_SPECIFIER_NONE;
    TypeQualifiers qualifiers = TYPE_QUALIFIER_NONE;
    TypeFunctionSpecifier function_spec = TYPE_FUNCTION_SPECIFIER_NONE;
    TypeSpecifier type_spec = TYPE_SPECIFIER_NONE;

    // This is the final type we will use
    Type* type = NULL;

    // TODO: should this be converted into a switch to be faster?
    while (has_declaration_specifier(parser, current_token(parser)))
    {
        const Token* token = current_token(parser);

        if (has_match(parser, storage_class, storage_class_count))
        {
            TypeStorageSpecifier new_spec = parse_storage_class_specifier(parser);

            if (storage_spec != TYPE_STORAGE_SPECIFIER_NONE)
            {
                diag_error("already have a storage specifier in declaration");

                continue;
            }
            
            storage_spec = new_spec;

            continue;
        }
        else if (has_match(parser, type_qualifier, type_qualifier_count))
        {
            TypeQualifiers new_qualifier = parse_type_qualifier(parser);

            if (type_qualifier_already_has(qualifiers, new_qualifier))
            {
                diag_warning("already have qualifier got a double up");
            }

            qualifiers |= new_qualifier;

            continue;
        }
        else if (has_match(parser, function_specificer, function_specificer_count))
        {
            TypeFunctionSpecifier new_spec =  parse_function_specificer(parser);

            if (function_spec != TYPE_FUNCTION_SPECIFIER_NONE)
            {
                diag_warning("already have 'inline' specifier");

                continue;
            }

            function_spec = TYPE_FUNCTION_SPECIFIER_INLINE;
            
            continue;
        }
        else if (is_typename_start(parser, token))
        {
            // If we have a enum, struct, or union we will want to parse those
            // sepereately. Otherwise we will want to get the 
            if (has_match(parser, (TokenType[]) {TOKEN_UNION, TOKEN_STRUCT}, 2))
            {
                // Parse a compound type here
                parse_struct_or_union_specifier(parser);
            }
            else if (is_match(parser, TOKEN_ENUM))
            {
                parse_enum_specificer(parser);
            }
            else if (is_match(parser, TOKEN_IDENTIFIER))
            {
                // TODO: here we will eventually want some system to look ahead
                // here and check maybe if were done or not if we don't get
                // a typedef name

                // TODO: also adding a check here for if we already have 
                // specifiers or not...

                panic("currently unreachable");
            }
            else
            {
                TypeSpecifier new_spec = parse_type_specifier(parser);

                // Here we we have a type specifier and it isn't a long specifier
                if (type_specifier_has(type_spec, new_spec)
                        && (new_spec != TYPE_SPECIFIER_LONG))
                {
                    diag_error("already recieved type specifier");

                    continue;
                }
                else if (new_spec == TYPE_SPECIFIER_LONG)
                {
                    // Check for 2 cases:
                    // 1. we don't have it so just add it
                    // 2. We already have the specifier long to upgrade it to
                    //    long long
                    // 3. we have long long already so make an error
                    if (!(type_spec & TYPE_SPECIFIER_LONG))
                    {
                        type_spec |= new_spec;
                    }
                    else if (type_spec & TYPE_SPECIFIER_LONG 
                            && !(type_spec & TYPE_SPECIFIER_LONG_LONG))
                    {
                        type_spec |= TYPE_SPECIFIER_LONG_LONG;
                    }
                    else
                    {
                        assert(type_specifier_has(type_spec, TYPE_SPECIFIER_LONG_LONG));

                        diag_error("long long long is was too long!");
                    }

                    continue;
                }

                // Otherwise just add the specifier and continue
                type_spec |= new_spec;

                continue;
            }
        }
        else
        {
            panic("unreachable");
        }
    }

    // TODO: here if we have no type yet but have type specifiers we will need
    // to deal with that and determine the type
    if (!type && (type_spec != TYPE_SPECIFIER_NONE))
    {
        type = type_from_declaration_specifiers(parser, type_spec);
    }
    else if (!type && (type_spec == TYPE_SPECIFIER_NONE))
    {
        diag_error("not type specifiers; not assuming int just yet");
    }
    // TODO: add check here for if we have specifiers AND a type...

    // Here we finally build and return our declaration specifiers
    DeclarationSpecifiers specifiers = 
    {
        .function_spec = function_spec,
        .storage_spec = storage_spec,
        .qualifiers = qualifiers,
        .type = type
    };

    return specifiers;
}

static Declaration* parse_declaration(Parser* parser)
{
    // First we need to get our declaration specifiers here
    const DeclarationSpecifiers decl_spec = parse_declaration_specifiers(parser);

    if (!is_match(parser, TOKEN_SEMI))
    {
        parse_init_declarator_list(parser);
    }
    else
    {
        // parse_error(parser, "declaration does not declare anything");
        // TODO: maybe a warning about how we didn't really declare anything
    }

    return NULL;
}

static Declaration* parse_type_name(Parser* parser)
{
    Declaration* decl = parse_specifier_qualifier_list(parser);
    // TODO: parse abstract declarator if needed

    if (has_match(parser, 
            (TokenType[]) {TOKEN_STAR, TOKEN_LPAREN, TOKEN_LBRACKET}, 3))
    {
        parse_abstract_declarator(parser);
    }

    return NULL;
}

// The definitions of the functions we will use for pasing

static Declaration* parse_top_level_declaration_or_definition(Parser* parser)
{
    Declaration* decl = parse_declaration(parser);

    // TODO: we eventually want something like this
    // if (decl->base.declaration_type == DECLARATION_FUNCTION &&
    //         is_match(parser, TOKEN_LCURLY))
    // {
    //     Statement* body = parse_compound_statement(parser);

    //     decl->function.function_body = body;
    // }
    // else
    // {
    //     match(parser, TOKEN_SEMI);
    // }

    // return decl;

    if (is_match(parser, TOKEN_SEMI))
    {
        consume(parser);
    }
    else if (is_match(parser, TOKEN_LCURLY))
    {
        Statement* body = parse_compound_statement(parser);
    }
    else
    {
        match(parser, TOKEN_EOF);

        panic("expected ';' or '{'");
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
}

void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};
    parser.ast.ast_allocator = ast_allocator_create();
    parser.ast.top_level_decls = declaration_vector_create(1);

    parse_translation_unit_internal(&parser);

    return;
}
