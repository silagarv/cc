#include "parser.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "files/location.h"
#include "util/panic.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "files/line_map.h"

#include "lex/token.h"

#include "parse/literal_parser.h"
#include "parse/type.h"
#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"

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
static void add_recover_tokens(Parser* parser, const TokenType* types, size_t count);
static void remove_recover_tokens(Parser* parser, const TokenType* types, size_t count);

static bool is_in_recover_set(Parser* parser, TokenType type);

// Parser methods for getting the current and next token. We aim to be a LL(1)
// like parser so we should only ever need these in order to figure own the
// current production.
static Token* current_token(Parser* parser);
static Token* next_token(Parser* parser);

static TokenType current_token_type(Parser* parser);
static TokenType next_token_type(Parser* parser);

// Methods for mathing a token type or unconditionally consuming a token.
static void match(Parser* parser, TokenType type);
static void consume(Parser* parser);

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

// Methods for mathing a token type or unconditionally consuming a token.
static void match(Parser* parser, TokenType type)
{
    if (token_stream_current_type(parser->stream) == type)
    {
        token_stream_advance(parser->stream);

        return;
    }

    parse_error(parser, "expected '%s' but got '%s'", 
            token_type_get_name(type), 
            token_type_get_name(current_token_type(parser))
        );

    panic("failed to match token... TODO: finish this method");
}

static void consume(Parser* parser)
{
    token_stream_advance(parser->stream);
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
static Declaration* parse_declarator(Parser* parser);
static Declaration* parse_direct_declarator(Parser* parser);
static Declaration* parse_direct_abstract_declarator(Parser* parser);
static Declaration* parse_abstract_declarator(Parser* parser);
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

static void* parse_type_name(Parser* parser);

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
            // TODO: we will need to look up if the symbol exists as a typename


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
            return true; // TODO: fix once we have a proper symbol table
        
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
    TokenType current_type = current_token_type(parser);
    switch (current_type)
    {
        case TOKEN_LPAREN: 
        {
            consume(parser);

            Expression* expr = parse_expression(parser);

            match(parser, TOKEN_RPAREN);

            return expr;
        }
        
        case TOKEN_IDENTIFIER:
        {
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

                expr = create_error_expression(current);
            }
            else
            {
                expr = create_integer_expression(current, &value);
            }

            return expr;
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

                expr = create_error_expression(current);
            }
            else
            {
                expr = create_string_expression(current, &string);
            }

            return expr;
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

                expr = create_error_expression(current);
            }
            else
            {
                expr = create_character_expression(current, &value);
            }

            return expr;
        }

        default:
            diag_error("expected expression");

            return create_error_expression(current);
    }
}

static Expression* parse_postfix_expression(Parser* parser)
{
    /* For postfix expression we ignore the following rule:
     * ( type-name ) { initializer-list } 
     * this rule is actually handled in the function parse_cast_expression. See
     * that function for details
     */

    Expression* primary_expr = parse_primary_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS}, 6))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_LBRACKET:
                consume(parser);
                parse_expression(parser);
                match(parser, TOKEN_RBRACKET);
                break;

            case TOKEN_LPAREN:
                consume(parser);
                if (!is_match(parser, TOKEN_RPAREN))
                {
                    parse_argument_expression_list(parser);
                }
                match(parser, TOKEN_RPAREN);
                break;

            case TOKEN_DOT:
                consume(parser);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_ARROW:
                consume(parser);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_PLUS_PLUS:
                consume(parser);
                break;

            case TOKEN_MINUS_MINUS:
                consume(parser);
                break;

            default:
                panic("unreachable");
                break;
        }
    }

    return NULL;
}

static Expression* parse_argument_expression_list(Parser* parser)
{
    parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        consume(parser);
        parse_assignment_expression(parser);
    }

    return NULL;
}

static Expression* parse_unary_expression(Parser* parser)
{
    switch (current_token_type(parser))
    {
        case TOKEN_PLUS_PLUS:
            consume(parser);
            parse_unary_expression(parser);
            break;

        case TOKEN_MINUS_MINUS:
            consume(parser);
            parse_unary_expression(parser);
            break;

        case TOKEN_AND:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_STAR:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_PLUS:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_MINUS:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_TILDE:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_NOT:
            consume(parser);
            parse_cast_expression(parser);
            break;

        case TOKEN_SIZEOF:
            consume(parser);
            if (is_match(parser, TOKEN_LPAREN) && 
                    is_typename_start(parser, next_token(parser)))
            {
                consume(parser);
                parse_type_name(parser);
                match(parser, TOKEN_RPAREN);
            }
            else
            {
                parse_unary_expression(parser);
            }
            break;

        default:
            parse_postfix_expression(parser);
            break;
    }

    return NULL;
}

static Expression* parse_cast_expression(Parser* parser)
{
    /* ( type-name ) cast-expression */
    while (is_match(parser, TOKEN_LPAREN) && 
            is_typename_start(parser, next_token(parser)))
    {
        consume(parser);

        Type* type = parse_type_name(parser);
        
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
    parse_cast_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT}, 3))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_STAR: 
                consume(parser); 
                break;
            
            case TOKEN_SLASH: 
                consume(parser); 
                break;
            
            case TOKEN_PERCENT: 
                consume(parser); 
                break;

            default:
                panic("unreachable");
                break;
        }

        parse_cast_expression(parser);
    }

    return NULL;
}

static Expression* parse_additive_expression(Parser* parser)
{
    static const TokenType additive_operators[] = {TOKEN_PLUS, TOKEN_MINUS};
    static const size_t num_operators = countof(additive_operators);

    add_recover_tokens(parser, additive_operators, num_operators);

    Expression* expr = parse_multiplicative_expression(parser);

    while (has_match(parser, additive_operators, num_operators))
    {
        /* Need to create a new binary expression and allocate it that way and
         * then set its type to match the corrosponding operator
         */
        
        ExpressionType type = EXPRESSION_ERROR;
        switch (current_token_type(parser))
        {
            case TOKEN_PLUS: 
                consume(parser);
                type = EXPRESSION_BINARY_ADD; 
                break;

            case TOKEN_MINUS: 
                consume(parser);
                type = EXPRESSION_BINARY_SUBTRACT; 
                break;

            default: 
                panic("unreachable"); 
                break;
        }

        Expression* rhs = parse_multiplicative_expression(parser);

        // TODO: get new binary expression tmp with type type
        Expression* tmp = NULL; 
        // TODO: assign tmp's lhs to expr, and tmp's rhs to rhs
        
        expr = tmp;

        // To stop warning about unused for now
        (void) type;
    }

    remove_recover_tokens(parser, additive_operators, num_operators);

    return expr;
}

static Expression* parse_shift_expression(Parser* parser)
{
    parse_additive_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LT_LT, TOKEN_GT_GT}, 2))
    {
        switch (current_token_type(parser)) 
        {
            case TOKEN_LT_LT: 
                consume(parser); 
                break;
            
            case TOKEN_GT_GT: 
                consume(parser); 
                break;

            default:
                panic("unreachable");
                break;
        }
        
        parse_additive_expression(parser);
    }

    return NULL;
}

static Expression* parse_relational_expression(Parser* parser)
{
    parse_shift_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LT, TOKEN_GT, TOKEN_LT_EQUAL, TOKEN_GT_EQUAL}, 4))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_LT: 
                consume(parser); 
                break;
            
            case TOKEN_GT: 
                consume(parser); 
                break;
            
            case TOKEN_LT_EQUAL: 
                consume(parser); 
                break;
            
            case TOKEN_GT_EQUAL: 
                consume(parser); 
                break;

            default:
                panic("unreachable");
                break;
        }

        parse_shift_expression(parser);
    }

    return NULL;
}

static Expression* parse_equality_expression(Parser* parser)
{
    parse_relational_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL}, 2))
    {
        switch (current_token_type(parser))
        {
            case TOKEN_EQUAL_EQUAL: 
                consume(parser);
                break;
            
            case TOKEN_NOT_EQUAL: 
                consume(parser);
                break;

            default:
                panic("unreachable");
                break;
        }

        parse_relational_expression(parser);
    }

    return NULL;
}

static Expression* parse_and_expression(Parser* parser)
{
    parse_equality_expression(parser);

    while (is_match(parser, TOKEN_AND))
    {
        match(parser, TOKEN_AND);

        parse_equality_expression(parser);
    }

    return NULL;
}

static Expression* parse_exclusive_or_expression(Parser* parser)
{
    parse_and_expression(parser);

    while (is_match(parser, TOKEN_XOR))
    {
        consume(parser);

        parse_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_inclusive_or_expression(Parser* parser)
{
    parse_exclusive_or_expression(parser);

    while (is_match(parser, TOKEN_OR))
    {
        consume(parser);

        parse_exclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_and_expression(Parser* parser)
{
    parse_inclusive_or_expression(parser);

    while (is_match(parser, TOKEN_AND_AND))
    {
        consume(parser);

        parse_inclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_or_expression(Parser* parser)
{
    parse_logical_and_expression(parser);

    while (is_match(parser, TOKEN_OR_OR))
    {
        consume(parser);

        parse_logical_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_conditional_expression(Parser* parser)
{
    Expression* expr = parse_logical_or_expression(parser);

    if (is_match(parser, TOKEN_QUESTION))
    {
        consume(parser);

        parse_expression(parser);

        match(parser, TOKEN_COLON);

        parse_conditional_expression(parser);
    }

    return NULL;
}

static Expression* parse_assignment_expression(Parser* parser)
{
    static const TokenType assignment_operators[] = {
        TOKEN_EQUAL, TOKEN_STAR_EQUAL, TOKEN_SLASH_EQUAL, TOKEN_PERCENT_EQUAL,
        TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_LT_LT_EQUAL, TOKEN_GT_GT_EQUAL,
        TOKEN_AND_EQUAL, TOKEN_XOR_EQUAL, TOKEN_OR_EQUAL
    };
    static const size_t num_operators = 
            sizeof(assignment_operators) / sizeof(assignment_operators[0]);

    // Here as an extension gcc does this and then just checks the conditional
    // is valid. I'm not sure how this qualifies as an extension since this
    // honestly just seems like an easier way to write a parser so I will do
    // the same thing.
    Expression* expr = parse_conditional_expression(parser);

    if (has_match(parser, assignment_operators, num_operators))
    {
        // TODO: will need to make sure that we actually somehow get the token
        // type before simply consuming the token
        consume(parser);
        
        parse_assignment_expression(parser);
    }

    return NULL;
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
        consume(parser);

        Expression* rhs = parse_assignment_expression(parser);

        // TODO: build the expression here
    }

    remove_recover_token(parser, TOKEN_COMMA);

    return expr;
}

// For parsing statements

// Allocate and setup the current statement
static Statement* statement_allocate(Parser* parser, StatementType type)
{
    Statement* stmt = xmalloc(sizeof(Statement));

    stmt->base.type = type;
    stmt->base.loc = current_token(parser)->loc;
    stmt->base.parent = NULL; // TODO: change this from NULL to proper stmt

    return stmt;
}

static void statement_free(Statement* stmt)
{
    free(stmt);
}

static Statement* parse_label_statement(Parser* parser)
{
    Statement* stmt = statement_allocate(parser, STATEMENT_LABEL);

    Token* tok = current_token(parser);

    // First consume identifier then the colon...
    consume(parser);
    consume(parser);

    // Somehow we want to get the name from the token
    stmt->label_stmt.name = (String) {0};
    stmt->label_stmt.statement = parse_statement(parser);

    statement_free(stmt);

    return NULL;
}

static Statement* parse_case_statement(Parser* parser)
{
    Statement* current_switch = parser->current_context.current_switch;
    if (!current_switch)
    {
        parse_error(parser, "No current switch statement context");
    }
    
    Statement* stmt = statement_allocate(parser, STATEMENT_CASE);

    consume(parser);

    // TODO: somewhere we will need to parse the constant expression and then
    // TODO: eventually fold it somewhere
    
    // Get the experssion and the statment
    stmt->case_stmt.constant_expression = parse_constant_expression(parser);
    
    // Get the colon and the rest of the statment
    match(parser, TOKEN_COLON);

    stmt->case_stmt.statement = parse_statement(parser);

    statement_free(stmt);

    return NULL;
}

static Statement* parse_default_statement(Parser* parser)
{
    Statement* current_switch = parser->current_context.current_switch;
    if (!current_switch)
    {
        parse_error(parser, "No current switch statement context");
    }

    Statement* stmt = statement_allocate(parser, STATEMENT_CASE);

    consume(parser);
    match(parser, TOKEN_COLON);

    stmt->default_stmt.statement = parse_statement(parser);

    statement_free(stmt);

    return NULL;
}

static Statement* parse_compound_statement(Parser* parser)
{
    match(parser, TOKEN_LCURLY);

    while (!is_match(parser, TOKEN_RCURLY))
    {
        parse_statement(parser);
    }

    match(parser, TOKEN_RCURLY);

    return NULL;
}

static Statement* parse_expression_statement(Parser* parser)
{
    // Need some way to check if we can start and expression
    // like idk might need some kind of expression start set

    if (is_match(parser, TOKEN_SEMI))
    {
        consume(parser);
    }
    else
    {
        parse_expression(parser);
        match(parser, TOKEN_SEMI);
    }

    return NULL;
}

static Statement* parse_if_statement(Parser* parser)
{
    const Token* if_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    match(parser, TOKEN_RPAREN);

    Statement* if_body = parse_statement(parser);

    if (is_match(parser, TOKEN_ELSE))
    {
        consume(parser);
            
        Statement* else_body = parse_statement(parser);
    }

    // TODO: build the if statement fully here

    return NULL;
}

static Statement* parse_switch_statement(Parser* parser)
{
    // Save the ast context for later
    Statement* current_breakable = parser->current_context.current_breakable;
    Statement* current_switch = parser->current_context.current_switch;

    const Token* switch_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_LPAREN);

    Expression* expr = parse_expression(parser);

    match(parser, TOKEN_RPAREN);

    // Create the switch statement and set the current breakable to be the
    // switch statement.
    Statement* switch_stmt = NULL;
    parser->current_context.current_breakable = switch_stmt;
    parser->current_context.current_switch = switch_stmt;

    Statement* body = parse_statement(parser);

    // Restore the ast context
    parser->current_context.current_breakable = current_breakable;
    parser->current_context.current_switch = current_switch;

    return NULL;
}

static Statement* parse_while_statement(Parser* parser)
{
    // Save the context... we will restore this later. Save both since we can
    // both break and continue within a while statement.
    Statement* current_iteration = parser->current_context.current_iteration;
    Statement* current_breakable = parser->current_context.current_breakable;

    const Token* while_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    match(parser, TOKEN_RPAREN);

    // TODO: create the statement and push it
    Statement* this_stmt = NULL;
    parser->current_context.current_iteration = this_stmt;
    parser->current_context.current_breakable = this_stmt;

    Statement* body = parse_statement(parser);

    // Restore the ast context
    parser->current_context.current_iteration = current_iteration;
    parser->current_context.current_breakable = current_breakable;

    return NULL;
}

static Statement* parse_do_while_statement(Parser* parser)
{
    // Save the context... we will restore this later. Save both since we can
    // both break and continue within a while statement.
    Statement* current_iteration = parser->current_context.current_iteration;
    Statement* current_breakable = parser->current_context.current_breakable;

    const Token* do_token = current_token(parser);
    consume(parser);

    // TODO: create the statement and push it
    Statement* this_stmt = NULL;
    parser->current_context.current_iteration = this_stmt;
    parser->current_context.current_breakable = this_stmt;

    Statement* body = parse_statement(parser);

     // Restore the ast context
    parser->current_context.current_iteration = current_iteration;
    parser->current_context.current_breakable = current_breakable;

    // make sure we capture the while part.
    match(parser, TOKEN_WHILE);

    match(parser, TOKEN_LPAREN);

    Expression* cond = parse_expression(parser);

    match(parser, TOKEN_RPAREN);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Statement* parse_for_statement(Parser* parser)
{
    const Token* for_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_LPAREN);

    // Below if where it gets a little tricky :)
    if (!is_match(parser, TOKEN_SEMI))
    {
        if (is_typename_start(parser, current_token(parser)))
        {
            // TODO: how do I deal with this?
            parse_declaration(parser);
            
            // MASSIVE TODO: there is a but while parsing a declaration where
            // we do not eat a semi-colon and this needs to be fixed for this
            // to work properly but I'm not sure where this is just yet :)
            // goto skip_semi;
        }
        else
        {
            Expression* expr1 = parse_expression(parser);
        }
    }

    match(parser, TOKEN_SEMI);

// skip_semi:
    if (!is_match(parser, TOKEN_SEMI))
    {
        Expression* expr2 = parse_expression(parser);
    }

    match(parser, TOKEN_SEMI);

    if (!is_match(parser, TOKEN_RPAREN))
    {
        Expression* expr3 = parse_expression(parser);
    }

    match(parser, TOKEN_RPAREN);

    return NULL;
}

static Statement* parse_goto_statement(Parser* parser)
{
    const Token* goto_token = current_token(parser);
    consume(parser);

    const Token* label_name = current_token(parser);
    match(parser, TOKEN_IDENTIFIER);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Statement* parse_continue_statement(Parser* parser)
{
    Statement* current_iteration = parser->current_context.current_iteration;
    if (!current_iteration)
    {
        parse_error(parser, "No current continuable exists");
    }

    const Token* continue_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Statement* parse_break_statement(Parser* parser)
{
    Statement* current_breakable = parser->current_context.current_breakable;
    if (!current_breakable)
    {
        parse_error(parser, "No current breakable statement exists");
    }

    const Token* break_token = current_token(parser);
    consume(parser);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Statement* parser_return_statement(Parser* parser)
{
    void* current_function = parser->current_context.current_function;
    if (!current_function)
    {
        parse_error(parser, "Return statement outside of function");
    }

    const Token* return_token = current_token(parser);
    consume(parser);

    if (!is_match(parser, TOKEN_SEMI))
    {
        Expression* expr = parse_expression(parser);
    }

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Statement* parse_statement(Parser* parser)
{
    Statement* stmt;
    switch(current_token_type(parser))
    {
        // Here we are specifically looking for a label
        // e.g. fail: ...
        // TODO: we will also eventually need to check if the identifier might
        // TODO: be a type name or not i think
        case TOKEN_IDENTIFIER:
            if (next_token_type(parser) != TOKEN_COLON)
            {
                goto expression_statement;
            }
            stmt = parse_label_statement(parser);
            break;

        case TOKEN_CASE:
            stmt = parse_case_statement(parser);
            break;

        case TOKEN_DEFAULT:
            stmt = parse_default_statement(parser);
            break;

        case TOKEN_LCURLY:
            stmt = parse_compound_statement(parser);
            break;
        
        /* note that if we get a ';' we're just going to match an empty 
         * expression statement so this can easily just be reduced to match a
         * semi colon later but will leave this here for now
         */
        case TOKEN_SEMI:
            stmt = parse_expression_statement(parser);
            break;

        case TOKEN_IF:
            stmt = parse_if_statement(parser);
            break;

        case TOKEN_SWITCH:
            stmt = parse_switch_statement(parser);
            break;

        case TOKEN_WHILE:
            stmt = parse_while_statement(parser);
            break;

        case TOKEN_DO:
            stmt = parse_do_while_statement(parser);
            break;

        case TOKEN_FOR:
            stmt = parse_for_statement(parser);
            break;

        case TOKEN_GOTO:
            stmt = parse_goto_statement(parser);
            break;

        case TOKEN_CONTINUE:
            stmt = parse_continue_statement(parser);
            break;

        case TOKEN_BREAK:
            stmt = parse_break_statement(parser);
            break;

        case TOKEN_RETURN:
            stmt = parser_return_statement(parser);
            break;

        default:
        expression_statement:
            if (is_expression_start(parser, current_token(parser)))
            {
                stmt = parse_expression_statement(parser);    
            }
            else if (has_declaration_specifier(parser, current_token(parser)))
            {
                parse_declaration(parser);

                stmt = NULL;
            }
            else
            {
                // TODO: make an error statement here?
                stmt = NULL;

                match(parser, TOKEN_EOF);

                panic("bad statement start");
            }

            break;
    }

    return stmt;
}

// Below is things for declarations

static Declaration* parse_designation(Parser* parser)
{
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
    // TODO: fix this later
    match(parser, TOKEN_IDENTIFIER);
 
    return NULL;
}

static Declaration* parse_enumerator(Parser* parser)
{
    match(parser, TOKEN_IDENTIFIER);
    if (is_match(parser, TOKEN_EQUAL))
    {
        consume(parser);
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
    match(parser, TOKEN_ENUM);

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);

        if (!is_match(parser, TOKEN_LCURLY))
        {
            return NULL;
        }
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
        case TOKEN_IDENTIFIER:
            consume(parser);
            break;
        case TOKEN_LPAREN:
            consume(parser);
            parse_declarator(parser);
            match(parser, TOKEN_RPAREN);
            break;

        default:
            panic("bad parse_direct_declarator");
            break;
    }

    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
    {
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
    if (is_match(parser, TOKEN_LPAREN))
    {
        match(parser, TOKEN_LPAREN);

        if (is_match(parser, TOKEN_RPAREN))
        {
            match(parser, TOKEN_RPAREN);
        }
        else if (is_typename_start(parser, current_token(parser)))
        {
            parse_paramater_type_list(parser);

            match(parser, TOKEN_RPAREN);
        }
        else
        {
            parse_abstract_declarator(parser);

            match(parser, TOKEN_RPAREN);
        }
    }
    else if (is_match(parser, TOKEN_LBRACKET))
    {
        match(parser, TOKEN_LBRACKET);

        if (is_match(parser, TOKEN_RBRACKET))
        {
            match(parser, TOKEN_RBRACKET);
        }
        else
        {
            parse_constant_expression(parser);
        }
    }
    else
    {
        panic("unexpected token type...");
    }

    // TODO: make this code better

    // Now parse the rest of it
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_LPAREN}, 2))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            match(parser, TOKEN_LPAREN);

            if (is_match(parser, TOKEN_RPAREN))
            {
                match(parser, TOKEN_RPAREN);
            }
            else if (is_typename_start(parser, current_token(parser)))
            {
                parse_paramater_type_list(parser);

                match(parser, TOKEN_RPAREN);
            }
        }

        if (is_match(parser, TOKEN_LBRACKET))
        {
            match(parser, TOKEN_LBRACKET);

            if (is_match(parser, TOKEN_RBRACKET))
            {
                match(parser, TOKEN_RBRACKET);
            }
            else
            {
                assert(is_expression_start(parser, current_token(parser)));

                parse_constant_expression(parser);
            }
        }
        else
        {
            panic("unreachale");
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
    else
    {
        parse_direct_abstract_declarator(parser);
    }

    return NULL;
}

// TODO: eliminate recursion here
static Declaration* parse_pointer(Parser* parser)
{
    match(parser, TOKEN_STAR);

    while (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }

    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }
 
    return NULL;
}

static Declaration* parse_identifier_list(Parser* parser)
{
    match(parser, TOKEN_LPAREN);
    match(parser, TOKEN_IDENTIFIER);
    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        match(parser, TOKEN_IDENTIFIER);
    }

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
    parse_paramater_list(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        match(parser, TOKEN_ELIPSIS);
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

static const TokenType builtin_type_tokens[] =
{
    TOKEN_VOID,
    TOKEN_CHAR,
    TOKEN_SHORT,
    TOKEN_INT,
    TOKEN_LONG,
    TOKEN_FLOAT,
    TOKEN_DOUBLE,
    TOKEN_SIGNED,
    TOKEN_UNSIGNED,
    TOKEN__BOOL,
    TOKEN__COMPLEX,
    TOKEN__IMAGINARY
};

const size_t builtin_type_tokens_size = countof(builtin_type_tokens);

static bool is_builtin_type_token(Parser* parser)
{
    return has_match(parser, builtin_type_tokens, builtin_type_tokens_size);
}

static Type* parse_builtin_type(Parser* parser)
{
    TokenType type_unsigned;

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
        case TYPE_SPECIFIER_LONG_LONG | TYPE_SPECIFIER_UNSIGNED | TYPE_SPECIFIER_INT
                | TYPE_SPECIFIER_LONG:
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
                diag_error("already have 'inline' specifier");

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

        // TODO: make type int
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
        // TODO: maybe a warning about how we didn't really declare anything
    }

    return NULL;
}

static Declaration* parse_declaration_or_definition(Parser* parser)
{
    Declaration* decl = parse_declaration(parser);

    /* TODO: add declaration to list of declarations */

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

static void* parse_type_name(Parser* parser)
{
    Declaration* decl = parse_specifier_qualifier_list(parser);
    // TODO: parse abstract declarator if needed

    return NULL;
}

// The definitions of the functions we will use for pasing


void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};

    // add so we can never go past
    add_recover_token(&parser, TOKEN_EOF);

    while (current_token_type(&parser) != TOKEN_EOF)
    {
        parse_declaration_or_definition(&parser);
    }

    remove_recover_token(&parser, TOKEN_EOF);

    return;
}
