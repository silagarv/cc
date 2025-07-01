#include "parser.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "parse/expression.h"
#include "util/panic.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/statement.h"

static const TokenType storage_classes[] = 
{
    TOKEN_EXTERN,
    TOKEN_TYPEDEF,
    TOKEN_STATIC,
    TOKEN_AUTO,
    TOKEN_REGISTER
};

static const TokenType type_qualifiers[] =
{
    TOKEN_CONST,
    TOKEN_RESTRICT,
    TOKEN_VOLATILE,
    TOKEN_INLINE
};

static const TokenType statement_start_set[] = 
{
    TOKEN_FOR
};

static const size_t statement_start_set_size = 
        sizeof(statement_start_set) / sizeof(statement_start_set[0]);

static bool is_valid_stream_position(TokenStream* stream)
{
    return (stream->current_token < stream->count);
}

// Below are basic token matching functions for us to use while parsing

static TokenType curr_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token].type;
}

static TokenType next_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token + 1].type;
}

static TokenType next_next_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token + 2].type;
}

static Token* curr(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return &stream->tokens[stream->current_token];
}

static Token* next(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return &stream->tokens[stream->current_token + 1];
}

static void consume(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    // Make sure we only eat tokens when it is okay to do so, otherwise we might
    // go over how many we can actually eat
    if (stream->current_token < stream->count)
    {
        stream->current_token++;
    }
}

static void parse_error(Parser* parser, const char* fmt, ...)
{
    Token* tok = curr(parser->stream);   

    ResolvedLocation loc = line_map_resolve_location(parser->map, tok->loc);
    fprintf(stderr, "%s:%u:%u\n", loc.name->path, loc.line, loc.col);
    va_list args;
    va_start(args, fmt);
    diag_verror(fmt, args);
    va_end(args);
}

static bool match(Parser* parser, TokenType type)
{
    TokenStream* stream = parser->stream;

    if (curr_type(stream) == type)
    {
        consume(stream);

        return true;
    }

    parse_error(parser, "expected '%s' but got '%s'", 
            token_type_get_name(type), 
            token_type_get_name(curr_type(stream))
        );

    panic("error");

    return false;
}

static bool is_match(Parser* parser, TokenType type)
{
    return (curr_type(parser->stream) == type);
}

static bool has_match(Parser* parser, const TokenType* types, size_t count)
{
    const TokenType current = curr_type(parser->stream);

    for (size_t i = 0; i < count; i++)
    {
        if (current == types[i])
        {
            return true;
        }
    }

    return false;
}

// Functions to help with error recovery eventually we will have more 
// intelligent error recovery functions but this will do for now...
static void consume_until_and(Parser* parser, TokenType type)
{   
    TokenStream* stream = parser->stream;

    while (curr_type(stream) != type)
    {
        if (curr_type(stream) == TOKEN_EOF)
        {
            return;
        }

        consume(stream);
    }

    consume(stream);
}

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_integer_constant(Parser* parser);
static Expression* parse_floating_constant(Parser* parser);
static Expression* parse_enumeration_constant(Parser* parser);
static Expression* parse_character_constant(Parser* parser);
static Expression* parse_constant(Parser* parser);

// All of our functions for parsing expressions
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
static Statement* parse_labeled_statement(Parser* parser);
static Statement* parse_compound_statement(Parser* parser);
static Statement* parse_expression_statement(Parser* parser);
static Statement* parse_selection_statement(Parser* parser);
static Statement* parse_iteration_statement(Parser* parser);
static Statement* parse_jump_statement(Parser* parser);
static Statement* parse_statement(Parser* parser);

// All of our functions for parsing declarations / definitions



// The definitions of the functions we will use for pasing

// Some functions for creating errors


















void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};

    while (curr_type(stream) != TOKEN_EOF)
    {
        parse_expression(&parser);
        match(&parser, TOKEN_SEMI);
    }

    return;
}

static Expression* parse_integer_constant(Parser *parser);
static Expression* parse_floating_constant(Parser *parser);
static Expression* parse_enumeration_constant(Parser *parser);
static Expression* parse_character_constant(Parser *parser);

static Expression* parse_constant(Parser *parser)
{
    switch (curr_type(parser->stream))
    {


        default:
            panic("not a valid constant type");

    }

    return NULL;
}

static Expression* parse_primary_expression(Parser* parser)
{
    /* for now just match numbers */
    if (is_match(parser, TOKEN_LPAREN))
    {
        match(parser, TOKEN_LPAREN);

        parse_expression(parser);

        match(parser, TOKEN_RPAREN);
    }
    else if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);
    }
    else if (is_match(parser, TOKEN_NUMBER))
    {
        match(parser, TOKEN_NUMBER);
    }
    else
    {
        panic("parse_primary_expression");
    }

    return NULL;
}

static Expression* parse_postfix_expression(Parser* parser)
{
    /* for now we will ignore the typename followed by initialiser list */
    /* ( type-name ) { initializer-list } */
    /* in fact for now we will just parse a primary expression as this would 
     * be quite a bit of effort to implement at the moment
     */

    parse_primary_expression(parser);
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS}, 6))
    {
        switch (curr_type(parser->stream))
        {
            case TOKEN_LBRACKET:
                match(parser, TOKEN_LBRACKET);
                parse_expression(parser);
                match(parser, TOKEN_RBRACKET);
                break;

            case TOKEN_LPAREN:
                match(parser, TOKEN_LPAREN);
                if (curr_type(parser->stream) != TOKEN_RPAREN)
                {
                    parse_argument_expression_list(parser);
                }
                match(parser, TOKEN_RPAREN);
                break;

            case TOKEN_DOT:
                match(parser, TOKEN_DOT);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_ARROW:
                match(parser, TOKEN_ARROW);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_PLUS_PLUS:
                match(parser, TOKEN_PLUS_PLUS);
                break;

            case TOKEN_MINUS_MINUS:
                match(parser, TOKEN_MINUS_MINUS);
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
        match(parser, TOKEN_COMMA);
        parse_assignment_expression(parser);
    }

    return NULL;
}

static Expression* parse_unary_expression(Parser* parser)
{
    switch (curr_type(parser->stream))
    {
        case TOKEN_PLUS_PLUS:
            match(parser, TOKEN_PLUS_PLUS);
            parse_unary_expression(parser);
            break;

        case TOKEN_MINUS_MINUS:
            match(parser, TOKEN_MINUS_MINUS);
            parse_unary_expression(parser);
            break;

        case TOKEN_AND:
            match(parser, TOKEN_AND);
            parse_cast_expression(parser);
            break;

        case TOKEN_STAR:
            match(parser, TOKEN_STAR);
            parse_cast_expression(parser);
            break;

        case TOKEN_PLUS:
            match(parser, TOKEN_PLUS);
            parse_cast_expression(parser);
            break;

        case TOKEN_MINUS:
            match(parser, TOKEN_MINUS);
            parse_cast_expression(parser);
            break;

        case TOKEN_TILDE:
            match(parser, TOKEN_TILDE);
            parse_cast_expression(parser);
            break;

        case TOKEN_NOT:
            match(parser, TOKEN_NOT);
            parse_cast_expression(parser);
            break;

        case TOKEN_SIZEOF:
            match(parser, TOKEN_SIZEOF);
            /* for now we will ignore the case where we have a type name */
            parse_unary_expression(parser);
            break;

        default:
            parse_postfix_expression(parser);
            break;
    }

    return NULL;
}

static Expression* parse_cast_expression(Parser* parser)
{
    /* for now we will just do a unary expression and ignore the part below */

    /* this is because they share some of the same start set and we don't want
     * anyhing to do with that just yet 
     */

    /* ( type-name ) cast-expression*/

    parse_unary_expression(parser);

    return NULL;
}

static Expression* parse_multiplicative_expression(Parser* parser)
{
    parse_cast_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT}, 3))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_STAR: match(parser, TOKEN_STAR); break;
            case TOKEN_SLASH: match(parser, TOKEN_SLASH); break;
            case TOKEN_PERCENT: match(parser, TOKEN_PERCENT); break;

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
    parse_multiplicative_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_PLUS, TOKEN_MINUS}, 2))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_PLUS: match(parser, TOKEN_PLUS); break;
            case TOKEN_MINUS: match(parser, TOKEN_MINUS); break;

            default:
                panic("unreachable");
                break;
        }

        parse_multiplicative_expression(parser);
    }

    return NULL;
}

static Expression* parse_shift_expression(Parser* parser)
{
    parse_additive_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LT_LT, TOKEN_GT_GT}, 2))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_LT_LT: match(parser, TOKEN_LT_LT); break;
            case TOKEN_GT_GT: match(parser, TOKEN_GT_GT); break;

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
        switch (curr_type(parser->stream))
        {
            case TOKEN_LT: match(parser, TOKEN_LT); break;
            case TOKEN_GT: match(parser, TOKEN_GT); break;
            case TOKEN_LT_EQUAL: match(parser, TOKEN_LT_EQUAL); break;
            case TOKEN_GT_EQUAL: match(parser, TOKEN_GT_EQUAL); break;

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
        switch (curr_type(parser->stream))
        {
            case TOKEN_EQUAL_EQUAL: match(parser, TOKEN_EQUAL_EQUAL); break;
            case TOKEN_NOT_EQUAL: match(parser, TOKEN_NOT_EQUAL); break;

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
        match(parser, TOKEN_XOR);

        parse_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_inclusive_or_expression(Parser* parser)
{
    parse_exclusive_or_expression(parser);

    while (is_match(parser, TOKEN_OR))
    {
        match(parser, TOKEN_OR);

        parse_exclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_and_expression(Parser* parser)
{
    parse_inclusive_or_expression(parser);

    while (is_match(parser, TOKEN_AND_AND))
    {
        match(parser, TOKEN_AND_AND);

        parse_inclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_or_expression(Parser* parser)
{
    parse_logical_and_expression(parser);

    while (is_match(parser, TOKEN_OR_OR))
    {
        match(parser, TOKEN_OR_OR);

        parse_logical_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_conditional_expression(Parser* parser)
{
    parse_logical_or_expression(parser);

    if (is_match(parser, TOKEN_QUESTION))
    {
        match(parser, TOKEN_QUESTION);

        parse_expression(parser);

        match(parser, TOKEN_COLON);

        parse_conditional_expression(parser);
    }

    return NULL;
}

static Expression* parse_assignment_expression(Parser* parser)
{
    parse_conditional_expression(parser);

    /* 
     * well need to add in the other actual part but for now we'll just
     * do the top half of this production
     *
     * unary-expression assignment-operator assignment-expression
     */

    return NULL;
}

static Expression* parse_constant_expression(Parser* parser)
{
    parse_conditional_expression(parser);

    return NULL;
}

static Expression* parse_expression(Parser* parser)
{
    parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        parse_assignment_expression(parser);
    }

    return NULL;
}






