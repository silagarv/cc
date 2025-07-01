#include "parser.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

#include "util/panic.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/ast.h"

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

// All of our functions for parsing declarations / definitions 

// All of our functions for parsing expressions
static Expression* parse_primary_expression(Parser* parser);

static Expression* parse_expression(Parser* parser);

// All of our functions for parsing statements
static Statement* parse_labeled_statement(Parser* parser);
static Statement* parse_compound_statement(Parser* parser);
static Statement* parse_expression_statement(Parser* parser);
static Statement* parse_selection_statement(Parser* parser);
static Statement* parse_iteration_statement(Parser* parser);
static Statement* parse_jump_statement(Parser* parser);
static Statement* parse_statement(Parser* parser);



void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};

    while (curr_type(stream) != TOKEN_EOF)
    {
       consume(stream);
    }

    return;
}
