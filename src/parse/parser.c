#include "parser.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>

#include "util/panic.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "lex/token.h"

void parser_initialise(Parser* parser, TokenList* tokens);
void parser_finialise(Parser* parser);

static bool is_valid_stream_position(TokenStream* stream)
{
    return (stream->current_token < stream->count);
}

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

static bool match(TokenStream* stream, TokenType type)
{
    if (curr_type(stream) == type)
    {
        consume(stream);

        return true;
    }

    return false;
}

static bool is_match(TokenStream* stream, TokenType type)
{
    return (curr_type(stream) == type);
}

static bool has_match(TokenStream* stream, const TokenType* types, size_t count)
{
    const TokenType current = curr_type(stream);

    for (size_t i = 0; i < count; i++)
    {
        if (current == types[i])
        {
            return true;
        }
    }

    return false;
}

void parse_primary_expression(TokenStream* stream, LineMap* map)
{
    if (is_match(stream, TOKEN_IDENTIFIER))
    {
        match(stream, TOKEN_IDENTIFIER);
    }
    else if (false /* constant??? */)
    {
        /* idk... */
    }
    else if (has_match(stream, (TokenType[]) {TOKEN_STRING, TOKEN_WIDE_STRING}, 2))
    {

    }
    else if (is_match(stream, TOKEN_LPAREN))
    {
        /* parse_expression(stream, map); */
    }
    else
    {
        diag_error("Failed to parse primary expression");
    }
}

void parse_translation_unit(TokenStream stream, LineMap* map)
{
    while (curr_type(&stream) != TOKEN_EOF)
    {
        parse_primary_expression(&stream, map);
    }

    return;
}
