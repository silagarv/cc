#include "parser.h"

#include <stdbool.h>
#include <stddef.h>

#include "util/panic.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "lex/token.h"

void parser_initialise(Parser* parser, TokenList* tokens);
void parser_finialise(Parser* parser);

// Actual parsing code below

static TokenType curr_token_type(Parser* parser)
{
    return parser->stream.tokens[parser->stream.current_token].type;
}

static Token* curr_token(Parser* parser)
{
    return &parser->stream.tokens[parser->stream.current_token];
}

static TokenType peek_token_type(Parser* parser)
{
    return parser->stream.tokens[parser->stream.current_token + 1].type;
}

static void eat_token(Parser* parser)
{
    parser->stream.current_token++;
}

static bool is_match_type(Parser* parser, TokenType type)
{
    return (curr_token_type(parser) == type);
}

static bool is_match_types(Parser* parser, const TokenType* types, size_t count)
{
    const TokenType current = curr_token_type(parser);

    for (size_t i = 0; i < count; i++)
    {
        if (is_match_type(parser, types[i]))
        {
            return true;
        }
    }

    return false;
}

void test(Parser* p)
{
    if (is_match_types(p, (TokenType[]) {TOKEN_COLON, TOKEN_ARROW}, 2))
    {
        return;
    }
}


void parse_translation_unit(TokenStream stream)
{
    return;
}
