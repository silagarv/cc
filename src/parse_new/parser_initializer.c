#include "parser_new.h"

#include <assert.h>

#include "files/location.h"

#include "lex/token.h"

bool parser_is_designation(Parser* parser)
{
    return parser_has(parser, TOK_LBRACKET, TOK_DOT);
}

Initializer* parse_initializer_list(Parser* parser)
{
    assert(parser_is(parser, TOK_LCURLY) && "not a '{'?");

    Location lcurly = parser_consume(parser);

    do
    {
        // We found the end of the initializer list.
        if (parser_is(parser, TOK_RCURLY))
        {
            break;
        }

        // // Parse one of the members and add it to the current initializer list
        // // if it exists.
        // Location equals = LOCATION_INVALID;
        // if (parser_is_designation(parser))
        // {

        // }

        Initializer* init = parse_initializer(parser);
    }
    while (parser_try_consume(parser, TOK_COMMA, NULL));

    // Initializer* inner = parse_initializer(parser);
    // TODO: will need to parse the inner part of this as well!!
    
    // TODO: need a good function for expecting a rcurly...
    // TODO: create the initializer from the initializer list...
    return NULL;
}

Initializer* parse_initializer_expression(Parser* parser)
{
    Expression* expr = parse_expression(parser);
    return semantic_checker_initializer_from_expression(&parser->sc, expr);
}

Initializer* parse_initializer(Parser* parser)
{
    assert(parser_is_initializer(parser) && "not an initializer?");

    if (parser_is(parser, TOK_LCURLY))
    {
        return parse_initializer_list(parser);
    }
    return parse_initializer_expression(parser);
}
