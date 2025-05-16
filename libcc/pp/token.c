#include "token.h"

#include <stdbool.h>

#include "core/panic.h"


bool is_token_keyword(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
            return false;

        default:
            panic("unknown token type");
            return false;
    }
}

bool is_token_identifier(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
            return false;

        default:
            panic("unknown token type");
            return false;
    }
}

bool is_token_constant(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
            return false;

        default:
            panic("unknown token type");
            return false;
    }
}

bool is_token_string_literal(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
            return false;

        default:
            panic("unknown token type");
            return false;
    }
}

bool is_token_punctuator(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
            return false;

        default:
            panic("unknown token type");
            return false;
    }
}

bool is_token_defineable(Token* tok)
{
    if (!is_token_identifier(tok))
    {
        return false;
    }
    // need to check if token is "defined" or not
    return false;
}
