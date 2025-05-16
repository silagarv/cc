#include "token.h"

#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include "core/panic.h"

bool is_token_equal(Token* tok, const char* str)
{
    const size_t str_len = strlen(str);
    if (tok->len == str_len && !strncmp(tok->start, str, str_len))
    {
        return true;
    }

    return false;
}

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
    // Can't define something that isnt an identifier
    if (!is_token_identifier(tok))
    {
        return false;
    }
    
    // check if the strings are equal 
    if (is_token_equal(tok, "defined"))
    {   
        return false;
    }

    // we have an identifier and the token is not equal to "defined"
    return true;
}
