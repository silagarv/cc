#include "char_type.h"

#include <stdbool.h>

bool is_identifier_start(char c)
{
    if ('a' <= c && c <= 'z')
    {
        return true;
    }

    if ('A' <= c && c <= 'Z')
    {
        return true;
    }

    if (c == '_')
    {
        return true;
    }

    return false;
}

bool is_identifier(char c)
{
    if (is_identifier_start(c))
    {
        return true;
    }

    if (is_numeric(c))
    {
        return true;
    }

    return false;
}

bool is_numeric(char c)
{
    if ('0' <= c && c <= '9')
    {
        return true;
    }

    return false;
}

bool is_octal(char c)
{
    if ('0' <= c && c <= '7')
    {
        return true;
    }

    return false;
}

bool is_hexadecimal(char c)
{
    if (is_numeric(c))
    {
        return true;
    }

    if ('a' <= c && c <= 'e')
    {
        return true;
    }

    if ('A' <= c && c <= 'E')
    {
        return true;
    }

    return false;
}

bool is_horizontal_whitespace(char c)
{
    switch (c)
    {
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            return true;

        default:
            return false;
    }
}

bool is_ascii(char c)
{
    unsigned char uc = (unsigned char) c;

    // TODO: change to 'uc' since this is a bug lol... but this function is
    // unused for now though
    if (c <= 127)
    {
        return true;
    }

    return false;
}
