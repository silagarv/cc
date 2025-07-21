#include "char_help.h"

#include <stdbool.h>

#include "util/panic.h"

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

    if ('a' <= c && c <= 'f')
    {
        return true;
    }

    if ('A' <= c && c <= 'F')
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

bool is_vertical_whitespace(char c)
{
    switch (c)
    {
        case '\r':
        case '\n':
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
    if (uc <= 127)
    {
        return true;
    }

    return false;
}

// Convert an octal character to it's corrosponding numeric value. Note that
// convert hexadecimal can be used, but this is for stricter conversion
unsigned int convert_octal(char c)
{
    switch (c)
    {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        default: panic("invalid octal digit"); return 0;
    }
}

// Convert a decimal digit to its corrosponding numeric value. Note that we 
// could alternatively convert to hex but this is used for stricter conversion
unsigned int convert_decimal(char c)
{
    switch (c)
    {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        default: panic("invalid decimal digit"); return 0;
    }
}

// Convert a hexadecimal character to its corrosponding numeric value
unsigned int convert_hexadecimal(char c)
{
    switch (c)
    {
        case '0':           return 0;
        case '1':           return 1;
        case '2':           return 2;
        case '3':           return 3;
        case '4':           return 4;
        case '5':           return 5;
        case '6':           return 6;
        case '7':           return 7;
        case '8':           return 8;
        case '9':           return 9;
        case 'A': case 'a': return 10;
        case 'B': case 'b': return 11;
        case 'C': case 'c': return 12;
        case 'D': case 'd': return 13;
        case 'E': case 'e': return 14;
        case 'F': case 'f': return 15;
        default: panic("invalid hexadecimal digit"); return 0;
    }
}

bool is_valid_ucn(uint32_t value)
{
    return false;
}
