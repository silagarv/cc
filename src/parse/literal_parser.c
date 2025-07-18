#include "literal_parser.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <wchar.h>
#include <limits.h>
#include <assert.h>

#include "lex/token.h"
#include "util/panic.h"
#include "util/str.h"

// Check if the character given is a simple escape. Otherwise return false
static bool is_simple_escape(char c)
{
    switch (c)
    {
        case '\'':
        case '"':
        case '?':
        case '\\':
        case 'a':
        case 'b':
        case 'f':
        case 'n':
        case 'r':
        case 't':
        case 'v':
            return true;

        default:
            return false;
    }
}

// Convert the second character in a simple escape sequence to it's corrospoing
// value. This is used by our string and character parsers
static char convert_simple_escape(char c)
{
    assert(is_simple_escape(c));

    switch (c)
    {
        case '\'': return '\'';
        case '"':  return '\"';
        case '?':  return '\?';
        case '\\': return '\\';
        case 'a':  return '\a';
        case 'b':  return '\b';
        case 'f':  return '\f';
        case 'n':  return '\n';
        case 'r':  return '\r';
        case 't':  return '\t';
        case 'v':  return '\v';

        default: panic("unreachable"); return '\0'; // Default unreachable value
    }
}

// "An integer character constant has type int"
static int get_char_width(void)
{
    return sizeof(int) * CHAR_BIT;
}

// "A wide character constant has type wchar_t"
static int get_wide_char_width(void)
{
    return sizeof(wchar_t) * CHAR_BIT;
}

static unsigned int decode_escape_sequence(const String* to_convert, size_t* pos)
{
    assert(string_get(to_convert, *pos) == '\\');

    // Skip over the '\'
    *pos += 1;

    // Get the current escape
    char current = string_get(to_convert, *pos);

    if (is_simple_escape(current))
    {
        return convert_simple_escape(current);
    }

    // Hexadecimal escape sequence
    if (current == 'x')
    {
        // Here we have an unbounded number of hexadecimal chars
        panic("unsupported octal escapes");
    }

    // Octal escape sequence
    if (current >= '0' && current <= '7')
    {
        // Here we have a maximum of
        panic("unsupported octal escapes");
    }

    // TODO: here do we diagnose a bad escape???

    return '\0';
}

bool parse_char_literal(CharValue* value, const Token* token)
{
    assert(token->type == TOKEN_CHARACTER || token->type == TOKEN_WIDE_CHARACTER);

    // Do some setup here depending on the character literal
    const bool is_wide = (token->type == TOKEN_WIDE_CHARACTER);
    const int bit_width = is_wide ? get_wide_char_width() : get_char_width();

    // Get the string and it's length
    const String to_convert = token->opt_value;
    const size_t len = string_get_len(&to_convert);

    size_t pos;

    // Validate start and end of string including it's length to ensure that what
    // we are about to process is actually a valid character token
    if (is_wide)
    {
        assert(string_get(&to_convert, 0) == 'L');
        assert(string_get(&to_convert, 1) == '\'');
        assert(string_get(&to_convert, len - 1) == '\'');

        assert(len > 3);

        pos = 2;
    }
    else
    {
        assert(string_get(&to_convert, 0) == '\'');
        assert(string_get(&to_convert, len - 1) == '\'');

        assert(len > 2);

        pos = 1;
    }

    // Okay, now we know we have a well formed char literal the following applies
    // 1. Due to the way we formed tokens, we don't need to worry about trigraphs
    //    or backslash newlines
    // 2. Position is currently at the first character in the literal and len - 1
    //    is the last character in it
    // 3. That the char literal is well formed. e.g. it does not do anything
    //    funkier than escaped characters

    uint64_t char_value = 0;
    while (pos != len - 1)
    {
        unsigned int current = string_get(&to_convert, pos);

        // Here we want to check if we have an escpae sequence to manage
        if (current == '\\')
        {
            current = decode_escape_sequence(&to_convert, &pos);

            // If the escape sequence had issues just quit here and return failure
            if (!current)
            {
                return false;
            }
        }

        // Update the character value here this gives the same results as clang
        // TODO: make this a bit cleaner and nicer
        char_value = (char_value * (2 << (CHAR_BIT - 1))) + current;

        // Increment the position
        pos++;
    }

    value->value = char_value;
    value->error = false;

    return true;
}


