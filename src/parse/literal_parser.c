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

#include "lex/char_help.h"

bool parse_integer_value(IntegerValue* value, const Token* token)
{
    assert(token_is_literal(token));

    const String* to_convert = &token->data.literal->value;
    size_t pos = 0;

    

    return false;
}

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
static unsigned int convert_simple_escape(char c)
{
    assert(is_simple_escape(c));

    // Trusting trust :)
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

// The maximum value we can fit in a char size
static unsigned int get_char_max_value(void)
{
    return (2 << (CHAR_BIT - 1)) - 1;
}

// "An integer character constant has type int"
static unsigned int get_char_width(void)
{
    return sizeof(int) * CHAR_BIT;
}

// "A wide character constant has type wchar_t"
static unsigned int get_wide_char_width(void)
{
    return sizeof(wchar_t) * CHAR_BIT;
}

static unsigned int decode_escape_sequence(const String* to_convert, size_t* pos,
        bool is_wide)
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

    if (current == 'x')
    {
        // Hexadecimal escape sequence

        // Check if the next digit is hexadecimal otherwise return current.
        // TODO: diagnose not having an hex digit after the x
        if (!is_hexadecimal(string_get(to_convert, *pos + 1)))
        {
            panic("expected hexadecimal digit after '\\x'");

            return current;
        }

        *pos += 1;

        unsigned int value = 0;
        bool overflow = false;
        do
        {
            current = string_get(to_convert, *pos);

            // Check if we are about to make the value overflow. I.e. is the top
            // nibble of value currently set at all? This works since we are 
            // about to multiply the value by 16 which removes top 4 bits.
            if (value & 0xF0000000)
            {
                overflow = true;
            }
            value *= 16;
            value += convert_hexadecimal(current);

            *pos += 1;
        } while (is_hexadecimal(string_get(to_convert, *pos)));
        *pos -= 1;

        // TODO: emit diagnostic on overflow
        if (overflow)
        {
            panic("hexadecimal escape overflow");
        }

        return value;
    }

    if (current >= '0' && current <= '7')
    {
        // Octal escape sequence
        unsigned int value = 0;
        size_t num_digits = 0;
        do
        {
            current = string_get(to_convert, *pos);

            value *= 8;
            value += convert_octal(current);

            num_digits++;

            *pos += 1;
        } while (num_digits < 3 && is_octal(string_get(to_convert, *pos)));
        *pos -= 1;

        // Check the limits on character conversion
        if (!is_wide && value > get_char_max_value())
        {
            // TODO: ensure there is an error here for octal conversion

            panic("octal escape value overflow");

            // Stolen from LLVM, TODO: get why this is the answer...
            value &= ~0U >> (32-CHAR_BIT);
        }

        return value;
    }

    // TODO: here do we diagnose a bad escape???
    // TODO: yes and just return the value and it is a warning in clang

    return current;
}

bool parse_char_literal(CharValue* value, const Token* token)
{
    assert(token->type == TOKEN_CHARACTER || token->type == TOKEN_WIDE_CHARACTER);

    // Do some setup here depending on the character literal
    const bool is_wide = (token->type == TOKEN_WIDE_CHARACTER);
    const int bit_width = is_wide ? get_wide_char_width() : get_char_width();

    // Get the string and it's length
    const String to_convert = token->data.literal->value;
    const size_t len = string_get_len(&to_convert);
    const size_t end = len - 1;

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
    size_t num_bytes = 0; // We want to test for multibyte constants
    while (pos != end)
    {
        unsigned int current = string_get(&to_convert, pos);

        // Here we want to check if we have an escpae sequence to manage
        if (current == '\\')
        {
            current = decode_escape_sequence(&to_convert, &pos, is_wide);
        }

        // Update the character value here this gives the same results as clang
        char_value *= (2 << (CHAR_BIT - 1));
        char_value += current;

        // Increment the position
        pos++;

        // Increment number of bytes
        num_bytes++;
    }

    // Silence unused variable warning for now but later we should warn for
    // multibyte character constants
    (void) num_bytes;

    value->value = char_value;
    value->error = false;

    return true;
}

// Note that decode escape sequence can probably be reused in parsing our
// string literals to reduce code size



