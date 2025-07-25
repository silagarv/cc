#include "literal_parser.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <wchar.h>
#include <limits.h>
#include <assert.h>

#include "util/panic.h"
#include "util/str.h"

#include "driver/target.h"

#include "lex/char_help.h"
#include "lex/token.h"


// Determining the type of value is quite important I think and simply comes
// down to interpreting the table in 6.4.4.1 whilst taking into account what
// values can be represented in the target... So I will need to develop the target
// thing eventually but for now we will REQUIRE x86-64-linux.
// The sizes it assumes are below
//
static IntegerValueType determine_integer_value_type(uint64_t value, size_t base, 
        IntegerValueSuffix suffix)
{
    // We need to know target information here... Should we retroactively fit
    // this onto the compiler or like what?


    // We should probably never return error here
    panic("integer value type underterimined");

    return INTEGER_VALUE_ERROR;
}

// We will use a pointer to pos since it think it would be good to check that
// we reached the end of the string
static IntegerValueSuffix parse_integer_suffix(const String* string, size_t pos, 
        size_t len)
{
    // Should never try to take the suffix of nothing
    assert(len != pos);

    const size_t suffix_len = len - pos;
    // We know we will never have a suffix bigger than ULL and its variants
    if (suffix_len > 3)
    {
        return INTEGER_VALUE_SUFFIX_INVALID;
    }

    bool has_u = false;
    bool has_l1 = false;
    bool has_l2 = false;

    // Note the below works since we can only ever have one of each of u, l, or
    // ll suffix in an integer number so we check for it
    while (pos < len)
    {
        char current = string_get(string, pos);
        pos++;
        
        // TODO: would a switch make the code below cleaner??

        if ((current == 'u' || current == 'U') && !has_u)
        {
            has_u = true;

            continue;
        }

        // Here we check for l1 since it will be set if we get any l's
        if ((current == 'l' || current == 'L') && !has_l1)
        {
            has_l1 = true;
            const char l_type = current;
            
            if (string_get(string, pos) == current)
            {
                pos++;

                has_l2 = true;
            }

            continue;
        }

        // We have got a digit that can never be part of a suffix
        return INTEGER_VALUE_SUFFIX_INVALID;
    }

    assert(pos == len);

    // Get if we're ll or l. We restict ourselves so that we can only be one
    bool is_ll = has_l1 && has_l2; // were ll is we have both l1 and l2
    bool is_l = has_l1 && !has_l2; // were l if we have l1 but not l2

    assert(is_l ? !has_l2 : true);

    // TODO: this could be a bit cleaner but will work for now...
    if (has_u)
    {
        if (is_l)
        {
            return INTEGER_VALUE_SUFFIX_UL;
        }
        else if (is_ll)
        {
            return INTEGER_VALUE_SUFFIX_ULL;
        }
        else
        {
            return INTEGER_VALUE_SUFFIX_U;
        }
    }
    else
    {
        if (is_l)
        {
            return INTEGER_VALUE_SUFFIX_L;
        }
        else if (is_ll)
        {
            return INTEGER_VALUE_SUFFIX_LL;
        }
    }

    panic("unreachable; we should have got some kind of suffix but didn't");
    
    return INTEGER_VALUE_SUFFIX_INVALID;
}

/*
    intteger-constant:
        decimal-constant integer-suffix opt
        octal-constant integer-suffix opt
        hexadecimal-constant integer-suffix opt
    
    decimal-constant:
        nonzero-digit
        decimal-constant digit
 
    octal-constant:
        0
        octal-constant octal-digit

    hexadecimal-constant:
        hexadecimal-prefix hexadecimal-digit
        hexadecimal-constant hexadecimal-digit
        hexadecimal-prefix: one of
        0x 0X
*/

// TODO: implementing binary literals here could also be good and quite easy
// to achieve. Would not be the hardest
bool parse_integer_literal(IntegerValue* value, const Token* token)
{
    assert(token_is_literal(token));

    // Get the string we would like to convert
    const String* to_convert = &token->data.literal->value;
    const size_t len = string_get_len(to_convert);

    size_t pos = 0;

    // Start with assuming the base is 10
    size_t base = 10;

    // Check the start to accurately determine the base of the number. The first
    // digit will tell us if it is a specical case
    if (string_get(to_convert, pos) == '0')
    {
        pos++;

        // Now check if the next character. If it is an x, check for a hex digit
        // afterwards, and if present then we got a Hex number. Otherwise we
        // have an octal number even if it is an 'x' or 'X'
        char current = string_get(to_convert, pos);
        char next = string_get(to_convert, pos + 1);

        if ((current == 'x' || current == 'X') && is_hexadecimal(next))
        {   
            // Skip over the 'x' or 'X' but do not skip the next digit
            pos++;

            base = 16;
        }
        else
        {
            base = 8;
        }
    }

    uint64_t int_value = 0;
    bool overflow = false;
    while (pos < len)
    {
        char current = string_get(to_convert, pos);

        if (!is_valid_character_in_base(current, base))
        {
            // We got a bad character, we could possibly be very invalid here
            // or we could be able to parse an integer suffix so figure that out
            // TODO: this will not work if I add binary. maybe instead do:
            //      if (base == 8 && is_decimal(current)) ????
            if (base == 8 && current == '9')
            {
                // This is the only BAD case all the others we defer any errors
                // until we get to the suffix parsing
                printf("Invalid digit '%c' in contant\n\n", current);

                goto bad_conversion;
            }
            else
            {
                break; // We now want to parse a suffix (or try)
            }
        }

        // TODO: issue diagnostic about overflow
        if (int_value * base < int_value)
        {
            printf("Overflowed during conversion\n");

            overflow = true;
        }
        int_value *= base;
        int_value += convert_character_base(current, base);

        pos++;
    }

    IntegerValueSuffix suffix = INTEGER_VALUE_SUFFIX_NONE;
    if (pos != len)
    {
        suffix = parse_integer_suffix(to_convert, pos, len);
        
        if (suffix == INTEGER_VALUE_SUFFIX_INVALID)
        {
            printf("Bad integer suffix; cannot continue parsing integer\n");

            return false;
        }
    }

    // TODO: we would like to also now determine the integer value type correctly
    IntegerValueType type = determine_integer_value_type(int_value, base, suffix);

    value->type = INTEGER_VALUE_ERROR;
    value->suffix = suffix;
    value->value = int_value;
    value->base = base;
    value->base = base;
    
    value->overflow = overflow;

    return true;

bad_conversion: // TODO: maybe do some error reporting here...?
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
        *pos -= 1; // TODO: I wan't to remove this by fixing decode char function

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
    // TODO: yes and just return the value and it is a warning in clang / gcc

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

    // Validate start and end of string including it's length to ensure that what
    // we are about to process is actually a valid character token
    size_t pos = 0;
    if (is_wide)
    {
        assert(string_get(&to_convert, pos) == 'L');
        assert(len > 3);

        pos++;
    }
    else
    {
        assert(len > 2);
    }
    assert(string_get(&to_convert, pos) == '\'');
    assert(string_get(&to_convert, len - 1) == '\'');
    pos++;

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



