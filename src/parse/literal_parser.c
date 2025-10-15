#include "literal_parser.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <wchar.h>
#include <limits.h>
#include <math.h>
#include <float.h>
#include <assert.h>

#include "util/panic.h"
#include "util/str.h"

#include "files/location.h"

#include "driver/diagnostic.h"

#include "lex/char_help.h"
#include "lex/token.h"

static int parse_integer_prefix(const char* string, size_t len, size_t* pos)
{
    assert(*pos == 0 && "not at start of integer");

    // Any number in a special base should start with a '0'
    if (string[*pos] != '0')
    {
        return 10;
    }

    // Check if we are a hexadecimal number or technically not.
    if (string[*pos + 1] == 'x' || string[*pos + 1] == 'X')
    {   
        // Technically we aren't a hexadecimal number according to annex a
        if (string[*pos + 2] == '\0')
        {
            return 10;
        }

        // Otherwise we got a hex number, increment the position
        *pos += 2;
        return 16;
    }
    else
    {
        // We got an octal constant 
        *pos += 1;
        return 8;
    }
}

static IntegerValueSuffix parse_integer_suffix(const char* string, size_t len,
        size_t* pos)
{
    // First check if we even have to parse a suffix.
    if (len == *pos)
    {
        return INTEGER_VALUE_SUFFIX_NONE;
    }

    // Note that on failure we don't want to increment the position since this
    // helps us give a nice diagnostic and where to show it. So we will use an
    // additional variable to help us.
    size_t current_pos = *pos;
    if (string[current_pos] == 'U' || string[current_pos] == 'u')
    {
        current_pos++;

        if (current_pos == len)
        {
            *pos = current_pos;
            return INTEGER_VALUE_SUFFIX_U;
        }

        // Here is our pseudo state machine. All suffix's are valid no matter
        // the integer type so this is simple enough to implement. Just need to
        // check each time for the end of the integer string.
        if (string[current_pos] == 'l' || string[current_pos] == 'L')
        {
            char l_type = string[current_pos];
            current_pos++;

            if (current_pos == len)
            {
                *pos = current_pos;
                return INTEGER_VALUE_SUFFIX_UL;
            }

            if (string[current_pos] == l_type)
            {
                current_pos++;

                if (current_pos == len)
                {
                    *pos = current_pos;
                    return INTEGER_VALUE_SUFFIX_ULL;
                }
            }
        }
    }
    else if (string[current_pos] == 'l' || string[current_pos] == 'L')
    {
        char l_type = string[current_pos];
        bool ll = false;

        current_pos++;

        if (current_pos == len)
        {
            *pos = current_pos;
            return INTEGER_VALUE_SUFFIX_L;
        }

        if (string[current_pos] == l_type)
        {
            current_pos++;
            ll = true;

            if (current_pos == len)
            {
                *pos = current_pos;
                return INTEGER_VALUE_SUFFIX_LL;
            }
        }

        if (string[current_pos] == 'u' || string[current_pos] == 'U')
        {
            current_pos++;

            if (current_pos == len)
            {
                *pos = current_pos;
                return ll ? INTEGER_VALUE_SUFFIX_ULL : INTEGER_VALUE_SUFFIX_UL;
            }
        }
    }

    return INTEGER_VALUE_SUFFIX_INVALID;
}

static IntegerValueType determine_integer_value_type(uint64_t value, int base,
        IntegerValueSuffix suffix)
{
    // Below is derived from section 6.4.4.1 of c99 standard
    // TODO: the base check can probably be combined into the normal checking.
    switch (suffix)
    {
        case INTEGER_VALUE_SUFFIX_ULL:
            return INTEGER_VALUE_UNSIGNED_LONG_LONG;

        case INTEGER_VALUE_SUFFIX_LL:
            if (base == 10)
            {
                return INTEGER_VALUE_LONG_LONG;
            }
            else
            {
                if (value <= LLONG_MAX)
                {
                    return INTEGER_VALUE_LONG_LONG;
                }
                else
                {
                    return INTEGER_VALUE_UNSIGNED_LONG_LONG;
                }
            }
            break;

        case INTEGER_VALUE_SUFFIX_UL:
            if (value <= ULONG_MAX)
            {
                return INTEGER_VALUE_UNSIGNED_LONG;
            }
            else
            {
                return INTEGER_VALUE_UNSIGNED_LONG_LONG;
            }
            break;

        case INTEGER_VALUE_SUFFIX_L:
            if (base == 10)
            {
                if (value <= LONG_MAX)
                {
                    return INTEGER_VALUE_LONG;
                }
                else if (value <= LLONG_MAX)
                {
                    return INTEGER_VALUE_LONG_LONG;
                }
            }
            else
            {
                if (value <= LONG_MAX)
                {
                    return INTEGER_VALUE_LONG;
                }
                else if (value <= ULONG_MAX)
                {
                    return INTEGER_VALUE_UNSIGNED_LONG;
                }
                else if (value <= LLONG_MAX)
                {
                    return INTEGER_VALUE_LONG_LONG;
                }
                else
                {
                    return INTEGER_VALUE_UNSIGNED_LONG_LONG;
                }
            }
            break;

        case INTEGER_VALUE_SUFFIX_U:
            if (value <= UINT_MAX)
            {
                return INTEGER_VALUE_UNSIGNED_INTEGER;
            }
            else if (value <= ULONG_MAX)
            {
                return INTEGER_VALUE_UNSIGNED_LONG;
            }
            else
            {
                return INTEGER_VALUE_UNSIGNED_LONG_LONG;
            }
            break;

        case INTEGER_VALUE_SUFFIX_NONE:
            if (base == 10)
            {
                if (value <= INT_MAX)
                {
                    return INTEGER_VALUE_INTEGER;
                }
                else if (value <= LONG_MAX)
                {
                    return INTEGER_VALUE_LONG;
                }
                else if (value <= LLONG_MAX)
                {
                    return INTEGER_VALUE_LONG_LONG;
                }
            }
            else
            {
                if (value <= INT_MAX)
                {
                    return INTEGER_VALUE_INTEGER;
                }
                else if (value <= UINT_MAX)
                {
                    return INTEGER_VALUE_UNSIGNED_INTEGER;
                }
                else if (value <= LONG_MAX)
                {
                    return INTEGER_VALUE_LONG;
                }
                else if (value <= ULONG_MAX)
                {
                    return INTEGER_VALUE_UNSIGNED_LONG;
                }
                else if (value <= LLONG_MAX)
                {
                    return INTEGER_VALUE_LONG_LONG;
                }
                else
                {
                    return INTEGER_VALUE_UNSIGNED_LONG_LONG;
                }
            }
            break;

        case INTEGER_VALUE_SUFFIX_INVALID:
            panic("unreachable");
            return INTEGER_VALUE_ERROR;
    }

    return INTEGER_VALUE_ERROR;
}

bool parse_integer_literal(LiteralValue* value, DiagnosticManager* dm,
        Location location, const char* string, size_t len)
{
    size_t pos = 0;

    // Parse the prefix part
    int base = parse_integer_prefix(string, len, &pos);

    uint64_t int_val = 0;
    bool overflow = false;
    for (; pos < len; pos++)
    {
        char current = string[pos];

        // Check if we got a suffix or an error
        if (!is_valid_character_in_base(current, base))
        {
            // Special case of octal constant getting a bad digit. In this we
            // want to report the error and stop. Otherwise we assume that we
            // have a suffix we want to parse.
            if (base == 8 && is_decimal(current))
            {
                diagnostic_error_at(dm, location,
                        "invalid digit '%c' in octal constant", current);
                return false;
            }

            break;
        }

        // Check if the multiplication will overflow
        if (int_val > UINT64_MAX / (uint64_t) base)
        {
            overflow = true;
        }

        // Okay now get the digit and perform the multiplication
        uint64_t digit = convert_character_base(current, base);
        uint64_t new_val = int_val * (uint64_t) base;

        // Now check if addition will overflow the result
        if (new_val + digit < new_val)
        {
            overflow = true;
        }

        // Add digit to newval
        new_val += digit;

        // Set the new int_val
        int_val = new_val;
    }

    // Possibly parse integer suffix and check its validity
    IntegerValueSuffix suffix = parse_integer_suffix(string, len, &pos);
    if (suffix == INTEGER_VALUE_SUFFIX_INVALID)
    {
        diagnostic_error_at(dm, location,
                "invalid suffix '%s' on integer constant",
                string + pos);
        return false;
    }

    assert(len == pos && "should have parsed entire number or failed");

    // Okay if our suffix was okay but we overflowed, then error here and stop.
    // This is done last since we consider an invalid suffix worse than an
    // overflow!
    if (overflow)
    {
        diagnostic_error_at(dm, location,
                "integer literal is too large to be represented in any "
                "integer type");
        return false;
    }

    // Finally, determine the integer value type.
    IntegerValueType type = determine_integer_value_type(int_val, base, suffix);
    // The only way for this to occur is if we have a non-signable integer that
    // is too large for any / no suffix. The only type it could possible fit in
    // is the unsigned long long type. We will simply warn about it and move on
    if (type == INTEGER_VALUE_ERROR)
    {
        diagnostic_warning_at(dm, location,
                "integer constant is so large that it is unsigned");
        type = INTEGER_VALUE_UNSIGNED_LONG_LONG;
    }

    // Set the values fields and we are all done!
    value->type = VALUE_INTEGER_TYPE;
    value->value.integer.type = type;
    value->value.integer.suffix = suffix;
    value->value.integer.value = int_val;

    return true;
}

static int parse_float_prefix(const char* string, size_t len, size_t* pos)
{
    assert(*pos == 0 && "not at start of integer");

    if (string[0] != '0')
    {
        return 10;
    }

    if (string[1] == 'x' || string[1] == 'X')
    {
        if (string[2] == '\0')
        {
            return 10;
        }

        *pos += 1;
        return 16;
    }

    return 10;
}

static FloatingValueSuffix parse_float_suffix(const char* string, size_t len,
        size_t* pos)
{
    // If we have no suffix
    if (len == *pos)
    {
        return FLOATING_VALUE_SUFFIX_NONE;
    }

    size_t current_pos = *pos;
    if (string[current_pos] == 'f' || string[current_pos] == 'F')
    {
        current_pos++;

        if (current_pos == len)
        {
            *pos = current_pos;

            return FLOATING_VALUE_SUFFIX_F;
        }
    }
    else if (string[current_pos] == 'l' || string[current_pos] == 'L')
    {
        current_pos++;

        if (current_pos == len)
        {
            *pos = current_pos;

            return FLOATING_VALUE_SUFFIX_L;
        }
    }

    return FLOATING_VALUE_SUFFIX_ERROR;
}

bool parse_float_exponent(const char* string, size_t len, size_t* pos,
        long double* exponent)
{
    assert(string[*pos] == 'E' || string[*pos] == 'e');

    *pos += 1;

    // see if we get a plus or minus first as well...
    bool negative = false;
    if (string[*pos] == '-')
    {
        negative = true;
        *pos += 1;
    }
    else if (string[*pos] == '+')
    {
        *pos += 1;
    }
    else if (string[*pos] == '\0')
    {
        // TODO: somehow issue an error that exponent has no digits
        return true;
    }

    // TODO: parse digit sequence...
    uint64_t value = 0;
    bool overflow = false;
    for (; *pos < len; *pos += 1)
    {
        char current = string[*pos];

        // We either have an error, a suffix, or are at the end of the string
        if (!is_decimal(current))
        {
            break;
        }

        // Check if the multiplication will overflow
        if (value > UINT64_MAX / (uint64_t) 10)
        {
            overflow = true;
        }

        // Okay now get the digit and perform the multiplication
        uint64_t digit = convert_character_base(current, 10);
        uint64_t new_val = value * 10;

        // Now check if addition will overflow the result
        if (new_val + digit < new_val)
        {
            overflow = true;
        }

        // Add digit to newval
        new_val += digit;

        // Set the new int_val
        value = new_val;
    }

    // If we overflow set the value to uint64 max so that the parse should later
    // error that the float overflowed.
    if (overflow)
    {
        value = UINT64_MAX;
    }

    // Finally set the exponent value correctly.
    if (negative)
    {
        *exponent = -1.l * (long double) value;
    }
    else
    {
        *exponent = (long double) value;
    }   

    return false;
}

static FloatingValueType determine_float_value_type(FloatingValueSuffix suffix)
{
    switch (suffix)
    {
        case FLOATING_VALUE_SUFFIX_NONE: return FLOATING_VALUE_DOUBLE;
        case FLOATING_VALUE_SUFFIX_F: return FLOATING_VALUE_FLOAT;
        case FLOATING_VALUE_SUFFIX_L: return FLOATING_VALUE_LONG_DOUBLE;
        default:
            panic("unreachable");
            return FLOATING_VALUE_ERROR;
    }
}

const char* floating_type_to_name(FloatingValueType type)
{
    switch (type)
    {
        case FLOATING_VALUE_ERROR:
            panic("unreachable");
            return NULL;
        
        case FLOATING_VALUE_FLOAT:
            return "float";

        case FLOATING_VALUE_DOUBLE:
            return "double";

        case FLOATING_VALUE_LONG_DOUBLE:
            return "long double";
    }
}

static bool check_float_value(long double* value, FloatingValueType type)
{
    switch (type)
    {
        case FLOATING_VALUE_FLOAT:
            if (*value > FLT_MAX)
            {
                *value = FLT_MAX;

                return true;
            }
            return false;

        case FLOATING_VALUE_DOUBLE:
            if (*value > DBL_MAX)
            {
                *value = DBL_MAX;

                return true;
            }
            return false;

        case FLOATING_VALUE_LONG_DOUBLE:
            if (*value > LDBL_MAX)
            {
                *value = LDBL_MAX;

                return true;
            }
            return false;

        case FLOATING_VALUE_SUFFIX_ERROR:
            panic("unreachable");
            break;
    }

    return false;
}

bool parse_float_literal(LiteralValue* value, DiagnosticManager* dm,
        Location location, const char* string, size_t len)
{
    size_t pos = 0;
    int base = parse_float_prefix(string, len, &pos);
    
    assert(base == 10 && "hexadecimal floats not supported");

    long double number = 0;
    long double postdot_multiply = 1E-1;
    long double exponent = 0;
    bool dot = false;
    bool had_exponent = false;
    
    for (; pos < len; pos++)
    {
        char current = string[pos];

        if (!is_valid_character_in_base(current, base))
        {
            if (current == '.')
            {
                dot = true;
                continue;
            }
            
            // Parse the exponent, then break from the loop to go to the last
            // part to check if we have a suffix or not
            if (current == 'e' || current == 'E')
            {
                bool error = parse_float_exponent(string, len, &pos,
                        &exponent);
                if (error)
                {
                    diagnostic_error_at(dm, location, "exponent has no digits");
                    return false;
                }
                had_exponent = true;
            }
            break;
        }

        uint64_t digit = convert_character_base(current, base);
        if (!dot)
        {
            // Handle the part before the dot
            number = (number * 10) + (long double) digit;
        }
        else
        {
            number = number + (long double) digit * postdot_multiply;
            postdot_multiply /= 10;
        }
    }

     // Finally get the suffix of the number
    FloatingValueSuffix suffix = parse_float_suffix(string, len, &pos);
    if (suffix == FLOATING_VALUE_SUFFIX_ERROR)
    {
        diagnostic_error_at(dm, location,
                "invalid suffix '%s' on floating constant", string + pos);
        return false;
    }

    assert(len == pos && "should have failed to convert");
    
    // First get the type of float that we have.
    FloatingValueType type = determine_float_value_type(suffix);

    // If had an exponent we need to do some maths here.
    if (had_exponent)
    {
        // TODO: what about really small values. Need to figure this out...
        exponent = powl(10, exponent);
        number *= exponent;
    }

    // TODO: check that we also don't underflow our floats i.e. get way too
    // small of a value
    bool error = check_float_value(&number, type);
    if (error)
    {   
        const char* type_name = floating_type_to_name(type);
        diagnostic_warning_at(dm, location,
                "floating-point constant too large for type '%s'", type_name);
    }

    // Finally set up our finished floating value type here.
    value->type = VALUE_FLOATING_TYPE;
    value->value.floating.type = type;
    value->value.floating.suffix = suffix;
    value->value.floating.value = number;

    return true;
}

bool parse_preprocessing_number(LiteralValue* value, DiagnosticManager* dm,
        const Token* token)
{
    assert(token_is_type(token, TOKEN_NUMBER));

    const Location location = token->loc;

    const LiteralNode* literal = token->data.literal;
    const char* string = literal->value.ptr;
    size_t len = literal->value.len;

    // First before we actually parse anything we need to figure out if we got
    // a float or a integer literal as it could be either. Then we can try
    // the conversion. We need to do this since we could go either way at this
    // point... Then we also need to redo character and string conversion sadly

    // return parse_float_literal(value, dm, location, string, len);

    return parse_integer_literal(value, dm, location, string, len);
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

        // TODO: check if itll fit into the type
        // TODO: make this check better instead of just using some random
        // magic numbers...
        if (!is_wide && value > 255)
        {
            panic("Hex escape out of range");
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
    // TODO: maybe change to pos < end
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

static void parse_string_literal_internal(Buffer* buffer, const Token* token)
{
    assert(token->type == TOKEN_STRING);

    const String to_convert = token->data.literal->value;
    const size_t len = string_get_len(&to_convert);
    const size_t end = len - 1;

    // Assertions to make sure we formed a valid string during tokenisation
    assert(string_get(&to_convert, 0) == '"');
    assert(string_get(&to_convert, end) == '"');

    // Here we actually convert the string
    size_t pos = 1;
    while (pos < end)
    {
        char current = string_get(&to_convert, pos);

        if (current == '\\')
        {
            current = decode_escape_sequence(&to_convert, &pos, false);
        }

        // Add the character to the buffer
        buffer_add_char(buffer, current);

        // Increment the position
        pos++;
    }
}

bool parse_string_literal(StringLiteral* value, const Token* tokens, size_t num_tokens)
{
    bool have_wide = false;
    size_t maximum_size = 0;
    for (size_t i = 0; i < num_tokens; i++)
    {
        const Token token = tokens[i];

        if (token.type == TOKEN_WIDE_STRING)
        {
            have_wide = true;
        }
    }

    if (have_wide)
    {
        panic("cannot convert a wide string literal");

        return false;
    }

    Buffer buffer = buffer_new();
    for (size_t i = 0; i < num_tokens; i++)
    {
        parse_string_literal_internal(&buffer, &tokens[i]);
    }

    value->string = string_from_buffer(&buffer);
    value->wide = false;
    value->error = false;

    return true;
}

