#include "literal_parser.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <wchar.h>
#include <limits.h>
#include <math.h>
#include <float.h>
#include <assert.h>

#include "parse/ast_allocator.h"
#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"

#include "files/location.h"

#include "driver/diagnostic.h"

#include "lex/char_help.h"
#include "lex/token.h"

typedef enum EscapeSequenceResult {
    ESCAPE_SEQUENCE_RESULT_OK,
    ESCAPE_SEQUENCE_RESULT_ERROR,
    ESCAPE_SEQUENCE_RESULT_FATAL
} EscapeSequenceResult;

uint64_t integer_value_get_value(const IntegerValue* val)
{
    return val->value;
}

uint64_t char_value_get_value(const CharValue* val)
{
    return val->value;
}

long double floating_value_get_value(const FloatingValue* val)
{
    return val->value;
}

ValueType literal_value_get_type(const LiteralValue* value)
{
    return value->type;
}

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

    // Now check if we got an empty exponent (after we have tried to skip a +-)
    if (string[*pos] == '\0')
    {
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
            if (current == '.' && !dot)
            {
                dot = true;
                continue;
            }
            
            // Parse the exponent, then break from the loop to go to the last
            // part to check if we have a suffix or not
            if (current == 'e' || current == 'E')
            {
                bool error = parse_float_exponent(string, len, &pos, &exponent);
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

static ValueType determine_value_type(const char* string, size_t len)
{
    size_t pos = 0;
    int base = 10;

    // Check if length > 2
    if (string[0] == '0')
    {
        if ((string[1] == 'x' || string[1] == 'X') && len > 2)
        {
            base = 16;
            pos += 2;
        }
        else
        {
            base = 8;
            pos++;
        }
    }

    for (; pos < len; pos++)
    {
        char current = string[pos];
        
        // Can simply keep going if we're at a valid character in the base
        if (is_valid_character_in_base(current, base))
        {
            continue;
        }

        // A dot means we certainly have a floating value
        if (current == '.')
        {
            return VALUE_FLOATING_TYPE;
        }

        if (current == 'p' || current == 'P')
        {
            if (pos < len && (string[pos + 1] == '+' || string[pos + 1] == '-'))
            {
                return VALUE_FLOATING_TYPE;
            }
        }

        if (current == 'e' || current == 'E')
        {
            if (pos < len && (string[pos + 1] == '+' || string[pos + 1] == '-'))
            {
                return VALUE_FLOATING_TYPE;
            }
        }
    }

    // If we get to the end and haven't seen any floating point stuff we got
    // and integer value type.
    return VALUE_INTEGER_TYPE;
} 

bool parse_preprocessing_number(LiteralValue* value, DiagnosticManager* dm,
        const Token* token)
{
    assert(token_is_type(token, TOK_NUMBER));

    const Location location = token->loc;

    const LiteralNode* literal = token->data.literal;
    const char* string = literal->value.ptr;
    size_t len = literal->value.len;

    // First before we actually parse anything we need to figure out if we got
    // a float or a integer literal as it could be either. Then we can try
    // the conversion. We need to do this since we could go either way at this
    // point... Then we also need to redo character and string conversion sadly
    if (determine_value_type(string, len) == VALUE_FLOATING_TYPE)
    {
        return parse_float_literal(value, dm, location, string, len);
    }
    else
    {
        return parse_integer_literal(value, dm, location, string, len);
    }
}

static uint64_t decode_escape_sequence(const char* raw, size_t len, size_t* pos,
        bool wide, DiagnosticManager* dm, Location loc,
        EscapeSequenceResult* success)
{
    assert(raw[*pos - 1] == '\\');

    // UINT_MAX for wide since it has type int on our platform
    const uint64_t max_escape = !wide ? UCHAR_MAX : UINT_MAX;

    // Set on error initially
    *success = ESCAPE_SEQUENCE_RESULT_OK;

    // Get the current and then skip over it, since we can always undo this
    char current = raw[*pos];
    *pos += 1;

    switch (current)
    {
        // All of our simple escapes.
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

        // Hexadecimal escapes
        case 'x':
        {
            // Check for if we are at the end of the sequence or if we got a
            // potentially invalid escape sequence here.
            if (*pos == len || !is_hexadecimal(raw[*pos]))
            {
                // This is not a fatal error so just return 0 here.
                diagnostic_error_at(dm, loc,
                        "\\x used with no following hex digits");
                *success = ESCAPE_SEQUENCE_RESULT_ERROR;

                return 0;
            }

            // Now actually convert the hexadecimal escape sequence
            uint64_t value = 0;
            do
            {
                // TODO: should overflow also be checked here

                value *= 16;
                value |= (uint64_t) convert_hexadecimal(raw[*pos]);

                *pos += 1;
            }
            while (is_hexadecimal(raw[*pos]));

            // Test for out of range and fatally error if so.
            if (value > max_escape)
            {
                diagnostic_error_at(dm, loc,
                        "hex escape sequence out of range");
                *success = ESCAPE_SEQUENCE_RESULT_FATAL;

                value = max_escape;
            }

            return value;
        }

        // Octal escapes
        case '0': case '1': case '2': case '3': 
        case '4': case '5': case '6': case '7':
        {
            // Undo the skip we did of the current digit.
            *pos -= 1;

            // Do the octal conversion.
            uint64_t digit_count = 0;
            uint64_t value = 0;
            do
            {
                value *= 8;
                value |= (uint64_t) convert_octal(raw[*pos]);

                digit_count++;

                *pos += 1;
            }
            while (is_octal(raw[*pos]) && digit_count < 3);

            // Test for out of range error but we do not have to fatally error
            // for this, since clang appears to not for some reason.
            if (value > max_escape)
            {
                diagnostic_error_at(dm, loc,
                        "octal escape sequence out of range");
                *success = ESCAPE_SEQUENCE_RESULT_ERROR;

                value = max_escape;
            }

            return value;
        }
        
        // Universal character names
        case 'u':
        case 'U':
        {
            if (*pos == len || !is_hexadecimal(raw[*pos]))
            {
                diagnostic_error_at(dm, loc,
                        "\\%c used with no following hex digits",
                        current);
                *success = ESCAPE_SEQUENCE_RESULT_FATAL;

                return 0;
            }

            uint64_t req_digits = (current == 'u') ? 4 : 8;
            uint64_t num_digits = 0;
            utf32 value = 0;

            // Do the main loop of ucn conversion
            do
            {
                current = raw[*pos];
                
                value *= 16;
                value |= (uint64_t) convert_hexadecimal(current);

                *pos += 1;

                num_digits++;
            }
            while (is_hexadecimal(raw[*pos]) && num_digits < req_digits);

            // Check we got the correct ammount of digits for the UCN
            if (num_digits != req_digits)
            {
                diagnostic_error_at(dm, loc,
                        "incomplete universal character name");
                *success = ESCAPE_SEQUENCE_RESULT_FATAL;

                return 0;
            }

            // Need to make sure we have a valid ucn
            if (!is_valid_ucn(value))
            {
                // If we don't get a valid ucn then we have a problem so error
                // about this. The below just builds a nicely formatted error
                diagnostic_error_at(dm, loc,
                        "\\%c%0*X is not a valid universal character",
                        (num_digits == 4) ? 'u' : 'U', num_digits, value);
                *success = ESCAPE_SEQUENCE_RESULT_ERROR;

                return value;
            }

            // Need to make sure the ucn fits in the character type.
            if (!wide && value > CHAR_MAX)
            {
                // Error basically if we have a ucn in a non-wide character
                // literal as according to clang this is wrong
                diagnostic_error_at(dm, loc,
                        "character too large for enclosing character literal "
                        "type");
                *success = ESCAPE_SEQUENCE_RESULT_FATAL;

                return CHAR_MAX;
            }

            return value;
        }

        // Unknown escape sequence. Make a warning and simply return the value
        // of whatever char we got.
        default:
            diagnostic_warning_at(dm, loc, "unknown escape sequence '\\%c'",
                    current);
            *success = ESCAPE_SEQUENCE_RESULT_ERROR;

            return current;
    }
}

bool parse_char_literal(CharValue* value, DiagnosticManager* dm,
        const Token* token, bool wide)
{
    assert(token->type == TOK_CHARACTER ||
            token->type == TOK_WIDE_CHARACTER);

    // Get the location and raw literal data for conversion
    const Location loc = token->loc;

    const String literal = token->data.literal->value;
    const char* raw = literal.ptr;
    const size_t len = literal.len;

    size_t pos = 0;

    // Some sanity checks for what we are about to convert
    assert(wide ? raw[0] == 'L' : true);

    if (wide)
    {
        pos++;
    }

    assert(raw[pos] == '\'');
    assert(raw[len - 1] == '\'');

    // Skip over the '\'' to ensure we don't get it
    pos++;
    
    uint64_t val = 0;
    uint64_t num_digits = 0;
    bool error = false;
    bool fatal_error = false;
    do
    {
        uint64_t current = raw[pos++];

        if (current == '\\')
        {
            EscapeSequenceResult result;
            current = decode_escape_sequence(raw, len - 1, &pos, wide, dm, loc,
                    &result);

            // Only fully fail if we cannot decode the escape at all. But set
            // the error flag otherwise
            if (result == ESCAPE_SEQUENCE_RESULT_FATAL)
            {
                fatal_error = true;
            }
            else if (result == ESCAPE_SEQUENCE_RESULT_ERROR)
            {
                error = true;
            }
        }

        // Shift and add the current value in
        val *= (2 << (CHAR_BIT - 1));
        val |= current;

        // Increment the number of digits
        num_digits++;
    }
    while (pos < len - 1);

    if (!wide && num_digits > 1)
    {
        diagnostic_warning_at(dm, loc, "multi-character character constant");

        // WIDE CHARACTER LITERALS MAY NOT CONTAIN MORE THAN 1 DIGIT!!!
    }
    else if (wide && num_digits > 1)
    {
        diagnostic_error_at(dm, loc, "wide character literals may not contain "
                "multiple characters");
        fatal_error = true;
    }
    
    if (!wide && val > INT_MAX)
    {
        diagnostic_warning_at(dm, loc,
                "character constant too long for its type");
    }

    // Okay if we had a normal character constant do a cast to char and then to
    // int to get the value that we should be getting.
    if (num_digits == 1)
    {
        val = (int)((char) val);
    }

    if (fatal_error)
    {
        return false;
    }

    value->value = val;
    value->error = error;
    value->is_wide = wide;

    return true;
}

// Note that decode escape sequence can probably be reused in parsing our
// string literals to reduce code size

static bool parse_string_literal_internal(DiagnosticManager* dm, Buffer* buffer,
        Token* token)
{
    assert(token->type == TOK_STRING);

    const Location loc = token->loc;

    const String to_convert = token->data.literal->value;
    const char* raw = to_convert.ptr;
    const size_t len = to_convert.len;

    assert(raw[0] == '"' && raw[len - 1] == '"');

    bool error = false;
    size_t pos = 1;
    do
    {
        char current = raw[pos++];

        if (current == '\\')
        {
            // TODO: do we have to add in an extra string (bool) parameter?
            // TODO: since this will error hard if it doesn't like the UCN in a
            // TODO: char context, yet they are allowed in the string context in
            // TODO: both clang and GCC :/
            EscapeSequenceResult result;
            uint64_t escape_sequence = decode_escape_sequence(raw, len - 1,
                    &pos, false, dm, loc, &result);
            if (result != ESCAPE_SEQUENCE_RESULT_OK)
            {
                // error = true;
            }

            if (escape_sequence > CHAR_MAX)
            {
                escape_sequence = CHAR_MAX;
            }

            current = (char) escape_sequence;
        }

        buffer_add_char(buffer, current);
    } while (pos < len - 1);

    return !error;
}

bool parse_string_literal(AstAllocator* allocator, StringLiteral* value,
        DiagnosticManager* dm, TokenVector tokens, LocationVector locs,
        bool wide)
{
    assert(token_vector_size(&tokens) == location_vector_size(&locs));

    if (wide)
    {
        diagnostic_error_at(dm, location_vector_get(&locs, 0),
                "cannot parse wide string literal at this time");
        diagnostic_help_at(dm, location_vector_get(&locs, 0), 
                "this needs to be implemented");
        diagnostic_note_at(dm, location_vector_get(&locs, 0), 
                "this needs to be implemented");
        return false;
    }

    const size_t num_strings = token_vector_size(&tokens);

    // Create the buffer to store our strings.
    Buffer string = buffer_new();
    for (size_t i = 0; i < num_strings; i++)
    {
        Token token = token_vector_get(&tokens, i);

        bool success = parse_string_literal_internal(dm, &string, &token);

        if (!success)
        {
            // TODO: also free buffer memory
            buffer_free(&string);
            return false;
        }
    }

    // Finish creating our buffer and print it to the terminal for now
    buffer_make_cstr(&string);

    size_t string_len = buffer_get_len(&string);
    char* ast_string = ast_allocator_alloc(allocator, string_len + 1);
    snprintf(ast_string, string_len + 1, "%s", buffer_get_ptr(&string));
    buffer_free(&string);

    value->string = (String) {ast_string, string_len};
    value->wide = false;
    value->error = false;

    return true;
}

