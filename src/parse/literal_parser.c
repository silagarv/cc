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

#include "driver/lang.h"
#include "driver/warning.h"
#include "lex/unicode.h"
#include "parse/ast_allocator.h"
#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"

#include "files/location.h"

#include "driver/diagnostic.h"

#include "lex/char_help.h"
#include "lex/token.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef enum EscapeSequenceResult {
    ESCAPE_SEQUENCE_RESULT_OK,
    ESCAPE_SEQUENCE_RESULT_ERROR,
    ESCAPE_SEQUENCE_RESULT_FATAL
} EscapeSequenceResult;

CharType get_char_type(TokenType type)
{
    switch (type)
    {
        case TOK_STRING:
        case TOK_CHARACTER:
            return CHAR_TYPE_CHAR;

        case TOK_WIDE_STRING:
        case TOK_WIDE_CHARACTER:
            return CHAR_TYPE_WIDE;

        case TOK_UTF8_STRING:
        case TOK_UTF8_CHARACTER:
            return CHAR_TYPE_UTF8;

        case TOK_UTF16_STRING:
        case TOK_UTF16_CHARACTER:
            return CHAR_TYPE_UTF16;

        case TOK_UTF32_STRING:
        case TOK_UTF32_CHARACTER:
            return CHAR_TYPE_UTF32;

        default:
            panic("bad token type");
            return CHAR_TYPE_CHAR; // Dummy return.
    }
}

size_t get_char_type_size(CharType type)
{
    switch (type)
    {
        case CHAR_TYPE_CHAR:
        case CHAR_TYPE_UTF8:
            return 1;

        case CHAR_TYPE_UTF16:
            return 2;

        case CHAR_TYPE_WIDE:
        case CHAR_TYPE_UTF32:
            return 4;

        default: panic("bad CharType");
            return 0;
    }
}

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

char* string_literal_buffer(const StringLiteral* string)
{
    return string->string;
}

size_t string_literal_length(const StringLiteral* string)
{
    return string->length;
}

size_t string_literal_char_size(const StringLiteral* string)
{
    return string->char_size;
}

CharType string_literal_char_type(const StringLiteral* string)
{
    return string->type;
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
        diagnostic_warning_at(dm, location, Wother,
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
        diagnostic_warning_at(dm, location, Wother,
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
            // if (pos < len)
            // {
            //     if ((string[pos + 1] == '+' || string[pos + 1] == '-'))
            //     {
            //         return VALUE_FLOATING_TYPE;
            //     }

            //     // Isn't this for hex floats so we need to do something
            //     if (is_decimal(string[pos + 1]))
            //     {
            //         return VALUE_FLOATING_TYPE;
            //     }
            // }

            return VALUE_FLOATING_TYPE;
        }

        if (current == 'e' || current == 'E')
        {
            // if (pos < len)
            // {
            //     if (string[pos + 1] == '+' || string[pos + 1] == '-')
            //     {
            //         return VALUE_FLOATING_TYPE;
            //     }

            //     if (is_decimal(string[pos + 1]))
            //     {
            //         return VALUE_FLOATING_TYPE;
            //     }
            // }

            return VALUE_FLOATING_TYPE;
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

static utf32 decode_escape_sequence_new(DiagnosticManager* dm, Location loc,
        LangOptions* lang, char** current_ptr, char* end_ptr,
        size_t element_size, bool unevaluated, EscapeSequenceResult* okay)
{
    assert(element_size == 1 || element_size == 4);
    assert(*(*current_ptr - 1) == '\\');
    assert(*current_ptr != end_ptr);

    const uint64_t max_esacpe = element_size == 1 ? UINT8_MAX : UINT32_MAX;

    // Get the beginning of the escape sequene.
    char* const escape_begin = *current_ptr;

    // Get the next char and advance our ptr
    char current = *escape_begin;
    (*current_ptr)++;

    utf32 result;
    switch (current)
    {
        // TODO: add \e and \E escapes to this...

        // All of our simple escapes.
        case '\'': result = '\''; break;
        case '"':  result = '\"'; break;
        case '?':  result = '\?'; break;
        case '\\': result = '\\'; break;
        case 'a':  result = '\a'; break;
        case 'b':  result = '\b'; break;
        case 'f':  result = '\f'; break;
        case 'n':  result = '\n'; break;
        case 'r':  result = '\r'; break;
        case 't':  result = '\t'; break;
        case 'v':  result = '\v'; break;

        // Hexadecimal escapes
        case 'x':
        {
            // If we didn't get any hex digits that is not a fatal error for 
            // building our string somehow, just act like we got no digits
            if (!is_hexadecimal(**current_ptr))
            {
                diagnostic_error_at(dm, loc, "\\x used with no following hex "
                        "digits");
                *okay = ESCAPE_SEQUENCE_RESULT_ERROR;
                return 0;
            }

            // Otherwise we will try to parse the escape sequence
            uint64_t value = 0;
            while (*current_ptr != end_ptr && is_hexadecimal(**current_ptr))
            {
                // Get the new value and check we are in range. Note that since
                // max escape is much less greater than values' range we should
                // always detect overflow here
                value *= 16; 
                value += convert_character_base(**current_ptr, 16);
                if (value > max_esacpe)
                {
                    diagnostic_error_at(dm, loc, "hex escape sequence out of "
                            "range");
                    *okay = ESCAPE_SEQUENCE_RESULT_FATAL;
                    return 0;
                }

                // Get the next character
                (*current_ptr)++;
            }
            assert(value <= max_esacpe);

            result = (utf32) value;
            break;
        }

        // Octal escapes.
        case '0': case '1': case '2': case '3': 
        case '4': case '5': case '6': case '7':
        {
            // Currenttly we have skipped this char (the digit we're on) so we
            // need to undo that.
            (*current_ptr)--;

            int num_digits = 0;
            uint64_t value = 0;
            while (*current_ptr != end_ptr && is_octal(**current_ptr) 
                    && num_digits < 3)
            {
                value *= 8;
                value += convert_character_base(**current_ptr, 8);

                // Increment our digit count
                num_digits++;

                // Get the next character
                (*current_ptr)++;
            }

            // Finally set our result
            assert(value <= UINT32_MAX);
            result = (utf32) value;

            // Check if we are out of range here. This can only occur if we are
            // in 1 char mode. so recover based on that premise
            if (value > max_esacpe)
            {
                diagnostic_error_at(dm, loc, "octal escape sequence out of "
                        "range");
                *okay = ESCAPE_SEQUENCE_RESULT_ERROR;

                assert(element_size == 1);                
                result &= 0xFF; // This is equivalent to clangs recovery.
            }
            break;
        }

        // Possible universal character names
        case 'u':
        case 'U':
        {
            const int required = (current == 'u') ? 4 : 8;
            int num_digits = 0;

            uint32_t value = 0;
            while (*current_ptr != end_ptr && is_hexadecimal(**current_ptr)
                    && num_digits < required)
            {
                // Convert the value
                value *= 16;
                value += convert_character_base(**current_ptr, 16);

                // Increment the count on the number of digits
                num_digits++;

                // Go to the next digit
                (*current_ptr)++;
            }
            
            // Ensure that we have an adequet number of digits.
            if (num_digits != required)
            {
                diagnostic_error_at(dm, loc, "incomplete universal character "
                        "name");
                *okay = ESCAPE_SEQUENCE_RESULT_FATAL;
                return 0;
            }

            // Do the conversion.
            result = (utf32) value;

            // Finally, check UCN validity... is must not be invalid utf32 and 
            // must not be a control characer. 0-31 are control chars.
            if (!is_valid_utf32(result))
            {
                diagnostic_error_at(dm, loc,
                        "\\%c%0*X is not a valid universal character",
                        (num_digits == 4) ? 'u' : 'U', num_digits, value);
                *okay = ESCAPE_SEQUENCE_RESULT_ERROR;
                return result;
            }
            else if (utf32_is_control_char(result))
            {
                diagnostic_error_at(dm, loc, "universal character name referes "
                        "to a control character");
                *okay = ESCAPE_SEQUENCE_RESULT_FATAL;
                return result;
            }
            else if (!lang_opts_c99(lang))
            {
                diagnostic_warning_at(dm, loc, Wunicode, "universal character "
                        "names are only valid in C99");
            }
            break;
        }

        default:
            diagnostic_warning_at(dm, loc, Wunknown_escape_sequence,
                    "unknown escape sequence '\\%c'", current);
            *okay = ESCAPE_SEQUENCE_RESULT_ERROR;
            return current;
    }

    *okay = ESCAPE_SEQUENCE_RESULT_OK;
    return result;
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
            diagnostic_warning_at(dm, loc, Wunknown_escape_sequence,
                    "unknown escape sequence '\\%c'", current);
            *success = ESCAPE_SEQUENCE_RESULT_ERROR;

            return current;
    }
}

bool parse_char_literal(CharValue* value, DiagnosticManager* dm,
        const Token token)
{
    assert(token_is_character(&token));

    bool wide = token_is_type(&token, TOK_WIDE_CHARACTER);

    // Get the location and raw literal data for conversion
    const Location loc = token.loc;

    String literal = token_get_literal_node(&token);
    char* raw = literal.ptr;
    size_t len = literal.len;

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
        diagnostic_warning_at(dm, loc, Wmultichar, "multi-character character "
                "constant");
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
        diagnostic_warning_at(dm, loc, Wother, "character constant too long "
                "for its type");
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

const char* string_literal_prefix(TokenType type)
{
    switch (type)
    {
        case TOK_STRING: return "";
        case TOK_WIDE_STRING: return "L";
        case TOK_UTF8_STRING: return "u8";
        case TOK_UTF16_STRING: return "u";
        case TOK_UTF32_STRING: return "U";
        default: panic("bad token type"); return NULL;
    }
}

void buffer_add_character(char* buffer, size_t* buffer_pos, size_t buffer_size,
        size_t element_size, utf32 value)
{
    assert(element_size == 1 || element_size == 4);
    assert(*buffer_pos + element_size <= buffer_size);
    
    switch (element_size)
    {
        case 1:
        {
            // Fast case of us being an ASCII character.
            if (value < UTF8_1_CHAR_MAX)
            {
                buffer[(*buffer_pos)++] = (char) value;
            }
            else // Slow case of being multibyte utf8
            {
                unsigned char utf8_buffer[4];
                size_t num_chars;
                utf32_to_buffer(value, utf8_buffer, &num_chars);
                for (size_t i = 0; i < num_chars; i++)
                {
                    buffer[(*buffer_pos)++] = (char) utf8_buffer[i];
                }                
            }                        
            break;
        }

        case 2:
        {
            panic("unimplemented for UTF-16 literals (and short wide chars)");
            break;
        }

        case 4:
        {
            // Make sure we are doing the right thing position wide and make
            // sure we are doing aligned memory accesses.
            assert(*buffer_pos % 4 == 0);
            assert(((uintptr_t) buffer) % 4 == 0);

            // This is slightly evil but should definitely work for what we need
            utf32* utf32_buffer = (utf32*) buffer;
            utf32_buffer[*buffer_pos / 4] = value;
            *buffer_pos += 4;
            break;
        }

        default:
            panic("bad element size");
            break;
    }

    return;
}

bool parse_single_string_literal(const String* string, DiagnosticManager* dm,
        Location location, LangOptions* lang, CharType type, char* buffer,
        size_t* buffer_pos, size_t buffer_size, size_t element_size,
        bool unevaluated)
{
    assert(element_size == 1 || element_size == 4);

    // Skip the leading spaces at the start of the literal.
    char* current_ptr = string_get_ptr(string);
    if (*current_ptr == 'L' || *current_ptr == 'U')
    {
        assert(*(current_ptr + 1) == '"');
        current_ptr += 2;
    }
    else if (*current_ptr == 'u')
    {
        current_ptr += 1;
        if (*current_ptr == '8')
        {
            current_ptr += 1;
        }
        assert(*current_ptr == '"');
        current_ptr += 1;
    }
    else
    {
        assert(*current_ptr == '"');
        current_ptr++;
    }

    char* end_ptr = string_get_ptr(string) + string_get_len(string) - 1; // '"'
    assert(*end_ptr == '"');

    while (current_ptr != end_ptr)
    {
        if (*current_ptr != '\\')
        {
            // Decode the utf8 and then add that codepoint to the buffer.
            utf32 value;
            utf8_to_utf32((unsigned char**) &current_ptr,
                    (const unsigned char*) end_ptr, &value);
            buffer_add_character(buffer, buffer_pos, buffer_size, element_size,
                    value);
            continue;
        }

        // Skip the '\\'
        current_ptr++;
        assert(current_ptr != end_ptr);

        // Special case for handling universal character names and their
        // encodings since they wont be handled properly in strings if we do
        // what we are doing below
        bool ucn = false;
        if (*current_ptr == 'u' || *current_ptr == 'U')
        {
            ucn = true;
        }

        // Now We want to decode the character escape sequence and get the value
        // from it that we want and then add the decoded value into our string's
        // buffer.
        EscapeSequenceResult okay;
        utf32 value = decode_escape_sequence_new(dm, location, lang, 
                &current_ptr, end_ptr, element_size, unevaluated, &okay);
        if (okay == ESCAPE_SEQUENCE_RESULT_FATAL)
        {
            return false;
        }

        // TODO: element size can not be 2.

        // If we are just chars then truncate the given escape sequence.
        if (element_size == 1 && !ucn)
        {
            // Truncate the value that we get if doing single chars
            buffer[*buffer_pos] = value & 0xFF;
            (*buffer_pos)++;
        }
        else // Otherwise just attempt to add it to our buffer as normal
        {
            buffer_add_character(buffer, buffer_pos, buffer_size, element_size,
                    value);
        }
    }

    return true;
}

bool parse_string_literal(AstAllocator* allocator, StringLiteral* value,
        DiagnosticManager* dm, LangOptions* lang, const TokenList* tokens,
        bool unevaluated)
{
    // CharType type = CHAR_TYPE_CHAR;
    size_t upper_bound = 0; // upper bound of final string literal length
    size_t num_strings = 0;
    bool concat_error = false; // Have we got a concat error.

    TokenType type = TOK_STRING;

    TokenListEntry* start = token_list_iter(tokens);
    for (TokenListEntry* current = start;
            current != NULL;
            current = token_list_entry_next(current))
    {
        // Get basic info about the token
        Token curr_tok = token_list_entry_token(current);
        TokenType curr_type = token_get_type(&curr_tok);
        num_strings++;
        
        // Do the size calculation for the tokens length.
        size_t t_len = token_get_length(&curr_tok);
        assert(t_len >= 2);
        upper_bound += t_len - 2;
        
        // Check what kind of conversions we are doing with each string token.
        if (unevaluated && curr_type != TOK_STRING)
        {
            diagnostic_warning_at(dm, token_get_location(&curr_tok),
                    Wignored_encoding, "encoding prefix '%s' has no effect",
                    string_literal_prefix(curr_type));
        }
        else if (curr_type != type && curr_type != TOK_STRING)
        {
            if (type == TOK_STRING)
            {
                type = curr_type;
            }
            else
            {
                diagnostic_error_at(dm, token_get_location(&curr_tok),
                        "unsupported non-standard concatenation of string "
                        "literals");
                concat_error = true;
            }
        }
    }
    
    if (concat_error)
    {
        return false;
    }

    CharType char_type = get_char_type(type);

    // Make sure we are okay to null terminate the string.
    upper_bound++;

    // Okay we now have most of what we need from all of our string literals in
    // order to start building up our string literal. We just need to do some
    // things in order to set up or literal evaluation.
    size_t element_size = get_char_type_size(char_type);
    size_t alloc_size = upper_bound * element_size;

    // Allocate all of the space that we could possibly need in the buffer
    char* buffer = ast_allocator_alloc(allocator, sizeof(char) * alloc_size);
    size_t buffer_pos = 0;

    // Now we want to perform all of our string literal evaluation
    for (TokenListEntry* current = start;
            current != NULL;
            current = token_list_entry_next(current))
    {
        Token t = token_list_entry_token(current);
        String literal = token_get_literal_node(&t);
        if (!parse_single_string_literal(&literal, dm, token_get_location(&t),
                lang, char_type, buffer, &buffer_pos, alloc_size, element_size,
                unevaluated))
        {
            return false;
        }
    }

    // Finally add the null terminating character onto the end
    buffer_add_character(buffer, &buffer_pos, alloc_size, element_size, 0);

    // Finally since we have been successful in parsing all our other tokens
    // finally we can attempt to do this one.
    assert(buffer_pos % element_size == 0);
    value->string = buffer;
    value->length = buffer_pos / element_size;
    value->char_size = element_size;
    value->type = char_type;

    return true;
}

