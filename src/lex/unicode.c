#include "unicode.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdint.h>

#include "lex/char_help.h"
#include "util/panic.h"

#define UTF8_REPLACEMENT_CHAR 0xFFFD

#define UTF8_1_CHAR_MIN 0x00
#define UTF8_1_CHAR_MAX 0x7F

#define UTF8_2_CHAR_MIN 0x80
#define UTF8_2_CHAR_MAX 0x7FF

#define UTF8_3_CHAR_MIN 0x800
#define UTF8_3_CHAR_MAX 0xFFFF

#define UTF8_4_CHAR_MIN 0x10000
#define UTF8_4_CHAR_MAX 0x10FFFF

// TODO: make a working unicode converter

static size_t utf8_get_num_bytes(unsigned char starting_char)
{
    
    // Need to determine how many bytes we will read from the stream so we
    // can accurately decode it

    // Byte starts for different number of characters
    // 1 byte: 0yyyzzzz -> mask = 0x80 -> value = 0x00
    // 2 byte: 110xxxyy -> mask = 0xE0 -> value = 0xC0
    // 3 byte: 1110wwww -> mask = 0xF0 -> value = 0xE0
    // 4 byte: 11110uvv -> mask = 0xF8 -> value = 0xF0

    if ((starting_char & 0x80) == 0)
    {
        return 1;
    }
    else if ((starting_char & 0xE0) == 0xC0)
    {
        return 2;
    }
    else if ((starting_char & 0xF0) == 0xE0)
    {
        return 3;
    }
    else if ((starting_char & 0xF8) == 0xF0)
    {
        return 4;
    }

    panic("invalid starting byte for utf8 conversion");

    // TODO: figure out the best course of action for the converter

    // For now just our invalid number
    return 5;
}

static bool utf8_is_valid(const unsigned char* start, size_t length, utf32 value)
{
    // TODO: go through and make sure that we get valid utf-8
    assert(length <= 4);
    
    if (value > 0x10FFFF)
    {
        panic("value is not a valid unicode codepoint");

        return false;
    }

    // TODO: implement enough error checking for the value
    

    return false;
}

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end, utf32* value)
{
    const unsigned char* starting_ptr = *current_ptr;

    const size_t num_bytes = utf8_get_num_bytes(*starting_ptr);

    // Check that we have enough bytes available for conversion
    if ((uintptr_t)(end - starting_ptr) < num_bytes)
    {
        panic("not enough bytes available for conversion");

        *value = UTF8_REPLACEMENT_CHAR;

        return false;
    }

    // Currently this 'decoder' simply assumes that the utf8 stream is valid
    // it also assumes that we will not overread the buffer (which should be
    // true since the above checks this)

    // TODO: we will want to check for invalid characters and any other
    // sequence which is illegal utf8 and replace it with the UTF8 replacement
    // character
    switch (num_bytes)
    {
        case 1:
            *value = starting_ptr[0] & 0x7F;
            break;
        
        case 2:
            *value = starting_ptr[0] & 0x1F;
            *value = (*value << 6) | (starting_ptr[1] & 0x3F);
            break;

        case 3:
            *value = starting_ptr[0] & 0x0F;
            *value = (*value << 6) | (starting_ptr[1] & 0x3F);
            *value = (*value << 6) | (starting_ptr[2] & 0x3F);
            break;

        case 4:
            *value = starting_ptr[0] & 0x07;
            *value = (*value << 6) | (starting_ptr[1] & 0x3F);
            *value = (*value << 6) | (starting_ptr[2] & 0x3F);
            *value = (*value << 6) | (starting_ptr[3] & 0x3F);
            break;

        default: panic("unreachable"); return false;
    }

    // TODO: make sure we modify *current_ptr so that we can correctly
    // synchronise the stream for the next character
    
    if (!utf8_is_valid(starting_ptr, num_bytes, *value))
    {
        panic("illegal utf8 in source");

        *value = UTF8_REPLACEMENT_CHAR;
    }

    return true;
}
