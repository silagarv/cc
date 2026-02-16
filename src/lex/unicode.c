#include "unicode.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stdint.h>

#include "lex/char_help.h"
#include "util/buffer.h"
#include "util/panic.h"

bool is_valid_utf32(utf32 value)
{
    if (value > UNICODE_MAX)
    {
        return false;
    }

    if (value >= UNICODE_SURROGATE_LOW && value <= UNICODE_SURROGATE_HIGH)
    {
        return false;
    }

    return true;
}

bool utf32_is_control_char(utf32 value)
{
    return value < 32;
}

void utf32_to_buffer(utf32 value, unsigned char* utf8buff, size_t* used)
{
    assert(is_valid_utf32(value));
    
    if (value <= UTF8_1_CHAR_MAX)
    {
        *used = 1;
        utf8buff[0] = (unsigned char) value;
    }
    else if (value <= UTF8_2_CHAR_MAX)
    {
        *used = 2;
        utf8buff[0] = (unsigned char) (0xC0 | (value >> 6));
        utf8buff[1] = (unsigned char) (0x80 | (value & 0x3F));
    }
    else if (value <= UTF8_3_CHAR_MAX)
    {
        *used = 3;
        utf8buff[0] = (unsigned char) (0xE0 | (value >> 12));
        utf8buff[1] = (unsigned char) (0x80 | ((value >> 6) & 0x3F));
        utf8buff[2] = (unsigned char) (0x80 | (value & 0x3F));
    }
    else
    {
        *used = 4;
        utf8buff[0] = (unsigned char) (0xF0 | (value >> 18));
        utf8buff[1] = (unsigned char) (0x80 | ((value >> 12) & 0x3F));
        utf8buff[2] = (unsigned char) (0x80 | ((value >> 6) & 0x3F));
        utf8buff[3] = (unsigned char) (0x80 | (value & 0x3F));
    }
}

void ucn_add_to_buffer(utf32 value, Buffer* to_add)
{
    assert(is_valid_utf32(value));

    unsigned char utf8buff[4];
    size_t num_digits;
    utf32_to_buffer(value, utf8buff, &num_digits);

    for (size_t i = 0; i < num_digits; i++)
    {
        buffer_add_char(to_add, (char) utf8buff[i]);
    }
}

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

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end,
        utf32* value)
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

    // Don't forget to advance our pointer by the number of bytes in the UTF8
    // that we got.
    *current_ptr += num_bytes;

    // TODO: make sure we modify *current_ptr so that we can correctly
    // synchronise the stream for the next character
    if (!is_valid_utf32(*value))
    {
        panic("illegal utf8 in source");
        *value = UTF8_REPLACEMENT_CHAR;
    }

    return true;
}

