#include "unicode.h"

#include <stddef.h>
#include <assert.h>
#include <stdint.h>

#include "lex/char_help.h"
#include "util/panic.h"

#define UTF8_REPLACEMENT_CHAR (0xFFFD)

// TODO: make a working unicode converter

static size_t get_num_bytes(unsigned char starting_char)
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

static bool is_legal_utf8(unsigned char* start, size_t length)
{
    // TODO: go through and make sure that we get valid utf-8
    


    return false;
}

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end, utf32* value)
{
    const unsigned char* starting_ptr = *current_ptr;

    const size_t num_bytes = get_num_bytes(*starting_ptr);

    // Check that we have enough bytes available for conversion
    if ((uintptr_t)(end - starting_ptr) < num_bytes)
    {
        panic("not enough bytes available for conversion");

        *value = UTF8_REPLACEMENT_CHAR;

        return false;
    }

    // Okay we have enough available we now need to convert the value to a
    // codepoint which we will need to shift bytes and stuff for
    switch (num_bytes)
    {
        case 1: 
            *value = *starting_ptr; 
            return true;
        
        case 2:
            panic("TODO");

            return true;

        case 3:
            panic("TODO");
            
            return true;

        case 4:
            panic("TODO");
            
            return true;

        default: panic("unreachable"); return false;
    }
}


