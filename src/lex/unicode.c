#include "unicode.h"

#include <stddef.h>
#include <assert.h>

#include "lex/char_help.h"

#define UTF8_REPLACEMENT_CHAR (0xFFFD)

// TODO: make a working unicode converter

static size_t get_num_bytes(unsigned char starting_char)
{
    
    // Need to determine how many bytes we will read from the stream so we
    // can accurately decode it
    

    return 0;
}

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end, utf32* value)
{




    return false;
}


