#ifndef UNICODE_H
#define UNICODE_H

#include <stdint.h>
#include <stdbool.h>

#include "util/buffer.h"

#include "lex/char_help.h"

#define UNICODE_MAX 0x10FFFF
#define UNICODE_SURROGATE_LOW 0xD800
#define UNICODE_SURROGATE_HIGH 0xDFFF

#define UTF8_REPLACEMENT_CHAR 0xFFFD

#define UTF8_1_CHAR_MIN 0x00
#define UTF8_1_CHAR_MAX 0x7F

#define UTF8_2_CHAR_MIN 0x80
#define UTF8_2_CHAR_MAX 0x7FF

#define UTF8_3_CHAR_MIN 0x800
#define UTF8_3_CHAR_MAX 0xFFFF

#define UTF8_4_CHAR_MIN 0x10000
#define UTF8_4_CHAR_MAX 0x10FFFF

bool is_valid_utf32(utf32 value);
bool utf32_is_control_char(utf32 value);

void utf32_to_buffer(utf32 value, unsigned char* buffer, size_t* used);
void ucn_add_to_buffer(utf32 value, Buffer* to_add);

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end,
        utf32* value);


#endif /* UNICODE_H */
