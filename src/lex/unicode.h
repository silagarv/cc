#ifndef UNICODE_H
#define UNICODE_H

#include <stdint.h>
#include <stdbool.h>

#include "util/buffer.h"

#include "lex/char_help.h"

bool is_valid_utf32(utf32 value);
void ucn_add_to_buffer(utf32 value, Buffer* to_add);

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end,
        utf32* value);


#endif /* UNICODE_H */
