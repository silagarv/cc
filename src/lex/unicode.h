#ifndef UNICODE_H
#define UNICODE_H

#include <stdint.h>
#include <stdbool.h>

#include "lex/char_help.h"

bool utf8_to_utf32(unsigned char** current_ptr, const unsigned char* end, utf32* value);

#endif /* UNICODE_H */
