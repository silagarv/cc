#ifndef HASH_H
#define HASH_H

#include <stddef.h>
#include <stdint.h>

#include "util/str.h"

uint32_t cstring_get_hash(const char* ptr, size_t length);
uint32_t string_get_hash(const String* string);

#endif /* HASH_H */
