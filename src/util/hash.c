#include "hash.h"

#include <stddef.h>
#include <stdint.h>

#include "util/str.h"

#define FNV_32_OFFSET_BASIS (0x811c9dc5)
#define FNV_32_PRIME (0x01000193)

uint32_t string_get_hash(const String *str)
{
    uint32_t hash = FNV_32_OFFSET_BASIS;

    const size_t length = string_get_len(str);
    const char* ptr = string_get_ptr(str);

    for (size_t i = 0; i < length; i++)
    {
        hash ^= ptr[i];
        hash *= FNV_32_PRIME;
    }

    return hash;
}
