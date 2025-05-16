#include "string_map.h"

#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

// String hashing will use the FNV1A hashing algorithm
#define FNV_OFFSET_BASIS_32 (0x811c9dc5)
#define FNV_PRIME_32 (0x01000193)

static uint32_t get_string_hash_len(const char* str, size_t len)
{
    uint32_t hash = FNV_OFFSET_BASIS_32;

    for (size_t i = 0; i < len; i++)
    {
        hash ^= str[i];
        hash *= FNV_PRIME_32;
    }

    return hash;
}

static uint32_t get_string_hash(const char* str)
{
    uint32_t hash = FNV_OFFSET_BASIS_32;

    for (char* iter = (char*) str; *iter; iter++)
    {
        hash ^= *iter; 
        hash *= FNV_PRIME_32;
    }

    // Here we will just do an assertion check for our sanity
    assert(hash == get_string_hash_len(str, strlen(str)));

    return hash;
}

StringMap* string_map_new(size_t start_cap)
{
    return NULL;
}

void string_map_delete(StringMap* map)
{
    
}

void* string_map_find_string(StringMap* map, char* string);
void* string_map_find_stringview(StringMap* map, char* string, size_t len);

bool string_map_insert(StringMap* map, char* string, void* value);




