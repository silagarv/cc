#include "static_string.h"

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#include "util/panic.h"
#include "util/xmalloc.h"

// For when we have already allocated the string
void static_string_init(StaticString* dest, char* val)
{
    dest->ptr = val;
    dest->len = strlen(val);
}

// For when we have a constant string
void static_string_init_copy(StaticString* dest, const char* val)
{
    const size_t len = strlen(val);

    char* new_val = xmalloc(sizeof(char) * (len + 1));
    strcpy(new_val, val);

    dest->ptr = new_val;
    dest->len = len;
}

void static_string_init_len(StaticString* dest, char* val, size_t len)
{
    dest->ptr = val;
    dest->len = len;
}

void static_string_copy(const StaticString* src, StaticString* dest)
{
    // Lazy but should work
    static_string_init_copy(dest, src->ptr);
}

// the ptr will always be some allocated value so no need to worry
void static_string_free(StaticString* str)
{
    free(str->ptr);
}

char* static_string_get_ptr(const StaticString* str)
{
    return str->ptr;
}

size_t static_string_get_len(const StaticString* str)
{
    return str->len;
}

char static_string_get(const StaticString* str, size_t idx)
{
    if (idx >= str->len)
    {
        panic("static string attempted out of bounds access");
    }

    return str->ptr[idx];
}
