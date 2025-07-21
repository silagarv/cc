#include "str.h"

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"

// For when we have already allocated the string
void string_init(String* dest, char* val)
{
    dest->ptr = val;
    dest->len = strlen(val);
}

// For when we have a constant string
void string_init_copy(String* dest, const char* val)
{
    const size_t len = strlen(val);

    char* new_val = xmalloc(sizeof(char) * (len + 1));
    strcpy(new_val, val);

    dest->ptr = new_val;
    dest->len = len;
}

void string_init_len(String* dest, char* val, size_t len)
{
    dest->ptr = val;
    dest->len = len;
}

void string_init_char(String* dest, char val)
{
    char* buffer = xmalloc(sizeof(char) * 2);
    buffer[0] = val;
    buffer[1] = '\0';

    dest->ptr = buffer;
    dest->len = 1;
}

void string_copy(const String* src, String* dest)
{
    // Lazy but should work
    string_init_copy(dest, src->ptr);
}

void string_copy_len(const String* src, String* dest, 
        size_t len)
{
    dest->ptr = xmalloc(sizeof(char) * (len + 1));
    strncpy(dest->ptr, src->ptr, len);
    dest->ptr[len] = '\0';
    dest->len = len;
}

// the ptr will always be some allocated value so no need to worry
void string_free(String* str)
{
    free(str->ptr);
}

// char* string_get_ptr(const String* str)
// {
//     return str->ptr;
// }

// size_t string_get_len(const String* str)
// {
//     return str->len;
// }

// char string_get(const String* str, size_t idx)
// {
//     if (idx >= str->len)
//     {
//         panic("static string attempted out of bounds access");
//     }

//     return str->ptr[idx];
// }

String string_from_buffer(Buffer* buff)
{
    // Ensure that we have a valid c-string
    buff->buffer[buff->len] = '\0';

    char* ptr = buffer_get_ptr(buff);
    size_t len = buffer_get_len(buff);

    return (String) {.ptr = ptr, .len = len};
}

bool string_equal(const String* str1, const char* str2)
{
    return (strcmp(str1->ptr, str2) == 0);
}

bool string_equal_string(const String* str1, const String* str2)
{
    return (str1->len == str2->len && !strcmp(str1->ptr, str2->ptr));
}

bool string_starts_with(const String* str, const char* starting)
{
    size_t starting_len = strlen(starting);
    return (strncmp(str->ptr, starting, starting_len) == 0);
}

