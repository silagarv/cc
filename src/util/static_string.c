#include "static_string.h"

#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"

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

void static_string_copy_len(const StaticString* src, StaticString* dest, 
        size_t len)
{
    dest->ptr = xmalloc(sizeof(char) * (len + 1));
    strncpy(dest->ptr, src->ptr, len);
    dest->ptr[len] = '\0';
    dest->len = len;
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

void static_string_from_buffer(StaticString* dest, Buffer* src)
{
    dest->ptr = buffer_get_ptr(src);
    dest->len = buffer_get_len(src);
}

bool static_string_equal(const StaticString* str1, const char* str2)
{
    return (strcmp(str1->ptr, str2) == 0);
}

bool static_string_starts_with(const StaticString* str, const char* starting)
{
    size_t starting_len = strlen(starting);
    return (strncmp(str->ptr, starting, starting_len) == 0);
}

void static_string_list_init(StaticStringList* list)
{
    *list = (StaticStringList)
    {
        .strings = xmalloc(sizeof(StaticString)),
        .used = 0,
        .allocated = 1
    };
}

void static_string_list_init_len(StaticStringList* list, size_t len)
{
    *list = (StaticStringList)
    {
        .strings = xmalloc(sizeof(StaticString) * len),
        .used = 0,
        .allocated = len
    };
}

void static_string_list_free(StaticStringList* list)
{
    const size_t len = list->used;
    for (size_t i = 0; i < len; i++)
    {
        static_string_free(&list->strings[i]);
    }

    if (list->strings)
    {
        free(list->strings);
    }
}

size_t static_string_list_length(StaticStringList* list)
{
    return list->used;
}

size_t static_string_list_capacity(StaticStringList* list)
{
    return list->allocated;
}

StaticString* static_string_list_get(StaticStringList* list, size_t i)
{
    if (i >= list->used)
    {
        panic("attempted to get out of range string");
    }

    return &list->strings[i];
}

static void static_string_list_expand(StaticStringList* list)
{
    list->allocated *= 2;
    list->strings = xrealloc(list->strings, 
            sizeof(StaticString) * list->allocated);
}

StaticString* static_string_list_get_next_free(StaticStringList* list)
{
    if (list->used == list->allocated)
    {
        static_string_list_expand(list);
    }

    return &list->strings[list->used++];
}

void static_string_list_push_back(StaticStringList* list, StaticString* str)
{
    if (list->used == list->allocated)
    {
        static_string_list_expand(list);
    }

    list->strings[list->used++] = *str;
}
