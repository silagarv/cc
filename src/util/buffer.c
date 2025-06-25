#include "buffer.h"

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "panic.h"

#include "panic.h"
#include "xmalloc.h"

#define BUFFER_START_SIZE (40)

Buffer buffer_new_size(size_t start_cap)
{
    Buffer buff = (Buffer)
    {
        .buffer = xmalloc(sizeof(char) * start_cap),
        .len = 0,
        .cap = start_cap
    };

    return buff;
}

Buffer buffer_new(void)
{
    return buffer_new_size(BUFFER_START_SIZE);
}

void buffer_free(Buffer* buff)
{
    free(buff->buffer);
}

Buffer buffer_from_cstr(const char* string)
{
    const size_t len = strlen(string);

    Buffer buffer = buffer_new_size(len + 1);
    sprintf(buffer_get_ptr(&buffer), "%s", string);

    return buffer;
}

static bool buffer_is_equal_internal(const char* str1, const char* str2, 
        size_t len)
{    
    return (strncmp(str1, str2, len) == 0);
}

bool buffer_is_equal(Buffer* buff1, Buffer* buff2)
{
    // First check the two lengths
    const size_t len1 = buffer_get_len(buff1);
    const size_t len2 = buffer_get_len(buff2);

    if (len1 != len2)
    {
        return false;
    }

    const char* str1 = buffer_get_ptr(buff1);
    const char* str2 = buffer_get_ptr(buff2);

    return buffer_is_equal_internal(str1, str2, len1);
}

bool buffer_equals_str(Buffer* buff, const char* str)
{
    const size_t len1 = buffer_get_len(buff);
    const size_t len2 = strlen(str);

    if (len1 != len2)
    {
        return false;
    }

    const char* str_buff = buffer_get_ptr(buff);

    return buffer_is_equal_internal(str_buff, str, len1);
}

void buffer_set_len(Buffer* buff, size_t len)
{
    assert(buff->len <= buff->cap);

    buff->len = len;
}

void buffer_reset(Buffer* buff)
{
    buff->len = 0;
}

static void buffer_resize(Buffer* buff)
{
    buff->cap *= 2;
    buff->buffer = xrealloc(buff->buffer, sizeof(char) * buff->cap);
}

static void buffer_add_char_internal(Buffer* buff, char c, bool inc_len)
{
    if (buffer_get_len(buff) == buffer_get_cap(buff))
    {
        buffer_resize(buff);
    }

    buff->buffer[buff->len] = c;

    if (inc_len)
    {
        buff->len++;
    }
}

void buffer_add_char(Buffer* buff, char c)
{
    buffer_add_char_internal(buff, c, true);
}

void buffer_make_cstr(Buffer* buff)
{
    buffer_add_char_internal(buff, '\0', false);
}

char buffer_get(Buffer* buff, size_t idx)
{
    if (idx >= buff->len)
    {
        panic("attempted buffer_get with out of range index");
    }

    return buff->buffer[idx];
}

bool buffer_read_from_file(Buffer* buff, FILE* fp)
{
    const size_t read = fread(buff->buffer, sizeof(char), buff->cap, fp);

    buff->len = read;

    return (read > 0);
}

void buffer_vprintf(Buffer* buff, const char* fmt, va_list args)
{
    const size_t buffer_start_len = buff->len;

    while (true)
    {
        // Get the maxlen we can print and the starting ptr
        const size_t max_len = buff->cap - buff->len;
        char* print_start = buff->buffer + buffer_start_len;

        // Copy args, print and get the number of chars attemped to put in
        va_list args_copy;
        va_copy(args_copy, args);
        int printed = vsnprintf(print_start, max_len, fmt, args_copy);
        va_end(args_copy);

        // Check for encoding error
        if (printed < 0)
        {
            panic("unable to print into buffer in buffer_vprintf");
        }

        // the cast is okay since printed >= 0, check if we 'overran' buff
        if ((size_t) printed < max_len)
        {
            buff->len += (size_t) printed;
            break;
        }

        // If we 'overran' we need to resize and try again
        buffer_resize(buff);
    }
}

void buffer_printf(Buffer* buff, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    buffer_vprintf(buff, fmt, ap);
    va_end(ap);
}

Buffer buffer_from_format(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    Buffer buff = buffer_new();
    buffer_vprintf(&buff, fmt, ap);
    va_end(ap);
    return buff;
}
