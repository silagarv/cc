#include "buffer.h"

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>

#include "pp/location.h"
#include "core/panic.h"
#include "core/xmalloc.h"

#define BUFFER_START_SIZE (40)

Buffer* buffer_new(void)
{
    Buffer* buff = xmalloc(sizeof(Buffer));
    *buff = (Buffer)
    {
        .buffer = xmalloc(sizeof(char) * BUFFER_START_SIZE),
        .len = 0,
        .cap = BUFFER_START_SIZE
    };

    return buff;
}

Buffer* buffer_new_size(size_t start_cap)
{
    Buffer* buff = xmalloc(sizeof(Buffer));
    *buff = (Buffer)
    {
        .buffer = xmalloc(sizeof(char) * start_cap),
        .len = 0,
        .cap = start_cap
    };

    return buff;
}

Buffer buffer_new_stack(void)
{
    Buffer buff = (Buffer)
    {
        .buffer = xmalloc(sizeof(char) * BUFFER_START_SIZE),
        .len = 0,
        .cap = BUFFER_START_SIZE
    };

    return buff;
}

Buffer buffer_new_stack_size(size_t start_cap)
{
    Buffer buff = (Buffer)
    {
        .buffer = xmalloc(sizeof(char) * start_cap),
        .len = 0,
        .cap = start_cap
    };

    return buff;
}

void buffer_delete_stack(Buffer* buff)
{
    free(buff->buffer);
}

void buffer_delete(Buffer* buff)
{
    free(buff->buffer);
    free(buff);
}

size_t buffer_get_len(Buffer* buff)
{
    return buff->len;
}

size_t buffer_get_cap(Buffer* buff)
{
    return buff->cap;
}

char* buffer_get_ptr(Buffer* buff)
{
    return buff->buffer;
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

int buffer_get(Buffer* buff, size_t idx)
{
    if (idx >= buff->len)
    {
        panic("attempted buffer_get with out of range index");
    }

    return buff->buffer[idx];
}
