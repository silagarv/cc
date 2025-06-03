#ifndef BUFFER_H
#define BUFFER_H

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

struct Buffer
{
    char* buffer;
    size_t len;
    size_t cap;
};
typedef struct Buffer Buffer;

Buffer* buffer_new_size(size_t start_cap);
Buffer* buffer_new(void);

void buffer_free(Buffer* buff);

Buffer* buffer_from_cstr(const char* string);

size_t buffer_get_len(Buffer* buff);
size_t buffer_get_cap(Buffer* buff);
char* buffer_get_ptr(Buffer* buff);

void buffer_set_len(Buffer* buff, size_t len);

void buffer_reset(Buffer* buff);

void buffer_add_char(Buffer* buff, char c);
void buffer_make_cstr(Buffer* buff);

char buffer_get(Buffer* buff, size_t idx);

// Read into a buffer from fp, returning true if >0 chars read
// and only reading as many chars as the buffer capacity...
bool buffer_read_from_file(Buffer* buff, FILE* fp);

void buffer_vprintf(Buffer* buff, const char* fmt, va_list args);
void buffer_printf(Buffer* buff, const char* fmt, ...);

Buffer* buffer_from_format(const char* fmt, ...);

#endif /* BUFFER_H */
