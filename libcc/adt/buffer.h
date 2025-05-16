#ifndef BUFFER_H
#define BUFFER_H

#include <stddef.h>

typedef struct Buffer {
    char* buffer;
    size_t len;
    size_t cap;
} Buffer;

Buffer* buffer_new(void);
void buffer_delete(Buffer* buff);

size_t buffer_get_len(Buffer* buff);
size_t buffer_get_cap(Buffer* buff);
char* buffer_get_ptr(Buffer* buff);

void buffer_reset(Buffer* buff);

void buffer_add_char(Buffer* buff, char c);
void buffer_make_cstr(Buffer* buff);

int buffer_get(Buffer* buff, size_t idx);

// TODO: extend buffer to be able to handle filenames and other strings???

#endif /* BUFFER_H */
