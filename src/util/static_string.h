#ifndef STATIC_STRING_H
#define STATIC_STRING_H

#include <stddef.h>
#include <stdbool.h>

#include "util/buffer.h"

struct StaticString {
    char* ptr;
    size_t len;
};
typedef struct StaticString StaticString;

void static_string_init(StaticString* dest, char* val);
void static_string_init_copy(StaticString* dest, const char* val);
void static_string_init_len(StaticString* dest, char* val, size_t len);

void static_string_free(StaticString* str);

void static_string_copy(const StaticString* src, StaticString* dest);

char* static_string_get_ptr(const StaticString* str);
size_t static_string_get_len(const StaticString* str);

char static_string_get(const StaticString* str, size_t idx);

void static_string_from_buffer(StaticString* dest, Buffer* src);

bool static_string_equal(const StaticString* str1, const char* str2);
bool static_string_starts_with(const StaticString* str, const char* starting);

#endif /* STATIC_STRING_H */
