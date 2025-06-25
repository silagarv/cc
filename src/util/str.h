#ifndef STR_H
#define STR_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "util/buffer.h"

struct String {
    char* ptr;
    size_t len;
};
typedef struct String String;

void string_init(String* dest, char* val);
void string_init_copy(String* dest, const char* val);
void string_init_len(String* dest, char* val, size_t len);
void string_init_char(String* dest, char val);

void string_free(String* str);

void string_copy(const String* src, String* dest);
void string_copy_len(const String* src, String* dest, 
        size_t len);

#define string_get_ptr(str) ((str)->ptr)
#define string_get_len(str) ((str)->len)

#define string_get(str, idx) string_get_ptr(str)[idx]

String string_from_buffer(Buffer* buff);

bool string_equal(const String* str1, const char* str2);
bool string_equal_string(const String* str1, const String* str2);
bool string_starts_with(const String* str, const char* starting);

uint32_t string_get_hash(const String* str);

#endif /* STR_H */
