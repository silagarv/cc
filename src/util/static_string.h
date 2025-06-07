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

struct StaticStringList {
    StaticString* strings;
    size_t used;
    size_t allocated;
};
typedef struct StaticStringList StaticStringList;

void static_string_init(StaticString* dest, char* val);
void static_string_init_copy(StaticString* dest, const char* val);
void static_string_init_len(StaticString* dest, char* val, size_t len);

void static_string_free(StaticString* str);

void static_string_copy(const StaticString* src, StaticString* dest);
void static_string_copy_len(const StaticString* src, StaticString* dest, 
        size_t len);

char* static_string_get_ptr(const StaticString* str);
size_t static_string_get_len(const StaticString* str);

char static_string_get(const StaticString* str, size_t idx);

void static_string_from_buffer(StaticString* dest, Buffer* src);

bool static_string_equal(const StaticString* str1, const char* str2);
bool static_string_starts_with(const StaticString* str, const char* starting);

void static_string_list_init(StaticStringList* list);
void static_string_list_init_len(StaticStringList* list, size_t len);
void static_string_list_free(StaticStringList* list);

size_t static_string_list_length(StaticStringList* list);
size_t static_string_list_capacity(StaticStringList* list);
StaticString* static_string_list_get(StaticStringList* list, size_t i);

StaticString* static_string_list_get_next_free(StaticStringList* list);
void static_string_list_push_back(StaticStringList* list, StaticString* str);

#endif /* STATIC_STRING_H */
