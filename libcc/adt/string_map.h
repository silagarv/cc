#ifndef STRING_MAP_H
#define STRING_MAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct StringMap {
    char** strings;
    void** values;
    size_t count;
    size_t cap;
    size_t resize;
} StringMap;

StringMap* string_map_new(size_t start_cap);
void string_map_delete(StringMap* map);

void* string_map_find_string(StringMap* map, char* string);
void* string_map_find_stringview(StringMap* map, char* string, size_t len);

bool string_map_insert(StringMap* map, char* string, void* value);

#endif /* STRING_MAP_H */
