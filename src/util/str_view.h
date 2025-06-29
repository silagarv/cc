#ifndef STR_VIEW_H
#define STR_VIEW_H

#include <stddef.h>
#include <stdbool.h>

typedef struct StringView {
    char* start;
    size_t len;
} StringView;

#define string_view_get_ptr(view) ((view)->start)
#define string_view_get_len(view) ((view)->len)

StringView string_view(const char* string, size_t len);

bool string_view_equals(StringView* view, const char* string);

#endif /* STR_VIEW_H */
