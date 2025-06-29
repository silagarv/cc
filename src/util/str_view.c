#include "str_view.h"

#include <stddef.h>
#include <stdbool.h>
#include <string.h>

StringView string_view(const char* string, size_t len);

bool string_view_equals(StringView* view, const char* string)
{
    size_t string_len = strlen(string);

    if (view->len != string_len)
    {
        return false;
    }

    return (strncmp(view->start, string, view->len) == 0);
}
