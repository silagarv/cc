#include "vector.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

#include "core/xmalloc.h"

#include <stdio.h>

static size_t vector_get_alloc_size(size_t elem_size, size_t capacity)
{
    return (sizeof(VectorHeader) + elem_size * capacity);
}

void* vector_new(size_t elem_size, size_t start_capacity)
{
    size_t to_alloc = vector_get_alloc_size(elem_size, start_capacity);
    VectorHeader* header = xmalloc(to_alloc);

    header->elem_size = elem_size;
    header->len = 0;
    header->cap = start_capacity;

    return (void*) (header + 1);
}

void vector_delete(void* vec)
{
    free(vector_get_header(vec));
}

static bool vector_should_expand(void** vec)
{
    VectorHeader* header = vector_get_header(*vec);
    return header->len == header->cap;
}

static void vector_do_expand(void** vec)
{
    VectorHeader* header = vector_get_header(*vec);

    // Double the vector size and realloc
    header->cap *= 2;
    size_t new_size = vector_get_alloc_size(header->elem_size, header->cap);

    header = xrealloc(header, new_size);

    // ensure the vector is set
    *vec = (void*) (header + 1);
}

void vector_maybe_expand(void** vec)
{
    if (vector_should_expand(vec))
    {
        vector_do_expand(vec);
    }
}
