#include "vector.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "panic.h"
#include "xmalloc.h"

Vector* vector_new(size_t elem_size, size_t start_capacity)
{
    Vector* vec = xmalloc(sizeof(Vector));
    *vec = (Vector) 
    {
        .elems = xmalloc(elem_size * start_capacity),
        .elem_size = elem_size,
        .count = 0,
        .cap = start_capacity
    };

    return vec;
}

void vector_free(Vector* vec)
{
    free(vec->elems);
    free(vec);
}

size_t vector_get_capacity(Vector* vec)
{
    return vec->cap;
}

size_t vector_get_count(Vector* vec)
{
    return vec->count;
}

static void vector_resize(Vector* vec)
{
    vec->cap *= 2;
    vec->elems = xrealloc(vec->elems, vec->elem_size * vec->cap);
}

void vector_push(Vector* vec, void* elem)
{
    if (vector_get_count(vec) == vector_get_capacity(vec))
    {
        vector_resize(vec);
    }

    char* starting = ((char*) vec->elems) + vec->elem_size * vec->count;
    memcpy(starting, elem, vec->elem_size);

    vec->count++;
}

void* vector_pop(Vector* vec)
{
    if (!vector_get_count(vec))
    {
        panic("attempting to pop from empty vector");
    }

    vec->count--;

    // we now get from element that was just at the end...
    return vector_get(vec, vector_get_count(vec));
}

void* vector_get(Vector* vec, size_t pos)
{
    assert(pos < vector_get_count(vec));

    return ((char*) vec->elems + vec->elem_size * pos);
}
