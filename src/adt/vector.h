#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdbool.h>

#include "core/xmalloc.h"

typedef struct Vector {
    void* elems;
    size_t elem_size;
    size_t count;
    size_t cap;
} Vector;

typedef struct VectorHeader {
    size_t elem_size;
    size_t len;
    size_t cap;
} VectorHeader;

// TODO: credit layec vector implementation more or less ripped from there
// https://github.com/laye-lang/laye-legacy/blob/
// b619041c8420a8abb1576dca5096a1744ae1a91b/lca/include/lca.h#L4

#define vector(T) T*

void* vector_new(size_t elem_size, size_t start_capacity);

// only deletes the pointer if items themselves are pointers mem is leaked
void vector_delete(void* vec);

#define vector_get_header(V) (((VectorHeader*)(V)) - 1)

#define vector_get_count(V) ((V) ? vector_get_header(V)->len : 0)

#define vector_set_count(V, count) \
    do \
    { \
        Vector_get_header(V)->len = count; \
    } while (0)

void vector_maybe_expand(void** vec);

#define vector_push(V, E) \
    do \
    { \
        vector_maybe_expand((void*) &V); \
        (V)[vector_get_count(V)] = E; \
        vector_get_header(V)->len++; \
    } while (0)

// TODO: above 

#define vector_pop(V) \
    do \
    { \
        if (vector_get_count(V)) \
        { \
            vector_get_header(V)->len--; \
        } \
    } while (0) \

#define vector_last(V) (V[vector_get_count(V) - 1])

#endif /* VECTOR_H */
