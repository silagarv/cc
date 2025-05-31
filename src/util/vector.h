#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdbool.h>

#define Vector_of(T) Vector

typedef struct Vector
{
    void* elems;
    size_t elem_size;
    size_t count;
    size_t cap;
} Vector;

Vector* vector_new(size_t elem_size, size_t start_capacity);
void vector_free(Vector* vec);

size_t vector_get_capacity(Vector* vec);
size_t vector_get_count(Vector* vec);

void vector_push(Vector* vec, void* elem);
void* vector_pop(Vector* vec);

void* vector_get(Vector* vec, size_t pos);

#endif /* VECTOR_H */
