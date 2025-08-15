#ifndef VEC_H
#define VEC_H

#include <stddef.h>

#include "util/panic.h"
#include "util/xmalloc.h"

// This is our generic vector implementation. How this works is there are two
// macros which control this whole thing. There is the declaration macro which
// defines the type and all of the functions which might be used by the vector.
// and there is also the vector implementation, which contains all of the code
// used by the vector.
//
// In order to use this there must the a .c/.h pair so that the functions can 
// be both declared and defined. If this is not the case then the vector will
// not correctly exits.
//
// Additionally the vector code contains checks which will crash program if they
// fail.

#define vector_of_decl(T, s_name, f_name) \
typedef struct s_name ## Vector { \
    T* data; \
    size_t count; \
    size_t cap; \
} s_name ## Vector; \
 \
s_name ## Vector f_name ## _vector_create(size_t cap); \
void f_name ## _vector_free(s_name ## Vector* vec, void (*free_func)(T)); \
size_t f_name ## _vector_size(const s_name ## Vector* vec); \
size_t f_name ## _vector_capacity(const s_name ## Vector* vec); \
void f_name ## _vector_push(s_name ## Vector* vec, T elem); \
T f_name ## _vector_pop(s_name ## Vector* vec); \
T f_name ## _vector_get(s_name ## Vector* vec, size_t index)

#define vector_of_impl(T, s_name, f_name) \
s_name ## Vector f_name ## _vector_create(size_t cap) \
{ \
    s_name ## Vector vector = (s_name ## Vector) \
    { \
        .data = xmalloc(sizeof(T) * cap), \
        .count = 0, \
        .cap = cap \
    }; \
    \
    return vector;\
} \
 \
void f_name ## _vector_free(s_name ## Vector* vec, void (*free_func)(T)) \
{ \
    if (free_func != NULL) \
    { \
        for (size_t i = 0; i < vec->count; i++) \
        { \
            free_func(vec->data[i]); \
        } \
    } \
    \
    free(vec->data); \
} \
 \
size_t f_name ## _vector_size(const s_name ## Vector* vec) \
{ \
    return vec->count; \
} \
 \
size_t f_name ## _vector_capacity(const s_name ## Vector* vec) \
{ \
    return vec->cap; \
} \
 \
void f_name ## _vector_push(s_name ## Vector* vec, T elem) \
{ \
    if (vec->count == vec->cap) \
    { \
        vec->cap *= 2; \
        vec->data = xrealloc(vec->data, sizeof(T) * vec->cap); \
    } \
    \
    vec->data[vec->count++] = elem; \
} \
 \
T f_name ## _vector_pop(s_name ## Vector* vec) \
{ \
    if (vec->count == 0) \
    { \
        panic("attempting to pop from 0 length vecotr"); \
    } \
    \
    return vec->data[--vec->count]; \
} \
 \
T f_name ## _vector_get(s_name ## Vector* vec, size_t index) \
{ \
    if (index >= vec->count) \
    { \
        panic("attempting to get out of bounds element of vector"); \
    } \
    \
    return vec->data[index]; \
}

#endif /* VEC_H */
