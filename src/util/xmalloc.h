#ifndef XMALLOC_H
#define XMALLOC_H

#include <stddef.h>

/* memory allocation functions that return or fail... */

void* xmalloc(size_t size);

void* xcalloc(size_t nmenb, size_t size);

void* xrealloc(void* ptr, size_t size);

#endif /* XMALLOC_H */
