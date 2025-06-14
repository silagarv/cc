#include "xmalloc.h"

#include <stdlib.h>

#include "panic.h"

static void xmem_error(void)
{
    panic("failed to allocate memory");
}

void* xmalloc(size_t size)
{
    void* ptr = malloc(size);

    if (!ptr) 
    {
        xmem_error();
    }

    return ptr;
}

void* xcalloc(size_t nmenb, size_t size)
{
    void* ptr = calloc(nmenb, size);

    if (!ptr) 
    {
        xmem_error();
    }

    return ptr;
}

void* xrealloc(void* ptr, size_t size)
{
    void* ptr_new = realloc(ptr, size);
    
    if (!ptr_new) 
    {
        xmem_error();
    }

    return ptr_new;
}
