#include "allocator.h"

#include <stddef.h>

#include "util/arena.h"

AstAllocator ast_allocator_create(void)
{
    AstAllocator allocator = (AstAllocator)
    {
        .arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE * 4, 
                ARENA_DEFAULT_ALIGNMENT)
    };

    return allocator;
}

void ast_allocator_delete(AstAllocator* allocator)
{
    arena_delete(&allocator->arena);
}

void* ast_allocator_alloc(AstAllocator* allocator, size_t size)
{
    return arena_malloc(&allocator->arena, size);
}
