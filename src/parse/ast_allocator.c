#include "ast_allocator.h"

#include <stddef.h>

#include "util/arena.h"

AstAllocator ast_allocator_create(void)
{
    AstAllocator allocator = (AstAllocator)
    {
        .memory = arena_new(ARENA_DEFAULT_CHUNK_SIZE * 4, 
                ARENA_DEFAULT_ALIGNMENT)
    };

    return allocator;
}

void ast_allocator_delete(AstAllocator* allocator)
{
    arena_delete(&allocator->memory);
}

void* ast_allocator_alloc(AstAllocator* allocator, size_t size)
{
    return arena_allocate_size(&allocator->memory, size);
}
