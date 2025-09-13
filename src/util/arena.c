#include "arena.h"

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "util/xmalloc.h"

static size_t arena_round_up(size_t used, size_t align)
{
    assert((align & (align - 1)) == 0);

    return (used + (align - 1)) & ~(align - 1);
}

static ArenaChunk* arena_chunk_new(size_t capacity, size_t alignment)
{
    // Here allocate alignment extra space so that we have space to burn
    ArenaChunk* chunk = xmalloc(sizeof(ArenaChunk));
    chunk->base_ptr = xmalloc(capacity);
    chunk->used = 0;
    chunk->capacity = capacity;
    chunk->next = NULL;

    // Check alignment of the base ptr here...
    if ((uintptr_t) chunk->base_ptr % alignment != 0)
    {
        chunk->used += alignment - ((uintptr_t) chunk->base_ptr % alignment);
    }

    assert(((uintptr_t) chunk->base_ptr + chunk->used) % alignment == 0);

    return chunk;
}

static void arena_chunk_delete(ArenaChunk* chunk)
{
    free(chunk->base_ptr);
    free(chunk);
}

static void arena_chunk_reset(ArenaChunk* chunk)
{
    chunk->used = 0;
}

Arena arena_new(size_t chunk_size, size_t alignment)
{
    // Check we got a power of 2
    assert((alignment & (alignment - 1)) == 0);
    
    // Non-zero chunk size
    assert(chunk_size);

    // Make sure we can sufficiently allocate
    assert(alignment < chunk_size);

    Arena arena;
    arena.chunk_size = chunk_size;
    arena.alignment = alignment;
    arena.first = arena_chunk_new(chunk_size, alignment);
    arena.current = arena.first;

    return arena;
}

void arena_delete(Arena* arena)
{
    ArenaChunk* current = arena->first;
    ArenaChunk* tmp;

    assert(current != NULL);

    do {
        tmp = current->next;

        arena_chunk_delete(current);

        current = tmp;
    } while (current != NULL);
}

void arena_reset(Arena* arena)
{
    ArenaChunk* current = arena->first;

    while (current != NULL)
    {
        // Set the use count to 0
        current->used = 0;

        current = current->next;
    }

    // Reset arena back to the first
    arena->current = arena->first;
}

void* arena_allocate_size(Arena* arena, size_t size)
{
    assert(arena->chunk_size >= size);

    void* return_ptr = NULL;

    // Check if the current arena can fit it
    if (arena->current->used + size < arena->current->capacity)
    {
        return_ptr = arena->current->base_ptr + arena->current->used;

        // Add to the size
        arena->current->used += size;
    }
    else
    {
        // Get a new chunk and allocate it there
        ArenaChunk* new_chunk = arena_chunk_new(arena->chunk_size, arena->alignment);

        arena->current->next = new_chunk;
        arena->current = new_chunk;

        arena->current->used += size;

        return_ptr = new_chunk->base_ptr;
    }

    size_t new_used = arena_round_up(arena->current->used, arena->alignment);

    assert(new_used >= arena->current->used);

    arena->current->used = new_used;

    // TODO: what if alignment requirements are more strict that what malloc gives?
    // Just do a sanity check to be 100% sure we got the alignment for previous
    assert(((uintptr_t) return_ptr % arena->alignment) == 0);

    return return_ptr;
}




