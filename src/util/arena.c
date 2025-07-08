#include "arena.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "util/xmalloc.h"

#define NEW_USED(current_used, alignment) (((current_used) + ((alignment) - 1)) / (alignment)) * alignment

#include <stdio.h>

static ArenaChunk* arena_chunk_new(size_t capacity, size_t alignment)
{
    // Here allocate alignment extra space so that we have space to burn
    ArenaChunk* chunk = xmalloc(sizeof(ArenaChunk));
    chunk->base_ptr = xmalloc(capacity + alignment);
    chunk->used = 0;
    chunk->capacity = capacity;

    chunk->next = NULL;

    // Here we need to update used to be aligned we NEED the base ptr here since
    // chunk->used is 0 upon itialisation (i.e. always aligned to a multiple of anything)
    const size_t new_used = NEW_USED((uintptr_t) chunk->base_ptr + chunk->used, alignment);

    assert(new_used >= chunk->used);

    chunk->used = new_used;

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
    }

    // Fix arena alignment to make sure it is all good
    const size_t new_used = NEW_USED(arena->current->used, arena->alignment);

    assert(new_used >= arena->current->used);

    arena->current->used = new_used;

    // TODO: what if alignment requirements are more strict that what malloc gives?
    // Just do a sanity check to be 100% sure we got the alignment for previous
    assert(((uintptr_t) return_ptr % arena->alignment) == 0);

    return return_ptr;
}




