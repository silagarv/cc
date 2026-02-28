#include "arena.h"

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "util/xmalloc.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define ALIGNED_POW2(align) ((align & (align - 1)) == 0)

struct ArenaChunk {
    ArenaChunk* next;

    size_t size;
    size_t align;

    char* start;

    size_t off;
};

static size_t arena_round_up(size_t used, size_t align)
{
    assert(ALIGNED_POW2(align) && "Should align to a power of 2");
    return (used + (align - 1)) & ~(align - 1);
}

static ArenaChunk* arena_chunk_new(size_t size, size_t align)
{
    assert(ALIGNED_POW2(align) && "Should be aligned to a power of 2");

    // Round up the allocation size to be a multiple of the alignment so that
    // even if an allocation of size occurs in the region that we are able to
    // accomodate it even with the alignment.
    size = arena_round_up(size, align);

    ArenaChunk* region = xmalloc(sizeof(ArenaChunk));
    *region = (ArenaChunk)
    {
        .next = NULL,
        .size = size,
        .align = align,
        .start = xmalloc(sizeof(char) * size),
        .off = 0 
    };

    return region;
}

static ArenaChunk* arena_chunk_next(const ArenaChunk* current)
{
    assert(current != NULL && "Should have a current chunk");
    return current->next;
}

static void arena_chunk_set_next(ArenaChunk* current, ArenaChunk* next)
{
    assert(current->next == NULL && "Cannot have a next region!");
    current->next = next;
}

static void arena_chunk_delete(ArenaChunk* chunk)
{
    assert(chunk != NULL && "Can't free NULL chunk");
    xfree(chunk->start);
    xfree(chunk);
}

static void* arena_chunk_bump(ArenaChunk* region, size_t size)
{
    assert(region->off % region->align == 0 && "not aligned to a power of 2");
    assert(size != 0 && "Should not have a allocation size of 0");

    size_t size_with_align = arena_round_up(size, region->align);

    // No space for the allocation with the alignment will want to tell the
    // allocator to retry so give it NULL
    if (region->off + size_with_align > region->size)
    {
        return NULL;
    }

    // Get the pointer that we are going to return and increment the current 
    // offset of the region
    void* ptr = region->start + region->off;
    region->off += size_with_align;

    assert(region->off <= region->size && "Allocated too much size?");
    return ptr;
}

Arena arena_new(size_t chunk_size, size_t alignment)
{
    return (Arena) { chunk_size, alignment, NULL, NULL };
}

Arena arena_new_default(void)
{
    return arena_new(ARENA_DEFAULT_CHUNK_SIZE, ARENA_DEFAULT_ALIGNMENT);
}

void arena_delete(Arena* arena)
{
    ArenaChunk* current = arena->first;
    ArenaChunk* next = NULL;

    while (current != NULL)
    {
        next = arena_chunk_next(current);
        arena_chunk_delete(current);
        current = next;
    }
}

void* arena_malloc(Arena* arena, size_t size)
{
    assert(arena != NULL && "Can't allocate without an arena");

    // Test for the allocation of an empty size.
    if (size == 0)
    {
        return NULL;
    }

    // Okay we now need to get the current region potentially having to set up
    // a space of memory if the allocator has not been used yet.
    if (arena->current == NULL)
    {
        ArenaChunk* new_chunk = arena_chunk_new(arena->chunk_size,
                arena->alignment);
        arena->first = new_chunk;
        arena->current = new_chunk;
    }

    assert(arena->current != NULL && "need a region to allocate");
    
    // Okay now get the current region and simply try to do an easy bump to 
    // update it internally and its pointers
    ArenaChunk* region = arena->current;
    void* ptr = arena_chunk_bump(region, size);
    if (ptr != NULL)
    {
        return ptr;
    }

    // Okay we failed to allocate a region so we will need to create a new 
    // region with the specifier values and set this region to be the current
    // region we want to allocate for
    size_t new_region_size = MAX(arena->chunk_size, size);
    ArenaChunk* new_chunk = arena_chunk_new(new_region_size, arena->alignment);

    arena_chunk_set_next(arena->current, new_chunk);
    arena->current = new_chunk;

    // Then again try to do a region bump which should never fail since we 
    // should have allocated enough space for the allocation this time.
    ptr = arena_chunk_bump(new_chunk, size);
    assert(ptr != NULL && "Failed allocator on retry");
    return ptr;
}

