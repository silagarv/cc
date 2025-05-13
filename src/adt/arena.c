#include "arena.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "core/xmalloc.h"

#define DEFAULT_CHUNK_SIZE (4096)

static ArenaChunk* arena_chunk_new(size_t size)
{
    ArenaChunk* chunk = xcalloc(1, sizeof(ArenaChunk));
    *chunk = (ArenaChunk) {
        .data = xcalloc(size, sizeof(char)),
        .pos = 0,
        .cap = size,
        .next = NULL
    };

    return chunk;
}

static void arena_chunk_delete(ArenaChunk* chunk)
{
    assert(chunk && "chunk is NULL");

    free(chunk->data);
    free(chunk);
}

// return a pointer to the allocated memory or null on failure to allocate
static void* arena_chunk_allocate(ArenaChunk* chunk, size_t size)
{
    if (chunk->pos + size > chunk->cap)
    {
        return NULL;
    }

    void* ptr = chunk->data + chunk->pos;
    
    chunk->pos += size;

    return ptr;
}

Arena arena_new(void)
{
    Arena arena;

    arena.start = arena_chunk_new(DEFAULT_CHUNK_SIZE);
    arena.curr = arena.start;

    return arena;
}

void arena_delete(Arena* arena)
{
    ArenaChunk* curr = arena->start;
    ArenaChunk* tmp;
    while (curr)
    {
        tmp = curr;
        curr = curr->next;

        arena_chunk_delete(tmp);
    }

    arena->start = NULL;
    arena->curr = NULL;
}

void arena_reset(Arena* arena)
{
    ArenaChunk* curr = arena->start;
    while (curr)
    {
        curr->pos = 0;
        
        curr = curr->next;
    }

    // reset that starting point to the start
    arena->curr = arena->start;
}

void* arena_allocate(Arena* arena, size_t size)
{
    // the reason we need this loop is in the case we allocate multiple chunks
    // and then reset. In this case we keep all the chunks but we reset to the
    // starting chunk. If this happens we essentially need to go through them
    // This could possibly be bad if we have alot of chunks and a big size to
    // allocate.
    do 
    {
        // First try to allocate in the current chunk
        void* ptr = arena_chunk_allocate(arena->curr, size);
        if (ptr)
        {
            return ptr;
        }

        // then try to go to the next chunk (only if it exists)
        if (arena->curr->next)
        {
            arena->curr = arena->curr->next;
        }
        else // otherwise we have exhauseted all available options
        {
            break;
        }

    } while (1);

    // Here we need new memory
    assert(!arena->curr->next && "Arena has a next chunk but shouldn't");

    // Otherwise create a new chunk choosing size = max(DEFAULT, size);
    size_t new_chunk_size = (size > DEFAULT_CHUNK_SIZE) ? size : 
            DEFAULT_CHUNK_SIZE;

    // Get the new chunk and set it as the current chunk
    ArenaChunk* new_chunk = arena_chunk_new(new_chunk_size);
    arena->curr->next = new_chunk;
    arena->curr = arena->curr->next;

    // Now try again (should never fail here)
    void* ptr = arena_chunk_allocate(arena->curr, size);

    assert(ptr && "Arena failed to allocate on retry");

    return ptr;
}

char* arena_dup_string(Arena* arena, const char* str)
{
    assert(str && "Arena cannot dup a NULL string");

    // get the len we want to allocate (with '\0')
    size_t len = strlen(str) + 1;
    char* ptr = arena_allocate(arena, len);
    strcpy(ptr, str);
    
    return ptr;
}

char* arena_dup_n_string(Arena* arena, const char* str, size_t strlen)
{
    // We already have the length we want here...
    assert(str && "Arena cannot dup a NULL string");

    char* ptr = arena_allocate(arena, strlen);
    strncpy(ptr, str, strlen);
    ptr[strlen] = '\0';

    return ptr;
}
