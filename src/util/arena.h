#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>

#define ARENA_DEFAULT_CHUNK_SIZE (4096)
#define ARENA_DEFAULT_ALIGNMENT (16)

typedef struct ArenaChunk {
    char* base_ptr;
    size_t used;
    size_t capacity;

    struct ArenaChunk* next;
} ArenaChunk;

typedef struct Arena {
    size_t chunk_size;
    size_t alignment;

    ArenaChunk* first;
    ArenaChunk* current;
} Arena;

Arena arena_new(size_t chunk_size, size_t alignment);
void arena_delete(Arena* arena);

void arena_reset(Arena* arena);

void* arena_allocate_size(Arena* arena, size_t size);

#endif /* ARENA_H */
