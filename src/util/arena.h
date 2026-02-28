#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>

#define ARENA_DEFAULT_CHUNK_SIZE (4096)
#define ARENA_DEFAULT_ALIGNMENT (16)

typedef struct ArenaChunk ArenaChunk;

typedef struct Arena {
    size_t chunk_size;
    size_t alignment;
    ArenaChunk* first;
    ArenaChunk* current;
} Arena;

Arena arena_new(size_t chunk_size, size_t alignment);
Arena arena_new_default(void);
void arena_delete(Arena* arena);

void* arena_malloc(Arena* arena, size_t size);

#endif /* ARENA_H */
