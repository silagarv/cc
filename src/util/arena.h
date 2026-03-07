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

// TODO: I think it would be good to be able to steal the chunks from an unused
// TODO: arena to create some kind of a free list in order to help reduce any
// TODO: memory allocation. Then the free list could be raided everytime an 
// TODO: arena needed another chunk. This might help reduce some pressure on
// TODO: malloc and free in the future.
// TODO: This would mainly be useful in the preprocessor, where we create alot 
// TODO: of these arenas and then free them quickly for only things like macro
// TODO: arguments which are relatively short lived.

#endif /* ARENA_H */
