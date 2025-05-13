#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>

typedef struct ArenaChunk {
    char* data;
    size_t pos;
    size_t cap;

    struct ArenaChunk* next;
} ArenaChunk;

typedef struct Arena {
    ArenaChunk* start;
    ArenaChunk* curr;
} Arena;

Arena arena_new(void);
void arena_delete(Arena* arena);

void arena_reset(Arena* arena);

void* arena_allocate(Arena* arena, size_t size);

char* arena_dup_string(Arena* arena, const char* str);
char* arena_dup_n_string(Arena* arena, const char* str, size_t strlen);

#endif /* ARENA_H */
