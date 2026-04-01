#ifndef AST_ALLOCATOR_H
#define AST_ALLOCATOR_H

#include "util/arena.h"

typedef struct AstAllocator {
    Arena arena;
} AstAllocator;

AstAllocator ast_allocator_create(void);
void ast_allocator_delete(AstAllocator* allocator);

void* ast_allocator_alloc(AstAllocator* allocator, size_t size);

#endif /* AST_ALLOCATOR_H */
