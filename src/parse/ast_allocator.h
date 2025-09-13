#ifndef AST_ALLOCATOR_H
#define AST_ALLOCATOR_H

#include <stddef.h>

#include "util/arena.h"

typedef struct AstAllocator {
    Arena memory;
} AstAllocator;

AstAllocator ast_allocator_create(void);

void ast_allocator_delete(AstAllocator* allocator);

void* ast_allocator_alloc(AstAllocator* allocator, size_t size);

#endif /* AST_ALLOCATOR_H */
