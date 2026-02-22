#ifndef PP_CONDITIONAL_H
#define PP_CONDITIONAL_H

#include "util/arena.h"

#include "files/location.h"

typedef struct ConditionalStackEntry ConditionalStackEntry;

typedef struct ConditionalStack {
    Arena* pp_allocator;
    ConditionalStackEntry* entries;
} ConditionalStack;

ConditionalStack conditional_stack_create(Arena* pp_allocator);

bool conditional_stack_empty(const ConditionalStack* stack);
Location conditional_stack_location(const ConditionalStack* stack);

void conditional_stack_push(ConditionalStack* stack, Location location);
void conditional_stack_pop(ConditionalStack* stack);

#endif /* PP_CONDITIONAL_H */
