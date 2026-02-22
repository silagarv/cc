#include "pp_conditional.h"

#include <assert.h>

#include "util/arena.h"

#include "files/location.h"

struct ConditionalStackEntry {
    ConditionalStackEntry* prev;
    Location location;
};

ConditionalStack conditional_stack_create(Arena* pp_allocator)
{
    ConditionalStack ret;
    ret.pp_allocator = pp_allocator;
    ret.entries = NULL;
    return ret;
}

bool conditional_stack_empty(const ConditionalStack* stack)
{
    return stack->entries == NULL;
}

Location conditional_stack_location(const ConditionalStack* stack)
{
    assert(!conditional_stack_empty(stack));
    return stack->entries->location;
}

void conditional_stack_push(ConditionalStack* stack, Location location)
{
    // Create the new entry and set it's previous to the stacks current.
    ConditionalStackEntry* entry = arena_allocate_size(stack->pp_allocator,
            sizeof(ConditionalStackEntry));
    entry->location = location;
    entry->prev = stack->entries;

    // Now set the stacks entry to this entry.
    stack->entries = entry;
}

void conditional_stack_pop(ConditionalStack* stack)
{
    assert(!conditional_stack_empty(stack));
    stack->entries = stack->entries->prev;
}
