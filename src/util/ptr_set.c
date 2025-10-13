#include "ptr_set.h"

#include <stdint.h>

#include "util/hash_map.h"

static uint32_t pointer_set_hash(const void* ptr)
{
    // TODO: this could be improved to be a bit better...
    return (uint32_t) ((uintptr_t) ptr);
}

static bool pointer_set_compare(const void* key1, const void* key2)
{
    return key1 == key2;
}

PtrSet pointer_set_create(void)
{
    PtrSet set = (PtrSet)
    {
        hash_map_create(pointer_set_hash,
                pointer_set_compare,
                NULL)
    };

    return set;
}

void pointer_set_delete(PtrSet* set)
{
    hash_map_delete(&set->map);
}

bool pointer_set_contains(PtrSet* set, void* ptr)
{
    return hash_map_contains(&set->map, ptr);
}

void pointer_set_insert(PtrSet* set, void* ptr)
{
    hash_map_insert(&set->map, ptr, ptr);
}
