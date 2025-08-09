#include "hash_map.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include "util/xmalloc.h"

#define GRAVE_YARD_ENTRY_KEY ((void*) -1)

HashMap hash_map_create(HashFunction hash, KeyCompareFunction eq, FreeFunction free)
{
    HashMap map = (HashMap)
    {
        .entries = xcalloc(32, sizeof(HashMapEntry)),
        .num_entries = 0,
        .cap_entries = 32,

        .hash_func = hash,
        .key_eq_func = eq,
        .free_func = free
    };

    return map;
}

// Delete a hashmap including all of the entries within it
void hash_map_delete(HashMap* map)
{
    // TODO: we need to iterate over the entire map and free any entries that
    // are within the map. 


    free(map->entries);
}

// Test if the hashmap already contains that entry
bool hash_map_contains(HashMap* map, void* key)
{
    // Get some important starting values
    const uint32_t key_hash = map->hash_func(key);
    size_t position = key_hash % map->cap_entries;

    // Get a starting entry within the map
    HashMapEntry entry = map->entries[position];
    
    // Loop until we get an empty position
    while (entry.key != NULL)
    {
        // Check if we got a grave yard or an empty key, if so get the next entry
        // and try again
        if (entry.key == GRAVE_YARD_ENTRY_KEY 
                || !map->key_eq_func(key, entry.key))
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // If we're here we have got the same keys for the entries so we know
        // that the map contains the entry
        return true;
    }

    return false;
}
