#include "hash_map.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util/panic.h"
#include "util/xmalloc.h"

#define GRAVE_YARD_KEY ((void*) -1)

static HashMap hash_map_create_size(size_t size, HashFunction hash, 
        KeyCompareFunction eq, FreeFunction free)
{
    HashMap map = (HashMap)
    {
        .entries = xcalloc(size, sizeof(HashMapEntry)),
        .num_entries = 0,
        .cap_entries = size,

        .hash_func = hash,
        .key_eq_func = eq,
        .free_func = free
    };

    return map;
}

HashMap hash_map_create(HashFunction hash, KeyCompareFunction eq, FreeFunction free)
{
    return hash_map_create_size(16, hash, eq, free);
}

// Delete a hashmap including all of the entries within it
void hash_map_delete(HashMap* map)
{
    // iterate over the entire map and free any entries that are within the map.
    // noting that if we have a grave yard entry then that means that it's memory
    // has already been free'd by the free function
    for (size_t i = 0; i < map->cap_entries; i++)
    {
        if (map->entries[i].key != NULL && map->entries[i].key != GRAVE_YARD_KEY)
        {
            map->free_func(map->entries[i].key, map->entries[i].data);
        }
    }

    free(map->entries);
}

// Test if the hashmap already contains that entry
bool hash_map_contains(HashMap* map, void* key)
{
    // If we get a value from the map and it is null then we don't have the key
    // in the map, otherwise if it is non-null we have it
    return (hash_map_get(map, key) != NULL);
}

// Get the data from a hashmap if it contains it otherwise return NULL
void* hash_map_get(HashMap* map, void* key)
{
    // Get our hash and starting position
    const uint32_t key_hash = map->hash_func(key);
    size_t position = key_hash % map->cap_entries;

    // Get some starting position
    HashMapEntry entry = map->entries[position];
    while (entry.key != NULL)
    {
        // Check if we got a grave yard key, if so get the next entry
        if (entry.key == GRAVE_YARD_KEY)
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];
            
            continue;
        }

        // First check if the hash is not equal to the key_hash, and if it is
        // then check that the key's are not equal. So if the key is not equal
        // then skip this entry, otherwise just go through
        if (entry.hash != key_hash || !map->key_eq_func(key, entry.key))
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // If we're here we have got the same keys for the entries so we know
        // that the map contains the entry
        return entry.data;
    }

    return NULL;
}

void hash_map_resize(HashMap* map)
{
    HashMap new_map = hash_map_create_size(map->cap_entries * 2, map->hash_func, 
            map->key_eq_func, map->free_func);

    // Go through and add each of the old entries into the hash_map
    for (size_t i = 0; i < map->cap_entries; i++)
    {
        if (map->entries[i].key != NULL && map->entries[i].key != GRAVE_YARD_KEY)
        {
            hash_map_insert(&new_map, map->entries[i].key, map->entries[i].data);
        }
    }

    // Free the original array (but not the entries)
    free(map->entries);

    // Now reassign map to be new_map
    *map = new_map;
}

void* hash_map_insert(HashMap* map, void* key, void* data)
{
    // We cannot have null data inserted otherwise we will fail in other things
    // and possibly get weird results
    assert(data != NULL);

    if (map->num_entries > map->cap_entries / 2)
    {
        hash_map_resize(map);
    }

    // Get our hash and starting position
    const uint32_t key_hash = map->hash_func(key);
    size_t position = key_hash % map->cap_entries;

    // Get some starting position
    HashMapEntry entry = map->entries[position];

    // Note that since the map can only be up to half full we will never not
    // have an empty space in the map. So this will never infinitely loop...
    while (entry.key != NULL)
    {
        // Check if we got a grave yard key, if so get the next entry. Need to
        // check this first so key_eq_func doesn't get passed a bad pointer
        if (entry.key == GRAVE_YARD_KEY)
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // Check if the keys are equal if so simply return the data for that
        // entry and do not insert the data into the map
        if (entry.hash == key_hash && map->key_eq_func(key, entry.key))
        {
            return entry.data;
        }

        // Should never reach here...
        panic("unreachable");
    }

    // Here we have found a nice place to insert the key
    assert(map->entries[position].key == NULL);

    // Insert the entry into the map
    map->entries[position] = (HashMapEntry)
    {
        .key = key,
        .hash = key_hash,
        .data = data
    };

    map->num_entries++;

    // Return the data since we just inserted that into the map
    return data;
}

void* hash_map_insert_force(HashMap* map, void* key, void* data)
{
    // We cannot have null data inserted otherwise we will fail in other things
    // and possibly get weird results
    assert(data != NULL);

    if (map->num_entries > map->cap_entries / 2)
    {
        hash_map_resize(map);
    }

    // Get our hash and starting position
    const uint32_t key_hash = map->hash_func(key);
    size_t position = key_hash % map->cap_entries;

    // Get some starting position
    HashMapEntry entry = map->entries[position];

    // Note that since the map can only be up to half full we will never not
    // have an empty space in the map. So this will never infinitely loop...
    while (entry.key != NULL)
    {
        // Check if we got a grave yard key, if so get the next entry. Need to
        // check this first so key_eq_func doesn't get passed a bad pointer
        if (entry.key == GRAVE_YARD_KEY)
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // Check if the keys are equal if so simply return the data for that
        // entry and do not insert the data into the map
        if (entry.hash == key_hash && map->key_eq_func(key, entry.key))
        {
            // Get the old data, replace it and return the old data
            void* old_data = entry.data;

            // Replace it using the pointer to the original key not the key
            // given in the function.
            map->entries[position] = (HashMapEntry)
            {
                .key = entry.key,
                .hash = key_hash,
                .data = data
            };

            return old_data;
        }

        // Should never reach here...
        panic("unreachable");
    }

    // Here we have found a nice place to insert the key
    assert(map->entries[position].key == NULL);

    // Insert the entry into the map
    map->entries[position] = (HashMapEntry)
    {
        .key = key,
        .hash = key_hash,
        .data = data
    };

    map->num_entries++;

    // Return the data since we just inserted that into the map
    return data;
}

void hash_map_remove(HashMap* map, void* key)
{
    // Get our hash and starting position
    const uint32_t key_hash = map->hash_func(key);
    size_t position = key_hash % map->cap_entries;

    HashMapEntry entry = map->entries[position];
    while (entry.key != NULL)
    {
        // Check if we got a grave yard key, if so get the next entry. Need to
        // check this first so key_eq_func doesn't get passed a bad pointer
        if (entry.key == GRAVE_YARD_KEY)
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // Check if the entry is not equal to the key
        if (entry.hash != key_hash || !map->key_eq_func(key, entry.key))
        {
            position = (position + 1) % map->cap_entries;
            entry = map->entries[position];

            continue;
        }

        // If we're here the following must be true...
        assert(entry.hash == key_hash && map->key_eq_func(key, entry.key));
    }

    // If we're here then we didn't find the entry or we found no entries at all
    // so we can safely return from the function
    if (entry.key == NULL)
    {
        return;
    }

    // Otherwise we found the entry

    // Free the entry within the map and set up the entry as a graveyard
    map->free_func(entry.key, entry.data);

    // set it up as a graveyard entry
    map->entries[position] = (HashMapEntry)
    {
        .key = GRAVE_YARD_KEY,
        .hash = 0,
        .data = NULL
    };
}
