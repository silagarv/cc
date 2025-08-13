#ifndef HASH_MAP_H
#define HASH_MAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

// A structure to represent a hashmap entry
typedef struct HashMapEntry {
    void* key;
    uint32_t hash;
    void* data;
} HashMapEntry;

// Functions that are meant for hashing of a key and freeing of data withina a
// hashtable when deleted.
//
// The hashfunction should be easily able to accept the key pointer and cast it
// as needed so that it can produce a u32 hash
//
// The key equal function accepts 2 keys and determines if the keys are equal to
// each other returning true if they are and false if they are not. This is so
// that duplicate entries cannot be inserted into the table
//
// The free function should easily be able to accept the data pointer and then
// free all of the memory ascociated with it. Note that the data pointer should
// also free the key pointer so there is some restriction on how the hashtable
// will need to operate. This is so that no memory can be leaked when the table
// is deleted.
typedef uint32_t (*HashFunction)(const void* key);
typedef bool (*KeyCompareFunction)(const void* key1, const void* key2);
typedef void (*FreeFunction)(void* key, void* data);

// A hashtable structure that we will use to store our entries into it
typedef struct HashMap {
    // The actual entries within the hashmap itself
    HashMapEntry* entries;
    size_t num_entries;
    size_t cap_entries;

    // Functions to hash and free entries from the hashmap
    HashFunction hash_func;
    KeyCompareFunction key_eq_func;
    FreeFunction free_func;
} HashMap;

// Create a hashmap with the following hashfunction and corrosponding free
// function
HashMap hash_map_create(HashFunction hash, KeyCompareFunction eq, FreeFunction free);

// Delete a hashmap including all of the entries within it
void hash_map_delete(HashMap* map);

// Test if the hashmap already contains that entry
bool hash_map_contains(HashMap* map, void* key);

// Get the data from a hashmap if it contains it otherwise return NULL
void* hash_map_get(HashMap* map, void* key);

// Insert an item into the hashmap if it does not already contain the item. If
// the map contains the item, return the data for the original item but do not
// insert the data into the hashmap
void* hash_map_insert(HashMap* map, void* key, void* data);

// Attempt to insert an item into the hash map even if it already contains the
// item. If the item is in the map, then we return the data for the original
// item but replace it with the new items data.
void* hash_map_insert_force(HashMap* map, void* key, void* data);

// Attempt to remove an element from the hashmap if it exists in the map, and 
// free the memory ascociated with the key using the hash_map free function
void hash_map_remove(HashMap* map, void* key);

#endif /* HASH_MAP_H */
