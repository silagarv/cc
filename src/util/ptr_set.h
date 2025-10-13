#ifndef PTR_SET_H
#define PTR_SET_H

#include "util/hash_map.h"

typedef struct PtrSet {
    HashMap map;
} PtrSet;

PtrSet pointer_set_create(void);

void pointer_set_delete(PtrSet* set);

bool pointer_set_contains(PtrSet* set, void* ptr);

void pointer_set_insert(PtrSet* set, void* ptr);

#endif /* PTR_SET_H */
