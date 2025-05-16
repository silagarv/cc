#ifndef LOCATION_H
#define LOCATION_H

#include <stddef.h>
#include <stdint.h>

#include "adt/vector.h"

typedef struct LineLocation {
    char* filename;
    uint32_t line_no;
} LineLocation;

typedef struct SourceLocation {
    char* filename;
    uint32_t line_no;
    uint32_t col_no;
} SourceLocation;

typedef size_t LocationID;

typedef struct LocationEntry {
    LocationID id;
    SourceLocation location;
    SourceLocation real_location;
} LocationEntry;

// not really a map since can easily be accessed by ID
typedef struct LocationMap {
    Vector* entries;
} LocationMap;

LocationMap* location_map_create(void);
void location_map_delete(LocationMap* map);

LocationID location_map_insert(LocationMap* map, SourceLocation location, 
        SourceLocation real_location);

SourceLocation* location_map_real_location(LocationMap* map, LocationID id);
SourceLocation* location_map_location(LocationMap* map, LocationID id);

#endif /* LOCATION_H */
