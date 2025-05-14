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

// the following is for giving tokens a location ID that can be easily mapped
// back to a real source line

typedef size_t LocationID;

typedef struct LocationEntry {
    LocationID id;
    SourceLocation location;
    SourceLocation real_location;
} LocationEntry;

typedef struct LocationMap {
    // vector(LocationEntry)
    Vector entries;
} LocationMap;

LocationMap location_map_create(void);
void location_map_delete(LocationMap* map);

LocationID location_map_insert(LocationMap* map, SourceLocation location, 
        SourceLocation real_location);

SourceLocation* location_map_real_location(LocationMap* map, LocationID id);
SourceLocation* location_map_location(LocationMap* map, LocationID id);

#endif /* LOCATION_H */
