#ifndef LOCATION_MAP_H
#define LOCATION_MAP_H

#include <stdint.h>

#include "frontend/source.h"
#include "frontend/location.h"

typedef uint32_t LocationID;

#define LOCATIONID_MAX (UINT32_MAX)

// NOTE: we may want to expand this to a union that can handle macros in some
// way. But that is for the future for now
struct LocationEntry {
    LineID line;
    uint32_t col;
};
typedef struct LocationEntry LocationEntry;

struct LocationMap {
    LocationEntry* elems;
    size_t count;
    size_t cap;
};
typedef struct LocationMap LocationMap;

LocationMap* location_map_new(void);
void location_map_free(LocationMap* map);

LocationID location_map_add_location(LocationMap* map, LineID id, 
        uint32_t start_col);

LineID location_map_get_line(LocationMap* map, LocationID id);
uint32_t location_map_get_col(LocationMap* map, LocationID id);

#endif /* LOCATION_MAP_H */
