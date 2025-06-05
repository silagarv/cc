#ifndef LOCATION_H
#define LOCATION_H

#include <stdint.h>
#include <stddef.h>

#include "preprocessor/line.h"

// an id we can use to track locations
typedef uint32_t Location;

#define LOCATION_INVALID (UINT32_MAX)

// A `real` location mapped from somewhere in the source
struct ResolvedLocation {
    const char* filename;
    uint32_t line;
    uint32_t col;
};
typedef struct ResolvedLocation ResolvedLocation;

struct LineLocation {
    const char* filename;
    uint32_t line;
};
typedef struct LineLocation LineLocation;

// typedef struct LocationInfo {
//     Location id;

//     ResolvedLocation source_loc;
//     ResolvedLocation real_source_loc;

//     Location from_id; // if from a macro where?
// } LocationInfo;

// struct LocationMapEntry {
//     // to go through all of the maps
//     struct LocationMapEntry* prev;
//     struct LocationMapEntry* next;

//     // to get the map that included this one
//     struct LoationMapEntry* parent_map;

//     // Store the lines within the map
//     Line* lines;
//     size_t lines_used;
//     size_t lines_allocated;

//     // the first id we handed out and the last id we handed out
//     Location start_id;
//     Location curr_id;

//     // the map which actually helps us hand out id's
//     struct LocationMap* overall_map;
// };
// typedef struct LocationMapEntry LocationMapEntry;

// struct LocationMap {
//     LocationMapEntry* first;
//     LocationMapEntry* last;
//     LocationMapEntry* current;

//     Location current_id;


// };
// typedef struct LocationMap LocationMap;

#endif /* LOCATION_H */
