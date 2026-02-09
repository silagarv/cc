#ifndef LINE_MAP_H
#define LINE_MAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/file_manager.h"
#include "files/location.h"

// A structure to represent a location triplet. I.e. the file id, raw line, and
// raw column. Note that this can then be used along with our mapping of our
// line table (for #line directives) to determine the accurate and precise
// location
typedef struct ResolvedLocation {
    uint32_t line;
    uint32_t col;
} ResolvedLocation;

// A structure that we will use to go from a files overall location and it's
// ranges back to a location triplet. This is eagerly built at the moment but
// eventually I want to be able to lazily build this structure. Also eventually
// we will want to fix some potential performace concerns with the current
// implementation.
typedef struct LineMap {
    LocationRange range; // Overall location range of the file
    LocationRangeVector ranges; // the different ranges in the line map
} LineMap;

LineMap line_map_create(FileBuffer* file, Location base_location);
void line_map_delete(LineMap* map);

ResolvedLocation line_map_resolve_location(const LineMap* map, Location loc);

uint32_t line_map_resolve_line(const LineMap* map, Location loc);
uint32_t line_map_resolve_column(const LineMap* map, Location loc);

// Get the starting location of the next line in the line map
Location line_map_get_next_line_start(const LineMap* map, Location loc);

// Get the location of the start of the line for location
Location line_map_get_line_start(const LineMap* map, Location loc);

#endif /* LINE_MAP_H */
