#ifndef LINE_MAP_H
#define LINE_MAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/source_file.h"
#include "files/location.h"

// A structure to represent a location triplet. I.e. the file id, raw line, and
// raw column. Note that this can then be used along with our mapping of our
// line table (for #line directives) to determine the accurate and precise
// location
typedef struct LocationTriplet {
    SourceFileId id;
    uint32_t line;
    uint32_t col;
} LocationTriplet;

// A line map representing the lines within a map
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
// lol wtf was I on when I wrote that haha

// A structure that we will use to go from a files overall location and it's
// ranges back to a location triplet. This is eagerly built at the moment but
// eventually I want to be able to lazily build this structure. Also eventually
// we will want to fix some potential performace concerns with the current
// implementation.
typedef struct LineMap {
    SourceFileId file_id; // A pointer to the source file

    LocationRange range; // Overall location range of the file

    LocationRange* ranges; // The different non-overlapping ranges
    size_t num_ranges; // The total number of lines
    size_t cap_ranges; // the total capacity
} LineMap;

void line_map(LineMap* map, SourceFile* file, Location base_location);
void line_map_delete(LineMap* map);

LocationTriplet line_map_resolve_location(const LineMap* map, Location loc);

#endif /* LINE_MAP_H */
