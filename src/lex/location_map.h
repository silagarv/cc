#ifndef LOCATION_MAP_H
#define LOCATION_MAP_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/source_file.h"

#include "lex/location.h"

// TODO: What to do for line maps when there are multiple files and we need
// to handle #line directives and such

// A range representing the line we are on
typedef struct LocationRange {
    Location start;
    Location end;
} LocationRange;

// A line map representing the lines within a map
typedef struct LineMap {
    SourceFile* file; // A pointer to the source file

    LocationRange range; // Overall location range of the file

    LocationRange* ranges; // The different non-overlapping ranges
    size_t num_ranges; // The total number of lines
    size_t cap_ranges; // the total capacity
} LineMap;

// A structure to represent a mapping of locations to actual positions in many 
// different files to a resolved location
typedef struct LocationMap {
    void* data;
} LocationMap;

// Check if a location range contains a specific location
bool location_range_contains(const LocationRange* range, Location loc);

void line_map(LineMap* map, SourceFile* file, Location base_location);
void line_map_delete(LineMap* map);

ResolvedLocation line_map_resolve_location(const LineMap* map, Location loc);


#endif /* LOCATION_MAP_H */
