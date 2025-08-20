#ifndef LOCATION_H
#define LOCATION_H

#include "util/vec.h"
#include <locale.h>
#include <stdint.h>
#include <limits.h>
#include <stdbool.h>

typedef uint32_t Location;

// The define to represent an invalid location
#define LOCATION_INVALID ((Location) 0)

// The max is the maximum of 32 bit integers (not unsigned). This is so that we
// have room to represent all of our macro locations. And a location may be valid
// whilst being more than LOCATION_MAX
#define LOCATION_MAX INT32_MAX

// If we are a macro location the top bit of the location is set. Givin use 
// 2**31 bits for normal locations and 2**31 bits for macro locations which
// should be plenty.
#define LOCATION_MACRO_BIT (1 << 31)

// A simple structure which holds 2 values the starting and ending locations.
// This range is from [start, end)
typedef struct LocationRange {
    Location start;
    Location end;
} LocationRange;

vector_of_decl(LocationRange, LocationRange, location_range);

// Function to check whether a location is valid or invalid
bool location_is_valid(Location loc);

// Function to check if a location is considered a file location
bool location_is_file(Location loc);

// Function to check if a location is a macro location
bool location_is_macro(Location loc);

// Check if a location range contains a specific location
bool location_range_contains(const LocationRange* range, Location loc);

#endif /* LOCATION_H */
