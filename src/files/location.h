#ifndef LOCATION_H
#define LOCATION_H

#include <stdint.h>
#include <stdbool.h>

typedef uint32_t Location;

// A simple structure which holds 2 values the starting and ending locations
typedef struct LocationRange {
    Location start;
    Location end;
} LocationRange;

// Check if a location range contains a specific location
bool location_range_contains(const LocationRange* range, Location loc);

#endif /* LOCATION_H */
