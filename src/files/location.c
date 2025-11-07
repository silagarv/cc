#include "location.h"

#include "util/vec.h"

#include <stdlib.h>

vector_of_impl(Location, Location, location)
vector_of_impl(LocationRange, LocationRange, location_range)

// Check if a location is valid or invalid
bool location_is_valid(Location loc)
{
    return (loc != LOCATION_INVALID);
}

bool location_is_file(Location loc)
{
    return location_is_valid(loc) && (loc < LOCATION_MAX);
}

bool location_is_macro(Location loc)
{
    return ((loc & (1 << 31)) != 0);
}

// Check if a location range contains a specific location
bool location_range_contains(const LocationRange* range, Location loc)
{
    return (loc >= range->start && loc < range->end);
}
