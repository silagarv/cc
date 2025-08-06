#include "location.h"

// Check if a location range contains a specific location
bool location_range_contains(const LocationRange* range, Location loc)
{
    return (loc >= range->start && loc < range->end);
}
