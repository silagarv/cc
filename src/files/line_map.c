#include "line_map.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "files/location.h"
#include "files/file_manager.h"

// TODO: track down bug for end of files location for a well formed file which
// ends with a newline. (It works fine when the file doesn't end with a newline)


Location calculate_location(Location base, const char* start, char* curr)
{
    return base + (Location) (curr - start);
}

static void line_map_calculate(LineMap* map, FileBuffer* file)
{
    const Location base = map->range.start;

    // TODO: we want to change it to this
    const char* start = file->buffer_start;
    const char* end = file->buffer_end;

    char* current = file->buffer_start;

    while (current != end)
    {
        // First get the starting location of the range
        const Location range_start = calculate_location(base, start, current);

        // Now we want to find the ending location of the line range
        while (true)
        {
            // If we got the end of file
            if (current == end)
            {
                break;
            }

            // Get the current char and increment the position
            char curr = current[0];
            current++;

            // We found a newline
            if (curr == '\r' || curr == '\n')
            {
                // Check for a windows style ending and just increase ptr
                if (curr == '\r' && current[0] == '\n')
                {
                    current++;
                }

                break;
            }

            // We didn't find the line ending so just continue
        }

        // Get the range end
        const Location range_end = calculate_location(base, start, current) + 1;

        // Add the range to the ranges
        location_range_vector_push(&map->ranges, 
                (LocationRange) {range_start, range_end});
    }
}

LineMap line_map_create(FileBuffer* file, Location base_location)
{
    // The starting and ending pointers for the map
    const char* start = file->buffer_start;
    const char* end = file->buffer_end;

    LineMap map = (LineMap)
    {
        .range = (LocationRange)
        {
            .start = base_location,
            .end = base_location + (end - start) + 1
        },
        .ranges = location_range_vector_create(1)
    };

    // Calculate the line map
    line_map_calculate(&map, file);

    return map;
}

void  line_map_delete(LineMap* map)
{
    location_range_vector_free(&map->ranges, NULL);
}

// According to man bsearch the first argument to compare function is expected
// to be the key, and the second arument is expected to be an array memeber.
// Thank you whoever created bsearch for this foresight
static int location_range_compare(const void* loc, const void* loc_range)
{
    const Location location = *(Location*) loc;
    const LocationRange* range = loc_range;

    if (location_range_contains(range, location))
    {
        return 0;
    }

    if (location < range->start)
    {
        return -1;
    }

    assert(location >= range->end && "Must be true...");

    return 1;
}

static LocationRange* line_map_get_location_range(const LineMap* map, Location loc)
{
    assert(location_range_contains(&map->range, loc));

    LocationRange* range = bsearch(&loc, map->ranges.data, 
            location_range_vector_size(&map->ranges), 
            sizeof(LocationRange), location_range_compare);

    assert(range && "Location range contains location but bsearch failed");

   return range;
}

ResolvedLocation line_map_resolve_location(const LineMap* map, Location loc)
{
    LocationRange* range = line_map_get_location_range(map, loc);
    size_t range_index = range - map->ranges.data;

    // Convert from 0 based indexing to 1 base indexing
    const uint32_t line = range_index + 1;
    const uint32_t col = loc - range->start + 1; 
    
    return (ResolvedLocation) {line, col};
}

uint32_t line_map_resolve_line(const LineMap* map, Location loc)
{
    return line_map_resolve_location(map, loc).line;
}

uint32_t line_map_resolve_column(const LineMap* map, Location loc)
{
    return line_map_resolve_location(map, loc).col;
}

Location line_map_get_next_line_start(const LineMap* map, Location loc)
{
    LocationRange* range = line_map_get_location_range(map, loc);
    size_t range_index = range - map->ranges.data;

    // Need to check if we're at the end of the map or not.
    if (range_index + 1 > location_range_vector_size(&map->ranges))
    {
        return LOCATION_INVALID;
    }

    // Get the next range and return it's start
    return map->ranges.data[range_index + 1].start;
}

