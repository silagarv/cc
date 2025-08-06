#include "line_map.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "files/source_file.h"
#include "util/panic.h"
#include "util/xmalloc.h"

#include "files/location.h"

// TODO: track down bug for end of files location for a well formed file which
// ends with a newline. (It works fine when the file doesn't end with a newline)


Location calculate_location(Location base, const char* start, char* curr)
{
    return base + (Location) (curr - start);
}

static void line_map_calculate(LineMap* map, SourceFile* file)
{
    const Location base = map->range.start;

    // TODO: we want to change it to this
    const char* start = file->file_buffer->buffer_start;
    const char* end = file->file_buffer->buffer_end;

    char* current = file->file_buffer->buffer_start;

    while (current != end)
    {
        if (map->num_ranges == map->cap_ranges)
        {
            map->cap_ranges = (map->cap_ranges == 0) ? 1 : (map->cap_ranges * 2);
            map->ranges = xrealloc(map->ranges, sizeof(LocationRange) * map->cap_ranges);
        }

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
        const Location range_end = calculate_location(base, start, current);

        // Add the range to the ranges
        map->ranges[map->num_ranges] = (LocationRange) {range_start, range_end};
        map->num_ranges++;
    }
}

void line_map(LineMap* map, SourceFile* file, Location base_location)
{
    map->file_id = file->id;
    
    // The starting and ending pointers for the map
    const char* start = file->file_buffer->buffer_start;
    const char* end = file->file_buffer->buffer_end;

    // Set up the overall range
    map->range = (LocationRange) 
    {
        .start = base_location, 
        .end = base_location + (start - end) + 1
    };

    // Zero out the ranges for now
    map->ranges = NULL;
    map->num_ranges = 0;
    map->cap_ranges = 0;

    // Calculate the line map
    line_map_calculate(map, file);
}

void  line_map_delete(LineMap* map)
{
    free(map->ranges);
}

LocationTriplet line_map_resolve_location(const LineMap* map, Location loc)
{
    if (!location_range_contains(&map->range, loc))
    {
        panic("line map does not contain location");

        return (LocationTriplet) {0};
    }

    // First we need to find the range that contains the line
    LocationRange* range = NULL;
    size_t range_index;

    // Currently just linear search through each of the ranges but later it would
    // be a good idea to convert this to a binary search for performance
    for (range_index = 0; range_index < map->num_ranges; range_index++)
    {
        range = map->ranges + range_index;

        if (location_range_contains(range, loc))
        {
            break;
        }

        if (range_index == map->num_ranges)
        {
            panic("unable to find location; should be unreachable");
        }
    }

    // Convert from 0 based indexing to 1 base indexing
    const uint32_t line = range_index + 1;
    const uint32_t col = loc - range->start + 1; 
    
    return (LocationTriplet) {map->file_id, line, col};
}

