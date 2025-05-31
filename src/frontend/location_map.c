#include "location_map.h"

#include <stdlib.h>

#include "util/xmalloc.h"

#include "diagnostic/diagnostic.h"

#define LOCATION_MAP_START_SIZE (8)

LocationMap* location_map_new(void)
{
    LocationMap* map = xmalloc(sizeof(LocationMap));
    *map = (LocationMap)
    {
        .elems = xmalloc(sizeof(LocationEntry) * LOCATION_MAP_START_SIZE),
        .count = 0,
        .cap = LOCATION_MAP_START_SIZE
    };

    return map;
}

void location_map_free(LocationMap* map)
{
    free(map->elems);
    free(map);
}

static size_t location_map_get_count(LocationMap* map)
{
    return map->count;
}

static size_t location_map_get_capacity(LocationMap* map)
{
    return map->cap;
}

static void location_map_expand(LocationMap* map)
{
    map->cap *= 2;
    map->elems = xrealloc(map->elems, sizeof(LocationEntry) * map->cap);
}

LocationID location_map_add_location(LocationMap* map, LineID id, 
        uint32_t start_col)
{
    if (location_map_get_count(map) == location_map_get_capacity(map))
    {
        location_map_expand(map);
    }

    // !!note may overflow if bigger than LOCATIONID_MAX
    const LocationID new_id = location_map_get_count(map);

    map->elems[new_id] = (LocationEntry)
    {
        .line = id,
        .col = start_col
    };

    map->count++;

    return new_id;
}

LineID location_map_get_line(LocationMap* map, LocationID id)
{
    // !!note may overflow LocationID if bigger than LOCATIONID_MAX
    const LocationID max_id = location_map_get_count(map);
    if (id >= max_id)
    {
        internal_compiler_error("location map bad id");
    }

    return map->elems[id].line;
}

uint32_t location_map_get_col(LocationMap* map, LocationID id)
{
    // !!note may overflow LocationID if bigger than LOCATIONID_MAX
    const LocationID max_id = location_map_get_count(map);
    if (id >= max_id)
    {
        internal_compiler_error("location map bad id");
    }

    return map->elems[id].col;
}
