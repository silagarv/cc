#include "location.h"

#include <stddef.h>

#include "adt/vector.h"

#define LOCATION_MAP_START_SIZE 64

LocationMap location_map_create(void)
{
    LocationMap new_map = {
        .entries = vector_new(sizeof(LocationEntry), LOCATION_MAP_START_SIZE)
    };

    return new_map;
}

void location_map_delete(LocationMap* map)
{
    vector_delete(map->entries);
}

LocationID location_map_insert(LocationMap* map, SourceLocation location, 
        SourceLocation real_location)
{
    LocationEntry entry = {
        .id = vector_get_count(map->entries),
        .location = location,
        .real_location = real_location
    };

    vector_push(map->entries, entry);

    return entry.id;
}

static bool location_map_has_location_id(LocationMap* map, LocationID id)
{
    return (vector_get_count(map->entries) >= id);
}

SourceLocation* location_map_real_location(LocationMap* map, LocationID id)
{
    if (!location_map_has_location_id(map, id))
    {
        return NULL;
    }

    return &map->entries[id].real_location;
}

SourceLocation* location_map_location(LocationMap* map, LocationID id)
{
    if (!location_map_has_location_id(map, id))
    {
        return NULL;
    }

    return &map->entries[id].location;
}
