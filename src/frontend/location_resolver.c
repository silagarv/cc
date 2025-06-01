#include "location_resolver.h"

#include <stdlib.h>

#include "util/xmalloc.h"

LocationResolver* location_resolver_new(void)
{
    LocationResolver* resolver = xmalloc(sizeof(LocationResolver));
    *resolver = (LocationResolver)
    {
        .line_map = linemap_new(),
        .location_map = location_map_new()
    };

    return resolver;
}

void location_resolver_free(LocationResolver* resolver)
{
    linemap_free(resolver->line_map);
    location_map_free(resolver->location_map);
    free(resolver);
}

LineID location_resolver_add_line(LocationResolver* resolver, Line* line)
{
    return linemap_add_line(resolver->line_map, line);
}

LocationID location_resolver_add_location(LocationResolver* resolver, LineID id, 
        uint32_t start_col)
{
    return location_map_add_location(resolver->location_map, id, start_col);
}

Line* location_resolver_get_line(LocationResolver* resolver, LocationID id)
{
    const LineID line_id = location_map_get_line(resolver->location_map, id);
    return linemap_get_line(resolver->line_map, line_id);
}

Location location_resolver_get_location(LocationResolver* resolver, 
        LocationID id)
{
    const Line* line = location_resolver_get_line(resolver, id);

    const uint32_t col = location_map_get_col(resolver->location_map, id);
    
    Location loc = (Location) 
    {
        .filename = line->source_name, 
        .line = line->line_no,
        .col = col
    };

    return loc;
}

Location location_resolver_get_real_location(LocationResolver* resolver, 
        LocationID id)
{
    const LineID line_id = location_map_get_line(resolver->location_map, id);
    const Line* line = linemap_get_line(resolver->line_map, line_id);

    const uint32_t col = location_map_get_col(resolver->location_map, id);
    
    Location loc = (Location) 
    {
        .filename = line->source_real_name, 
        .line = line->real_line_no,
        .col = col
    };

    return loc;
}
