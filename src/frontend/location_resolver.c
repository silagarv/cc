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

// TODO: remove this below and complete
#include "diagnostic/diagnostic.h"

Line* location_resolver_get_line(LocationResolver* resolver, LocationID id)
{
    internal_compiler_error("unimplemented");
    return NULL;
}

Location location_resolver_get_location(LocationResolver* resolver, 
        LocationID id)
{
    internal_compiler_error("unimplemented");
    return (Location) {0};
}
Location location_resolver_get_real_location(LocationResolver* resolver, 
        LocationID id)
{
    internal_compiler_error("unimplemented");
    return (Location) {0};
}
