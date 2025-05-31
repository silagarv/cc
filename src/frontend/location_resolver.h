#ifndef LOCATION_RESOLVER_H
#define LOCATION_RESOLVER_H

#include "frontend/line_map.h"
#include "frontend/location_map.h"
#include "frontend/location.h"

struct LocationResolver {
    LineMap* line_map;
    LocationMap* location_map;
};
typedef struct LocationResolver LocationResolver;

LocationResolver* location_resolver_new(void);
void location_resolver_free(LocationResolver* resolver);

LineID location_resolver_add_line(LocationResolver* resolver, Line* line);
LocationID location_resolver_add_location(LocationResolver* resolver, LineID id, 
        uint32_t start_col);

Line* location_resolver_get_line(LocationResolver* resolver, LocationID id);

Location location_resolver_get_location(LocationResolver* resolver, 
        LocationID id);
Location location_resolver_get_real_location(LocationResolver* resolver, 
        LocationID id);

#endif /* LOCATION_RESOLVER_H */
