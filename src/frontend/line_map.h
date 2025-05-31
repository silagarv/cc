#ifndef LINE_MAP_H
#define LINE_MAP_H

#include <stddef.h>
#include <stdint.h>

#include "frontend/source.h"

struct LineMap {
    Line** elems;
    size_t count;
    size_t cap;
};
typedef struct LineMap LineMap;

LineMap* linemap_new(void);
void linemap_free(LineMap* map);

LineID linemap_add_line(LineMap* map, Line* line);

Line* linemap_get_line(LineMap* map, LineID line);

#endif /* LINE_MAP_H */
