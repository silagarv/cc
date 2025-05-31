#include "line_map.h"

#include <stdlib.h>

#include "diagnostic/diagnostic.h"
#include "util/xmalloc.h"
#include "frontend/source.h"

#define LINE_MAP_START_SIZE (8)

LineMap* linemap_new(void)
{
    LineMap* map = xmalloc(sizeof(LineMap));
    *map = (LineMap)
    {
        .elems = xmalloc(sizeof(Line*) * LINE_MAP_START_SIZE),
        .count = 0,
        .cap = LINE_MAP_START_SIZE
    };

    return map;
}

static size_t linemap_get_count(LineMap* map)
{
    return map->count;
}

static size_t linemap_get_capacity(LineMap* map)
{
    return map->cap;
}

static void linemap_expand(LineMap* map)
{
    map->cap *= 2;
    map->elems = xrealloc(map->elems, sizeof(Line*) * map->cap);
}

void linemap_free(LineMap* map)
{
    for (size_t i = 0; i < linemap_get_count(map); i++)
    {
        line_free(map->elems[i]);
    }
    free(map->elems);
    free(map);
}

LineID linemap_add_line(LineMap* map, Line* line)
{
    // !!note may overflow LineID if bigger than LINE_ID_MAX
    if (linemap_get_count(map) == linemap_get_capacity(map))
    {
        linemap_expand(map);
    }

    const LineID new_id = linemap_get_count(map);

    line_set_id(line, new_id);

    map->elems[map->count++] = line;

    return new_id;
}

Line* linemap_get_line(LineMap* map, LineID line)
{
    // !!note may overflow LineID if bigger than LINE_ID_MAX
    const LineID max_id = (LineID) linemap_get_count(map);
    if (line >= max_id)
    {
        internal_compiler_error("requesting line that does not exist");
    }

    return map->elems[line];
}
