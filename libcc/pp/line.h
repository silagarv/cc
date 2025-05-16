#ifndef LINE_H
#define LINE_H

#include <stddef.h>
#include <stdbool.h>

#include "adt/buffer.h"

#include "pp/location.h"
#include "pp/source.h"

typedef size_t LineID;

// a line which has had trigraphs and backslash newlines replaced
typedef struct Line {
    LineID id;

    LineLocation loc;
    LineLocation real_loc;

    Buffer line;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
} Line;

typedef struct LineMap {
    Vector* lines;
} LineMap;

bool line_get_next_from_source(Source* source, Line* line);
void line_delete(Line* line);

size_t line_get_length(Line* line);
int line_get_char(Line* line, size_t index);

LineMap* line_map_create(void);
void line_map_delete(LineMap* map);

LineID line_map_insert_entry(LineMap* map, Line* line);

Line* line_map_get_line(LineMap* map, LineID id);

#endif /* LINE_H */
