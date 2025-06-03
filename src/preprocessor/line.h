#ifndef LINE_H
#define LINE_H

#include <stdint.h>

#include "util/buffer.h"

#include "preprocessor/buffered_source.h"

typedef uint32_t LineID;

struct Line
{
    // non-owned filenames
    char* source_name;
    char* source_real_name;

    uint32_t line_no;
    uint32_t real_line_no;

    // The line itself as a string
    Buffer* buffer;

    LineID id;

    // Line notes for now and later diagnostics
    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
};
typedef struct Line Line;

bool line_read_from_buffered_source(BufferedSource* source, Line* line);
void line_free(Line* line);

void line_set_id(Line* line, LineID id);
LineID line_get_id(Line* line);

size_t line_get_length(Line* line);
char line_get_char(Line* line, size_t index);
const char* line_get_ptr(Line* line);

#endif /* LINE_H */
