#ifndef LINE_H
#define LINE_H

#include <stddef.h>
#include <stdbool.h>

#include "adt/buffer.h"

#include "core/location.h"

// a line which has had trigraphs and backslash newlines replaced
typedef struct LogicalLine {
    LineLocation loc;
    LineLocation real_loc;

    Buffer* line;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
} LogicalLine;

LogicalLine* logical_line_build(void* input_reader);

#endif /* LINE_H */
