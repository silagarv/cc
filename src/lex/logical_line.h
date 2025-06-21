#ifndef LOGICAL_LINE_H
#define LOGICAL_LINE_H

#include <stddef.h>

#include "files/filepath.h"

#include "lex/source_line.h"
#include "lex/token.h"

typedef struct LogicalLine {
    Filepath* filepath;
    uint32_t line_no;

    SourceLine* source_line;

    Token* start;
    Token* end;
    size_t count;
} LogicalLine;

#endif /* LOGICAL_LINE_H */
