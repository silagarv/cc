#ifndef LINE_H
#define LINE_H

#include <stddef.h>
#include <stdbool.h>

#include "parse/location.h"

typedef struct Line {
    LineLocation loc;
    LineLocation real_loc;

    char* line_buffer;
    size_t line_length;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
} Line;

typedef struct LineBuilder {
    LineLocation start_loc;
    LineLocation real_start_loc;

    char* line_buffer;
    size_t line_length;
    size_t line_cap;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
} LineBuilder;

LineBuilder line_builder(SourceLocation start_loc, SourceLocation real_loc);
void line_builder_free(LineBuilder* builder);

void line_builder_add_char(LineBuilder* builder, char c);

size_t line_builder_length(LineBuilder* builder);

Line from_line_builder(LineBuilder* builder);

#endif /* LINE_H */
