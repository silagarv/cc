#include "line.h"

#include <stdlib.h>

#include "core/xmalloc.h"

#define LINE_START_SIZE 40

LineBuilder line_builder(SourceLocation start_loc, SourceLocation real_loc)
{
    LineBuilder builder = (LineBuilder) {
        .start_loc = (LineLocation) {
            .filename = start_loc.filename,
            // we assume we want to be starting on the next line and reader->c
            // is '\n' so we'll increment it here
            .line_no = start_loc.line_no + 1
        },

        .real_start_loc = (LineLocation) {
            .filename = real_loc.filename,
            .line_no = real_loc.line_no + 1
        },

        .line_buffer = xmalloc(sizeof(char) * LINE_START_SIZE),
        .line_length = 0,
        .line_cap = LINE_START_SIZE,

        .replaced_trigraphs = false,
        .backslash_newline = false,
        .ending_newline = false
    };

    return builder;
}

void line_builder_free(LineBuilder* builder)
{
    free(builder->line_buffer);
}

void line_builder_add_char(LineBuilder* builder, char c)
{   
    // realloc if needed 
    if (builder->line_length == builder->line_cap) 
    {
        builder->line_cap *= 2;
        builder->line_buffer = xrealloc(builder->line_buffer, 
                sizeof(char) * builder->line_cap);
    }

    // add char to buffer 
    builder->line_buffer[builder->line_length++] = c;
}

size_t line_builder_length(LineBuilder* builder)
{
    return builder->line_length;
}

Line from_line_builder(LineBuilder* builder)
{
    // add terminator but don't increase the length
    line_builder_add_char(builder, '\0');
    builder->line_length--;

    return (Line) {
        .loc = builder->start_loc,
        .real_loc = builder->real_start_loc,

        .line_buffer = builder->line_buffer,
        .line_length = builder->line_length,

        .replaced_trigraphs = builder->replaced_trigraphs,
        .backslash_newline = builder->backslash_newline,
        .ending_newline = builder->ending_newline
    };
}
