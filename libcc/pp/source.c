#include "source.h"

#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

#include "core/xmalloc.h"

#include "adt/buffer.h"
#include "adt/string_map.h"

#include "pp/location.h"

#define SOURCE_BUFFER_SIZE (4096)

Source* source_push(Source* prev, FILE* fp, char* filename, 
        SearchpathEntry* searchpath)
{   
    /* TODO: maybe do a thing where we copy the given filename */
    Source* new_source = xmalloc(sizeof(Source));
    *new_source = (Source)
    {
        .fp = fp,
        .buffer = buffer_new_size(SOURCE_BUFFER_SIZE),
        .buffer_pos = 0,

        .loc = (LineLocation)
        {
            .filename = filename,
            .line_no = 1
        },

        .real_loc = (LineLocation)
        {
            .filename = filename,
            .line_no = 1
        },

        .name_map = string_map_new(8),

        .include_depth = prev ? prev->include_depth + 1 : 0,
        .searchpath = searchpath,
        .prev = prev
    };

    return new_source;
}

void source_delete(Source* source)
{
    string_map_delete(source->name_map);
    buffer_delete(source->buffer);

    fclose(source->fp);
    free(source);
}

int source_read_char(Source* source)
{
    if (source->buffer_pos == buffer_get_len(source->buffer))
    {
        const size_t new_len = fread(
                buffer_get_ptr(source->buffer), 
                sizeof(char), 
                buffer_get_cap(source->buffer), 
                source->fp
            );
        
        buffer_set_len(source->buffer, new_len);
        source->buffer_pos = 0;

        if (!new_len)
        {
            return EOF;
        }      
    }

    const int c = buffer_get(source->buffer, source->buffer_pos++);

    if (c == '\n')
    {
        source->loc.line_no++;
        source->real_loc.line_no++;
    }

    return c;
}

// NOTE: this should only be done in between reading from lines and should never
// NOTE: be used WHILST reading from a line
void source_set_line(Source* source, size_t line_no)
{
    source->loc.line_no = line_no;
}

void source_set_file(Source* source, char* filename);
