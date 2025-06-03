#include "buffered_source.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/xmalloc.h"
#include "util/buffer.h"

#define SOURCE_BUFFER_SIZE (4096)

BufferedSource* buffered_source_from_file(FILE* fp, char* start_name, 
        BufferedSource* prev)
{
    BufferedSource* source = xmalloc(sizeof(BufferedSource));
    *source = (BufferedSource) 
    {
        .fp = fp,

        .buffer = buffer_new_size(SOURCE_BUFFER_SIZE),
        .buffer_pos = 0,

        .name = start_name,
        .current_name = start_name,
        
        .line_no = 1,
        .current_line_no = 1,
        
        .prev = NULL
    };

    return source;
}

BufferedSource* buffered_source_from_buffer(Buffer* buffer, char* start_name, 
        BufferedSource* prev)
{
    BufferedSource* source = xmalloc(sizeof(BufferedSource));
    *source = (BufferedSource) 
    {
        .fp = NULL,

        .buffer = buffer,
        .buffer_pos = 0,

        .name = start_name,
        .current_name = start_name,
        
        .line_no = 1,
        .current_line_no = 1,
        
        .prev = NULL
    };

    return source;
}

void buffered_source_free(BufferedSource* source)
{
    // Can only fclose an actual file...
    if (source->fp)
    {
        fclose(source->fp);
    }

    buffer_free(source->buffer);
    free(source);
}

void buffered_source_set_line_no(BufferedSource* source, uint32_t new_no)
{
    source->current_line_no = new_no;
}

void buffered_source_set_name(BufferedSource* source, char* new_name)
{
    source->current_name = new_name;
}

static bool buffered_source_is_eob(BufferedSource* source)
{
    return (buffer_get_len(source->buffer) == source->buffer_pos);
}

static bool buffered_source_read_more(BufferedSource* source)
{
    // If our buffered source is from a file...
    if (source->fp)
    {
        return buffer_read_from_file(source->buffer, source->fp);
    }
    
    // Otherwise it could be some builtin or command line
    return false; // so nothing more to read
}

int buffered_source_read_char(BufferedSource* source)
{
    // If nothing left then try to read more otherwise return EOF
    if (buffered_source_is_eob(source))
    {
        if (!buffered_source_read_more(source))
        {
            return EOF;
        }
    }

    // We only care about getting the starting line correct here...
    const char c = buffer_get(source->buffer, source->buffer_pos++);
    if (c == '\n')
    {
        source->line_no++;
        source->current_line_no++;
    }

    return c;
}
