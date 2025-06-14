#include "buffered_source.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/xmalloc.h"
#include "util/buffer.h"

#define SOURCE_BUFFER_SIZE (4096 * 4)

BufferedSource* buffered_source_from_file(FILE* fp, StaticString* start_name)
{
    BufferedSource* source = xmalloc(sizeof(BufferedSource));
    *source = (BufferedSource) 
    {
        .fp = fp,

        .buffer = buffer_new_size(SOURCE_BUFFER_SIZE),
        .buffer_pos = 0,

        // .name = start_name,
        // .current_name = start_name,
        
        .type = BUFFERED_SOURCE_FILE,

        .line_no = 1,
        .current_line_no = 1,
        
        .prev = NULL
    };

    static_string_copy(start_name, &source->name);
    static_string_copy(start_name, &source->current_name);

    return source;
}

BufferedSource* buffered_source_from_buffer(Buffer* buffer, StaticString* start_name)
{
    BufferedSource* source = xmalloc(sizeof(BufferedSource));
    *source = (BufferedSource) 
    {
        .fp = NULL,

        .buffer = buffer,
        .buffer_pos = 0,

        // .name = start_name,
        // .current_name = start_name,

        .type = BUFFERED_SOURCE_BUFFER,
        
        .line_no = 1,
        .current_line_no = 1,
        
        .prev = NULL
    };

    static_string_copy(start_name, &source->name);
    static_string_copy(start_name, &source->current_name);

    return source;
}

void buffered_source_free(BufferedSource* source)
{
    // Can only fclose an actual file...

    if (source->type == BUFFERED_SOURCE_FILE)
    {
        fclose(source->fp);
    }

    buffer_free(source->buffer);

    static_string_free(&source->name);
    static_string_free(&source->current_name);

    free(source);
}

void buffered_source_set_line_no(BufferedSource* source, uint32_t new_no)
{
    source->current_line_no = new_no;
}

void buffered_source_set_name(BufferedSource* source, StaticString* new_name)
{
    static_string_copy(new_name, &source->current_name);
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
        // Make sure we reset buffer pos... otherwise not at eob
        const bool success = buffer_read_from_file(source->buffer, source->fp);

        source->buffer_pos = 0;

        return success;
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

BufferedSource* buffered_source_push(BufferedSource* prev, BufferedSource* new)
{
    new->prev = prev;

    return new;
}

BufferedSource* buffered_source_pop(BufferedSource* old)
{
    BufferedSource* new = old->prev;

    buffered_source_free(old);

    return new;
}

bool buffered_source_has_prev(BufferedSource* source)
{
    return (source->prev != NULL);
}
