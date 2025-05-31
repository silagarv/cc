#include "source.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "diagnostic/diagnostic.h"
#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"

#define SOURCE_BUFFER_SIZE (4096)
#define LINE_START_SIZE (20)

Source* source_new(char* start_name, FILE* fp)
{
    Source* source = xmalloc(sizeof(Source));
    *source = (Source) 
    {
        .fp = fp,

        .buffer = buffer_new_size(SOURCE_BUFFER_SIZE),
        .buffer_pos = 0,

        .name = start_name,
        .current_name = start_name,
        
        .line_no = 1,
        .current_line_no = 1
    };

    return source;
}

void source_free(Source* source)
{
    buffer_free(source->buffer);
    fclose(source->fp);
    free(source);
}

void source_set_current_name(Source* source, char* new_name)
{
    source->current_name = new_name;
}

void source_set_current_line_number(Source* source, uint32_t new_line)
{
    source->current_line_no = new_line;
}

static bool source_is_end_of_buffer(Source* source)
{
    return (buffer_get_len(source->buffer) == source->buffer_pos);
}

static bool source_read_from_file(Source* source)
{
    assert(source_is_end_of_buffer(source));

    const bool success = buffer_read_from_file(source->buffer, source->fp);

    source->buffer_pos = 0;

    return success;
}

static bool source_is_end_of_file(Source* source)
{
    return (source->buffer_pos == 0 && feof(source->fp));
}

static int source_read_char(Source* source)
{
    if (source_is_end_of_buffer(source) && !source_read_from_file(source))
    {
        assert(source_is_end_of_file(source));

        return EOF;
    }

    const char c = buffer_get(source->buffer, source->buffer_pos++);

    // since we only care about starting line this is fine...
    if (c == '\n')
    {
        source->line_no++;
        source->current_line_no++;
    }

    return c;
}

static Line* line_get_fresh(Source* src)
{
    Line* line = xmalloc(sizeof(Line));
    *line = (Line)
    {
        .id = LINEID_MAX,

        .source_name = src->current_name,
        .source_real_name = src->name,

        .line_no = src->current_line_no,
        .real_line_no = src->line_no,

        .buffer = buffer_new_size(LINE_START_SIZE),

        .replaced_trigraphs = false,
        .backslash_newline = false,
        .ending_newline = false
    };

    return line;
}

// Function to recover on the incorrect characrers again returning true
// if we got eof or newline otherwise false
static bool line_get_internal_bad_char_recover(Line* line, Source* source, 
        int bad_char)
{
    if (bad_char == EOF)
    {
        return true;
    }

    // We know we can add the char
    buffer_add_char(line->buffer, bad_char);

    // Return always after here but return true if newline
    if (bad_char == '\n')
    {
        line->ending_newline = true;
        return true;
    }

    return false;
}

// Returns true if we got an EOF whilst handling
static bool line_get_internal_handle_backslash(Line* line, Source* source)
{
    const int next_char = source_read_char(source);

    if (next_char != '\n')
    {
        // add the '\' then check for EOF and (maybe) add next char
        buffer_add_char(line->buffer, '\\');

        // the reason this can be used here just fine is since we already know
        // that next char is not '\n' so the condition bad_char == '\n' is false
        // so this can only return true if next_char == EOF
        return line_get_internal_bad_char_recover(line, source, next_char);
    }

    // No need to handle possible case of newline ending input here
    line->backslash_newline = true;

    return false;
}

// Returns true if we got an EOF or newline whilst handline
static bool line_get_internal_handle_trigraph(Line* line, Source* source)
{
    const int next_char = source_read_char(source);

    if (next_char != '?')
    {
        // Add previous question mark and recover
        buffer_add_char(line->buffer, '?');

        return line_get_internal_bad_char_recover(line, source, next_char);
    }

    // here we recieved "??"
    const int next_next_char = source_read_char(source);
    int trigraph;

    switch (next_next_char)
    {
        case '(': trigraph = '['; break;
        case ')': trigraph = ']'; break;
        case '<': trigraph = '{'; break;
        case '>': trigraph = '}'; break;
        case '=': trigraph = '#'; break;
        case '/': trigraph = '\\'; break;
        case '\'': trigraph = '^'; break;
        case '!': trigraph = '|'; break;
        case '-': trigraph = '~'; break;
        default: trigraph = next_next_char; break;
    }

    if (next_next_char == trigraph)
    {
        // Add previous 2 question marks and recover
        buffer_add_char(line->buffer, '?');
        buffer_add_char(line->buffer, '?');

        return line_get_internal_bad_char_recover(line, source, next_next_char);
    }

    line->replaced_trigraphs = true;

    // Trigraph could replace to '\' in that case we need to check if it is
    // followed by a backslash and act accordingly
    if (trigraph != '\\')
    {
        buffer_add_char(line->buffer, trigraph);   
    }
    else
    {
        return line_get_internal_handle_backslash(line, source);
    }

    return false;
}

static bool line_get_internal(Line* line, Source* source)
{
    while (true)
    {
        const int curr = source_read_char(source);

        // In here if we want to exit the while loop i.e. we got EOF or a '\n'
        // then we break out of the switch. Otherwise we continue and go back
        // to the start of the while loop
        switch (curr)
        {
            case '\\':
                if (line_get_internal_handle_backslash(line, source))
                {
                    break;
                }
                continue;
            
            case '?':
                if (line_get_internal_handle_trigraph(line, source))
                {
                    break;
                }
                continue;

            case EOF:
                break;

            case '\n':
                buffer_add_char(line->buffer, curr);
                line->ending_newline = true;
                break;

            default:
                buffer_add_char(line->buffer, curr);
                continue;
        }

        break;
    }

    if (!line_get_length(line))
    {
        line_free(line);

        return false;
    }

    buffer_make_cstr(line->buffer);

    return true;
}

Line* source_read_line(Source* source)
{
    Line* line = line_get_fresh(source);

    if (!line_get_internal(line, source))
    {
        return NULL;
    }

    assert(line_get_length(line));

    return line;
}

void line_free(Line* line)
{
    buffer_free(line->buffer);
    free(line);
}

void line_set_id(Line* line, LineID id)
{
    if (line->id != LINEID_MAX)
    {
        internal_compiler_error("attempting to set line id more than once");
    }

    line->id = id;
}

LineID line_get_id(Line* line)
{
    if (line->id == LINEID_MAX)
    {
        internal_compiler_error("attempting to get line id of an unset line");
    }

    return line->id;
}

size_t line_get_length(Line* line)
{
    return buffer_get_len(line->buffer);
}

char line_get_char(Line* line, size_t index)
{
    return buffer_get(line->buffer, index);
}

const char* line_get_ptr(Line* line)
{
    return buffer_get_ptr(line->buffer);
}
