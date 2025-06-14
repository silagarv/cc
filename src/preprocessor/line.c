#include "line.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"

#define LINEID_MAX (UINT32_MAX)

#define LINE_START_SIZE (20)

static void line_clean(Line* line)
{
    *line = (Line) {0};
}

static void line_get_fresh(Line* line, BufferedSource* source)
{
    line_clean(line);

    *line = (Line)
    {
        .id = LINEID_MAX,

        // .source_name = source->current_name,
        // .source_real_name = source->name,

        .line_no = source->current_line_no,
        .real_line_no = source->line_no,

        .buffer = buffer_new_size(LINE_START_SIZE),

        .replaced_trigraphs = false,
        .backslash_newline = false,
        .ending_newline = false
    };

    static_string_copy(&source->current_name, &line->source_name);
    static_string_copy(&source->name, &line->source_real_name);
}

// Function to recover on the incorrect characrers again returning true
// if we got eof or newline otherwise false
static bool line_get_internal_bad_char_recover(Line* line, 
        BufferedSource* source, int bad_char)
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
static bool line_get_internal_handle_backslash(Line* line, 
        BufferedSource* source)
{
    const int next_char = buffered_source_read_char(source);

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
static bool line_get_internal_handle_trigraph(Line* line, 
        BufferedSource* source)
{
    const int next_char = buffered_source_read_char(source);

    if (next_char != '?')
    {
        // Add previous question mark and recover
        buffer_add_char(line->buffer, '?');

        return line_get_internal_bad_char_recover(line, source, next_char);
    }

    // here we recieved "??"
    const int next_next_char = buffered_source_read_char(source);
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

static bool line_get_internal(Line* line, BufferedSource* source)
{
    while (true)
    {
        const int curr = buffered_source_read_char(source);

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

    // Check if all we got was eof
    if (!line_get_length(line))
    {
        line_free(line);

        return false;
    }

    // add a newline to the end of the buffer if we need to
    if (!line->ending_newline)
    {
        buffer_add_char(line->buffer, '\n');
    }

    buffer_make_cstr(line->buffer);

    return true;
}

bool line_read_from_buffered_source(BufferedSource* source, Line* line)
{
    line_get_fresh(line, source);
    
    const bool success = line_get_internal(line, source);

    // Easy sanity check here
    if (success && line_get_length(line) == 0)
    {
        panic("line got successfully but length is 0");
    }

    return success;
}

void line_free(Line* line)
{
    static_string_free(&line->source_name);
    static_string_free(&line->source_real_name);

    buffer_free(line->buffer);
}

void line_set_id(Line* line, LineID id)
{
    if (line->id != LINEID_MAX)
    {
        panic("attempting to set line id more than once");
    }

    line->id = id;
}

LineID line_get_id(Line* line)
{
    if (line->id == LINEID_MAX)
    {
        panic("attempting to get line id of an unset line");
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
