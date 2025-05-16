#include "line.h"

#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

#include "adt/buffer.h"
#include "core/xmalloc.h"
#include "pp/source.h"

static void line_get_fresh(Source* source, Line* fresh_line)
{
    *fresh_line = (Line)
    {
        .id = SIZE_MAX,

        .loc = source->loc,
        .real_loc = source->real_loc,

        .line = buffer_new_stack(),

        .replaced_trigraphs = false,
        .backslash_newline = false,
        .ending_newline = false
    };
}

// Function to recover on the incorrect characrers again returning true
// if we got eof or newline otherwise false
static bool line_get_internal_bad_char_recover(Source* source, Line* line, 
        int bad_char)
{
    if (bad_char == EOF)
    {
        return true;
    }

    // We know we can add the char
    buffer_add_char(&line->line, bad_char);

    // Return always after here but return true if newline
    if (bad_char == '\n')
    {
        line->ending_newline = true;
        return true;
    }

    return false;
}

// Returns true if we got an EOF whilst handling
static bool line_get_internal_handle_backslash(Source* source, Line* line)
{
    const int next_char = source_read_char(source);

    // Bad case
    if (next_char != '\n')
    {
        // add the '\' then check for EOF and (maybe) add next char
        buffer_add_char(&line->line, '\\');

        // the reason this can be used here just fine is since we already know
        // that next char is not '\n' so that condition will jsut end up
        // false anyways. 
        //
        // This is more here for cleanliness :)
        return line_get_internal_bad_char_recover(source, line, next_char);
    }

    // No need to handle possible case of newline ending input here
    line->backslash_newline = true;

    return false;
}

// Returns true if we got an EOF or newline whilst handline
static bool line_get_internal_handle_trigraph(Source* source, Line* line)
{
    const int next_char = source_read_char(source);

    if (next_char != '?')
    {
        // Add previous question mark and recover
        buffer_add_char(&line->line, '?');

        return line_get_internal_bad_char_recover(source, line, next_char);
    }

    // Okay... here we definitively got 2 ?? in a row
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

    // Not a trigraph again :(........
    if (next_next_char == trigraph)
    {
        // Add previous 2 question marks and recover
        buffer_add_char(&line->line, '?');
        buffer_add_char(&line->line, '?');

        return line_get_internal_bad_char_recover(source, line, next_char);
    }

    // Okay HERE we finally have a trigaph
    line->replaced_trigraphs = true;

    // Somehow there is mroe to handle :(..
    if (trigraph != '\\')
    {
        buffer_add_char(&line->line, trigraph);   
    }
    else
    {
        // Just get the already made function to handle it lmao
        return line_get_internal_handle_backslash(source, line);
    }

    return false;
}

// Return's true on sucessfully getting a source line and false if we only
// got the end of file character
static bool line_get_internal(Source* source, Line* line)
{
    // Here is one of our major hot loops for the program
    for (;;)
    {
        const int curr = source_read_char(source);
        switch (curr)
        {   
            // Handle possible backslash newline
            case '\\':
                if (line_get_internal_handle_backslash(source, line))
                {
                    goto done;
                }
                break;
            
            // handle possible trigraph
            case '?':
                if (line_get_internal_handle_trigraph(source, line))
                {
                    goto done;
                }
                break;

            case EOF:
                goto done;

            case '\n':
                buffer_add_char(&line->line, curr);
                line->ending_newline = true;
                goto done;

            default:
                buffer_add_char(&line->line, curr);
                break;
        }
    }

done:
    if (line_get_length(line) == 0)
    {
        return false;
    }

    buffer_make_cstr(&line->line);

    return true;
}

bool line_get_next_from_source(Source* source, Line* line)
{
    // Setup line
    line_get_fresh(source, line);

    // If false we read no characters into the line
    if (!line_get_internal(source, line))
    {
        line_delete(line);
        return false;
    }

    return true;
}

void line_delete(Line* line)
{
    buffer_delete_stack(&line->line);
}

size_t line_get_length(Line* line)
{
    return buffer_get_len(&line->line);
}

int line_get_char(Line* line, size_t index)
{
    return buffer_get(&line->line, index);
}
