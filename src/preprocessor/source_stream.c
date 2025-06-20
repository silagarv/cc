#include "source_stream.h"

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"

#include "preprocessor/source_line.h"

// TODO: possible thing to do in the future is to change this code to
// simply alter the source stream itself. i.e. instead of making a buffer
// then deleting it everytime, why done we just alter the stream then do a
// memcpy, that is much faster really + no unneeded allocations

// also... that might handle null characters / binary files `better` not that
// that matters too much

// but this is fast enough for now anywyas we get ~65ms on sqlite3.c

SourceStream source_stream(char* fileguts, size_t len)
{
    SourceStream stream;
    stream.fileguts = fileguts;
    stream.end = fileguts + len;
    stream.pos = fileguts;

    return stream;
}

void source_stream_close(SourceStream* stream)
{
    free(stream->fileguts);
}

static int source_stream_get_char(SourceStream* stream)
{
    if (source_stream_at_eof(stream)) 
    {
        return EOF;
    }

    return *stream->pos++;
}

static bool internal_bad_char_recover(SourceLine* line, SourceStream* source, 
        Buffer* buffer, int bad_char)
{
    if (bad_char == EOF)
    {
        return true;
    }

    // We know we can add the char
    buffer_add_char(buffer, bad_char);

    // Return always after here but return true if newline
    if (bad_char == '\n')
    {
        line->num_phyical_lines++;
        line->ending_newline = true;
        
        return true;
    }

    return false;
}

static bool internal_handle_backslash(SourceLine* line, SourceStream* stream, 
        Buffer* buffer)
{
    const int next_char = source_stream_get_char(stream);

    if (next_char != '\n')
    {
        // add the '\' then check for EOF and (maybe) add next char
        buffer_add_char(buffer, '\\');

        // the reason this can be used here just fine is since we already know
        // that next char is not '\n' so the condition bad_char == '\n' is false
        // so this can only return true if next_char == EOF
        return internal_bad_char_recover(line, stream, buffer, next_char);
    }

    // No need to handle possible case of newline ending input here
    line->num_phyical_lines++;
    line->backslash_newline = true;

    return false;
}

static bool internal_handle_trigraph(SourceLine* line, SourceStream* stream, 
        Buffer* buffer)
{
    const int next_char = source_stream_get_char(stream);

    if (next_char != '?')
    {
        // Add previous question mark and recover
        buffer_add_char(buffer, '?');

        return internal_bad_char_recover(line, stream, buffer, next_char);
    }

    // here we recieved "??"
    const int next_next_char = source_stream_get_char(stream);
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
        buffer_add_char(buffer, '?');
        buffer_add_char(buffer, '?');

        return internal_bad_char_recover(line, stream, buffer, next_next_char);
    }

    line->replaced_trigraphs = true;

    // Trigraph could replace to '\' in that case we need to check if it is
    // followed by a backslash and act accordingly
    if (trigraph != '\\')
    {
        buffer_add_char(buffer, trigraph);   
    }
    else
    {
        return internal_handle_backslash(line, stream, buffer);
    }

    return false;
}

static SourceLine source_stream_clean_line(SourceStream* stream)
{
    const size_t buffer_start_size = 20;
    Buffer* tmp = buffer_new_size(buffer_start_size);

    SourceLine line = {0};

    while (true)
    {
        const int curr = source_stream_get_char(stream);
        switch (curr)
        {
            case '\\':
                if (internal_handle_backslash(&line, stream, tmp))
                {
                    break;
                }
                continue;
            
            case '?':
                if (internal_handle_trigraph(&line, stream, tmp))
                {
                    break;
                }
                continue;

            case EOF:
                break;

            case '\n':
                buffer_add_char(tmp, '\n');
                
                line.num_phyical_lines++;
                line.ending_newline = true;
                break;

            default:
                buffer_add_char(tmp, curr);
                continue;
        }

        break;
    }

    // Since we should never get eof as our only char...
    assert(buffer_get_len(tmp) != 0);

    // Do some finishing touched to it...
    if (!line.ending_newline)
    {
        line.num_phyical_lines++;

        buffer_add_char(tmp, '\n');
    }

    buffer_make_cstr(tmp);

    line.string = (String) {.ptr = buffer_get_ptr(tmp), .len = buffer_get_len(tmp)};

    buffer_free_ptr_only(tmp);

    return line;
}

SourceLine source_stream_read_line(SourceStream* stream)
{
    assert(!source_stream_at_eof(stream));

    return source_stream_clean_line(stream);
}
