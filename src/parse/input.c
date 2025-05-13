#include "input.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/vector.h"
#include "core/panic.h"
#include "core/xmalloc.h"
#include "parse/line.h"

#define INPUT_BUFFER_SIZE (4096)

SearchPathEntry* searchpath_entry_new(char* filepath, bool is_system)
{
    SearchPathEntry* entry = xmalloc(sizeof(SearchPathEntry));
    *entry = (SearchPathEntry) {
        .filepath = filepath,
        .is_system = is_system,
        
        .next = NULL
    };

    return entry;
}

void searchpath_entry_delete(SearchPathEntry* entry)
{
    free(entry);
}

void searchpath_add_entry(SearchPath* search, SearchPathEntry* entry)
{
    *search->anchor = entry;
    search->anchor = &entry->next;
}

void searchpath_append(SearchPath* start, SearchPath* after)
{
    *start->anchor = after->start;
}

InputReader* input_reader_from_file(FILE* fp, char* filename)
{
    if (!fp)
    {
        return NULL;
    }

    InputReader* reader = xmalloc(sizeof(InputReader));
    *reader = (InputReader) {
        .fp = fp,
        .buffer = xmalloc(sizeof(char) * INPUT_BUFFER_SIZE),
        .cap = INPUT_BUFFER_SIZE,

        .pos = 0,
        .len = 0,
        
        .c = '\n',

        .location = (SourceLocation) {
            .filename = filename,
            .line_no = 0,
            .col_no = 0
        },

        .real_location = (SourceLocation) {
            .filename = filename,
            .line_no = 0,
            .col_no = 0
        }
    };

    return reader;
}

void input_reader_free(InputReader* reader)
{
    // assert(!ferror(reader->fp) && "FILE* error");
    // assert(feof(reader->fp) && "InputReader not EOF");

    fclose(reader->fp);
    free(reader->buffer);
    free(reader);
}

void input_reader_set_filename(InputReader* reader, char* new_filename)
{
    reader->location.filename = new_filename;
}

// Set the next line to be the value of new_line
void input_reader_set_line(InputReader* reader, size_t new_line)
{
    reader->location.line_no = new_line - 1;
}

static int input_reader_next_char(InputReader* reader)
{   
    // if we have read all of the buffer 
    if (reader->len == reader->pos)
    {
        reader->pos = 0;
        reader->len = fread(reader->buffer, sizeof(char), reader->cap, 
                reader->fp);

        if (!reader->len)
        {
            reader->c = EOF;
            return reader->c;
        }
    }

    // if the previous char was a newline update position's accordingly
    if (reader->c == '\n')
    {
        reader->location.line_no++;
        reader->location.col_no = 0;

        reader->real_location.line_no++;
        reader->real_location.col_no = 0;
    }
    reader->location.col_no++;

    reader->real_location.col_no++;

    // Set the new current char 
    reader->c = reader->buffer[reader->pos++];

    // return the current char
    return reader->c;
}

Line input_reader_next_line(InputReader* reader)
{
    if (reader->c == EOF)
    {
        return (Line) {0};
    }

    LineBuilder builder = line_builder(reader->location, reader->real_location);

    do
    {
        const int curr = input_reader_next_char(reader);

        if (curr == EOF)
        {
            break;
        }

        // Backslash newlines (maybe)
        if (curr == '\\')
        {
            const int next = input_reader_next_char(reader);
            
            // No backslash newline 
            if (next != '\n')
            {
                line_builder_add_char(&builder, '\\');

                if (next == EOF)
                {
                    break;
                }

                // Don't have to check for newline here as it cannot be that

                line_builder_add_char(&builder, next);
                continue;
            }

            // Got a backslash newline
            builder.backslash_newline = true;
            continue;
        }

        // Trigraph handling
        if (curr == '?')
        {
            const int next = input_reader_next_char(reader);

            // Definitely not a trigraph here...
            if (next != '?')
            {
                line_builder_add_char(&builder, '?');

                if (next == EOF)
                {
                    break;
                }

                line_builder_add_char(&builder, next);
                
                // Break if we got a newline
                if (next == '\n')
                {
                    builder.ending_newline = true;
                    break;
                }

                continue;
            }

            const int next_next = input_reader_next_char(reader);
            int trigraph;

            switch (next_next) 
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
                default: trigraph = next_next; break;
            }
            
            // Not a trigraph again
            if (next_next == trigraph)
            {
                line_builder_add_char(&builder, '?');
                line_builder_add_char(&builder, '?');

                if (next_next == EOF)
                {
                    break;
                }
                
                line_builder_add_char(&builder, next);
                
                // Break if we got a newline
                if (next == '\n')
                {
                    builder.ending_newline = true;
                    break;
                }

                continue;
            }

            // Here we have a trigraph just check if maybe backslash newline
            builder.replaced_trigraphs = true;
            
            // special trigraph case 
            if (trigraph == '\\')
            {
                const int next_next_next = input_reader_next_char(reader);

                if (next_next_next != '\n')
                {
                    line_builder_add_char(&builder, '\\');

                    if (next_next_next == EOF)
                    {
                        break;
                    }

                    line_builder_add_char(&builder, next_next_next);
                    continue;
                }

                // otherwise trigraph backslash newline so continue
                builder.backslash_newline = true;
                continue;
            }
 
            line_builder_add_char(&builder, trigraph);
            continue;
        }

        // normal case add the char and check for newline 
        line_builder_add_char(&builder, curr);

        if (curr == '\n')
        {   
            builder.ending_newline = true;
            break;
        }
    } while (1);

    // check to see if we actually read anything at all
    if (!line_builder_length(&builder))
    {
        line_builder_free(&builder);
        return (Line) {0};
    }

    return from_line_builder(&builder);
}

static Input* input_from_fp(FILE* fp, char* filename, 
        SearchPathEntry* start_path)
{
    Input* input = xmalloc(sizeof(Input));
    *input = (Input) {
        .filename = filename,

        .reader = input_reader_from_file(fp, filename),

        .lines = vector_new(sizeof(Line), 1),

        .depth = 0, // unknown for now

        .entry = start_path,
        .parent = NULL // unknown for now
    };

    return input;
}

Input* input_new(FILE* fp, char* filename, SearchPathEntry* entry)
{
    if (!fp)
    {
        return NULL;
    }

    return input_from_fp(fp, filename, entry);
}

void input_delete(Input* input)
{   
    // Free all of our lines and the array itself 
    for (size_t i = 0; i < vector_get_count(input->lines); i++) {
        free(input->lines[i].line_buffer);
    }
    vector_delete(input->lines);

    input_reader_free(input->reader);
    free(input);
}

void input_set_filename(Input* input, char* new_filename)
{
    input_reader_set_filename(input->reader, new_filename);
}

void input_set_line(Input* input, size_t new_line)
{
    input_reader_set_line(input->reader, new_line);
}

Line* input_get_next_line(Input* input)
{
    Line next_line = input_reader_next_line(input->reader);

    if (!next_line.line_buffer)
    {
        return NULL;
    }

    vector_push(input->lines, next_line);

    return input->lines + vector_get_count(input->lines) - 1;
}

Line* input_find_real_line(Input* input, size_t real_line)
{
    // TODO: optimise the function, maybe through binary search or even
    // TODO: just some simple early termination conditions

    for (size_t i = 0; i < vector_get_count(input->lines); i++) {
        if (input->lines[i].real_loc.line_no == real_line) {
            return input->lines + i;
        }
    }

    return NULL;
}






