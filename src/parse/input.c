#include "input.h"

#include <assert.h>
#include <string.h>
#include <stdbool.h>
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
        }
    };

    return reader;
}

void input_reader_free(InputReader* reader)
{
    assert(!ferror(reader->fp) && "FILE* error");
    assert(feof(reader->fp) && "InputReader not EOF");

    fclose(reader->fp);
    free(reader->buffer);
    free(reader);
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
    }
    reader->location.col_no++;

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

    LineBuilder builder = line_builder(reader->location);

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
    // Get our reader
    InputReader* reader = input_reader_from_file(fp, filename);
    
    // Read the lines from memeory
    Vector lines = vector_new(sizeof(Line), 32);
    while (1)
    {
        Line next_line = input_reader_next_line(reader);

        if (!next_line.line_buffer)
        {
            break;
        }

        vector_push(&lines, &next_line);
    }

    // delete input reader
    input_reader_free(reader);

    // Get the start line for getting the first location
    Line* start_line = vector_get(&lines, 0);

    Input* input = xmalloc(sizeof(Input));
    *input = (Input) {
        .filename = filename,

        .lines = lines,

        .curr_line_idx = 0,
        .current_line = start_line,

        .line_pos = 0,

        .location = {
            .filename = start_line->loc.filename,
            .line_no = start_line->loc.line_no,
            .col_no = 1
        },

        .depth = 0,
        .entry = start_path,
        .parent = NULL
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
    for (size_t i = 0; i < vector_get_count(&input->lines); i++) {
        Line* line = vector_get(&input->lines, i);
        free(line->line_buffer);
    }
    vector_delete(&input->lines);

    free(input);
}

void input_set_filename(Input* input, char* new_filename)
{
    input->location.filename = new_filename;
}

// note that this function is made to alter the current line
// for a #line directive it should be fully processed and then get the next line
// and only THEN should the line be set
void input_set_line(Input* input, size_t new_line)
{
    input->location.line_no = new_line;
}

Line* input_get_next_line(Input* input)
{
    const Line* prev_line = input->current_line;
    const size_t num_lines = vector_get_count(&input->lines);

    // increment the lines
    input->curr_line_idx++;

    // if we're equal or gone over (num_lines == 0 if empty)
    // then set line to NULL and return it
    if (input->curr_line_idx >= num_lines)
    {
        input->current_line = NULL;
        return NULL;
    }

    // increment the current line
    input->current_line++;

    // Now we need to properly increment the current line number
    const uint32_t line_num_diff = input->current_line->loc.line_no - 
            prev_line->loc.line_no;

    // Fix our location by adding the line difference and reset col_no
    input->location.line_no += line_num_diff;
    input->location.col_no = 1;

    return input->current_line;
}

Line* input_find_real_line(Input* input, size_t real_line)
{
    // TODO: optimise the function, maybe through binary search or even
    // TODO: just some simple early termination conditions

    for (size_t i = 0; i < vector_get_count(&input->lines); i++) {
        Line* line = vector_get(&input->lines, i);
        if (line->loc.line_no == real_line) {
            return line;
        }
    }

    return NULL;
}

SourceLocation input_get_location(Input* input)
{
    return input->location;
}

SourceLocation input_get_real_location(Input* input)
{
    // We have somehow got the wrong name
    assert(!strcmp(input->filename, input->current_line->loc.filename));

    SourceLocation loc = (SourceLocation) {
        .filename = input->current_line->loc.filename,
        .line_no = input->current_line->loc.line_no,
        .col_no = input->location.col_no,
    };

    return loc;
}

InputManager* input_manager_new(void)
{
    InputManager* manager = xmalloc(sizeof(InputManager));
    *manager = (InputManager) {
        .inputs = vector_new(sizeof(Input*), 1),

        .filenames = vector_new(sizeof(char*), 32),

        .quote_paths = (SearchPath) {
            .start = NULL,
            .anchor = &manager->quote_paths.start
        },

        .bracket_paths = (SearchPath) {
            .start = NULL,
            .anchor = &manager->bracket_paths.start
        },

        .system_paths = (SearchPath) {
            .start = NULL,
            .anchor = &manager->system_paths.start
        },

        .after_paths = (SearchPath) {
            .start = NULL,
            .anchor = &manager->after_paths.start
        }
    };

    input_manager_add_system_path(manager, 
            (char*) "/usr/lib/gcc/x86_64-linux-gnu/11/include");
    input_manager_add_system_path(manager, 
            (char*) "/usr/local/include");
    input_manager_add_system_path(manager, 
            (char*) "/usr/include/x86_64-linux-gnu");
    input_manager_add_system_path(manager, 
            (char*) "/usr/include");

    return manager;
}

void input_manager_delete(InputManager* manager)
{   
    // Free our searchpath structs
    SearchPathEntry* entry = manager->quote_paths.start;
    while (entry != NULL) {
        SearchPathEntry* tmp = entry;
        entry = entry->next;

        free(tmp);
    }

    // Free our inputs
    for (size_t i = 0; i < vector_get_count(&manager->inputs); i++)
    {
        Input* input = vector_get(&manager->inputs, i);
        input_delete(input);
    }
    vector_delete(&manager->inputs);
    
    // Free our filenames
    for (size_t i = 0; i < vector_get_count(&manager->filenames); i++)
    {
        char** filename = vector_get(&manager->filenames, i);
        free(*filename);
    }
    vector_delete(&manager->filenames);

    // free the manager itself
    free(manager);
}

char* input_manager_allocate_filename_buffer(InputManager* manager, size_t len)
{
    panic("unimplemented: input_manager_allocate_filename_buffer");
    return NULL;
}

char* input_manager_allocate_filename(InputManager* manager, char* filename)
{
    size_t len = strlen(filename) + 1;
    char* new_buffer = xmalloc(sizeof(char) * len);
    strcpy(new_buffer, filename);

    vector_push(&manager->filenames, &new_buffer);

    return new_buffer;
}

char* input_manager_allocate_filename_len(InputManager* manager, char* filename,
        size_t len)
{
    char* new_buffer = xmalloc(sizeof(char) * (len + 1));
    strncpy(new_buffer, filename, len);
    new_buffer[len] = '\0';

    vector_push(&manager->filenames, &new_buffer);

    return new_buffer;
}

void add_searchpath(InputManager* manager, SearchPath* path, char* filename, 
        bool sys)
{
    char* allocated_filename = 
            input_manager_allocate_filename(manager, filename);

    SearchPathEntry* new_entry = searchpath_entry_new(allocated_filename, sys);

    *path->anchor = new_entry;
    path->anchor = &new_entry->next;
}

void input_manager_add_quote_path(InputManager* manager, char* filepath)
{
    add_searchpath(manager, &manager->quote_paths, filepath, false);
}

void input_manager_add_bracket_path(InputManager* manager, char* filepath)
{
    add_searchpath(manager, &manager->bracket_paths, filepath, 
            false);
}

void input_manager_add_system_path(InputManager* manager, char* filepath)
{
    add_searchpath(manager, &manager->system_paths, filepath, true);
}

void input_manager_add_after_path(InputManager* manager, char* filepath)
{
    add_searchpath(manager, &manager->after_paths, filepath, true);
}

void input_manager_finish_setup(InputManager* manager)
{
    searchpath_append(&manager->system_paths, &manager->after_paths);
    searchpath_append(&manager->bracket_paths, &manager->system_paths);
    searchpath_append(&manager->quote_paths, &manager->bracket_paths);
}

void input_manager_print_include_paths(InputManager* manager)
{
    fprintf(stderr, "#include \"...\" search starts here:\n");
    for (SearchPathEntry* entry = manager->quote_paths.start; 
            entry != manager->bracket_paths.start; entry = entry->next) {
        fprintf(stderr, " %s\n", entry->filepath);
    }

    fprintf(stderr, "#include <...> search starts here:\n");
    for (SearchPathEntry* entry = manager->bracket_paths.start; entry != NULL; 
        entry = entry->next) {
        fprintf(stderr, " %s\n", entry->filepath);
    }

    fprintf(stderr, "End of search list.\n\n");
}

FILE* input_manager_get_file(InputManager* manager, char* filepath);
FILE* input_manager_find_file(InputManager* manager, char* filename, 
        SearchPathEntry* entry, char* current_file);

Input* input_manager_get_input(InputManager* manager, char* filename);

// TODO: maybe add function input_manager_get_input(...)

Input* input_manager_find_real_file(InputManager* manager, char* filename)
{
    for (size_t i = 0; i < vector_get_count(&manager->inputs); i++)
    {
        Input* input = vector_get(&manager->inputs, i);
        
        if (!strcmp(input->filename, filename))
        {
            return input;
        }
    }

    return NULL;
}

Line* input_manager_find_real_line(InputManager* manager, char* filename,
        size_t real_line)
{
    Input* input = input_manager_find_real_file(manager, filename);

    if (!input)
    {
        return NULL;
    }

    return input_find_real_line(input, real_line);
}









