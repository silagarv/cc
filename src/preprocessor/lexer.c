#include "lexer.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <time.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/buffer.h"
#include "util/static_string.h"

#include "driver/diagnostic.h"

#include "preprocessor/files.h"

static void include_directory_list_add_path(IncludeDirectoryList* dirs, 
        StaticString* path)
{
    IncludeDirectory* new_dir = xmalloc(sizeof(IncludeDirectory));
    static_string_copy(path, &new_dir->path);
    new_dir->next = NULL;

    if (!dirs->start)
    {
        // Set up the directories properly
        dirs->start = new_dir;
        dirs->last = new_dir;
    }
    else
    {
        // Simply append to the end
        dirs->last->next = new_dir;
        dirs->last = new_dir;
    }
}

static void include_directory_list_free(IncludeDirectoryList* dirs)
{
    IncludeDirectory* dir;
    for (dir = dirs->start; dir != NULL; )
    {   
        // To avoid use after free bugs
        IncludeDirectory* to_free = dir;
        dir = dir->next;

        static_string_free(&to_free->path);
        free(to_free);
    }
}

static void lexer_add_path(IncludeDirectoryList* ignored, 
        IncludeDirectoryList* dirs, StaticString* path)
{
    // Determine which path to append to
    IncludeDirectoryList* to_append = NULL;
    if (!is_directory(path))
    {
        to_append = ignored;
    }
    else 
    {
        to_append = dirs;
    }

    include_directory_list_add_path(to_append, path);
}

static void lexer_add_builtin_paths(Lexer* lexer)
{
    const char* builtins[] = {
        "/usr/include"
    };
    const size_t num_paths = sizeof(builtins) / sizeof(builtins[0]);

    for (size_t i = 0; i < num_paths; i++)
    {
        StaticString path = (StaticString)
        {
            .ptr = (char*) builtins[i],
            .len = strlen(builtins[i])
        };

        lexer_add_system_path(lexer, &path);
    }
}

void lexer_add_quote_path(Lexer* lexer, StaticString* path)
{
    lexer_add_path(&lexer->ignored_paths, &lexer->quote_paths, path);
}

void lexer_add_bracket_path(Lexer* lexer, StaticString* path)
{
    lexer_add_path(&lexer->ignored_paths, &lexer->bracket_paths, path);
}

void lexer_add_system_path(Lexer* lexer, StaticString* path)
{
    lexer_add_path(&lexer->ignored_paths, &lexer->system_paths, path);
}

void lexer_add_after_path(Lexer* lexer, StaticString* path)
{
    lexer_add_path(&lexer->ignored_paths, &lexer->after_paths, path);
}

static void buffer_add_define_definition(Buffer* buffer, const char* name, 
        const char* definition)
{
    buffer_printf(buffer, "#define %s %s\n", name, definition);
}

static void buffer_add_define(Buffer* buffer, const char* name)
{
    buffer_printf(buffer, "#define %s\n", name);
}

static void buffer_add_undef_macro(Buffer* buffer, const char* name)
{
    buffer_printf(buffer, "#undef %s\n", name);
}

static void buffer_add_include(Buffer* buffer, const char* filename)
{
    buffer_printf(buffer, "#include \"%s\"\n", filename);
}

// need some functions for parsing command line definitions lol

// returns false if failed to get...
static void get_date_macro(StaticString* str)
{
    static const char months[12][3] =
    {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };

    const time_t current_time = time(NULL);

    // if we failed to get the time
    if (current_time == (time_t)(-1))
    {
        // TODO: should we just silently fail here or issue warning?
        static_string_init_copy(str, "??? ?? ????");

        return;
    }

    // Convert to local time and build macro value
    const struct tm* local_time = localtime(&current_time);
    
    char buffer[12];
    const int wrote = snprintf(buffer, 12, "%.3s %2d %4d", 
            months[local_time->tm_mon], local_time->tm_mday, 
            local_time->tm_year + 1900);

    assert(wrote == 11);

    static_string_init_copy(str, buffer);
}

static void get_time_macro(StaticString* str)
{
    const time_t current_time = time(NULL);

    // if we failed to get the time
    if (current_time == (time_t)(-1))
    {
        // TODO: should we just silently fail here or issue warning?
        static_string_init_copy(str, "??:??:??");

        return;
    }
    
    // Convert to local time and build macro value
    const struct tm* local_time = localtime(&current_time);

    char buffer[9];
    const int wrote = snprintf(buffer, 9, "%.02d:%.02d:%.02d", 
            local_time->tm_hour, local_time->tm_min, local_time->tm_sec);

    assert(wrote == 8);

    static_string_init_copy(str, buffer);
}

void lexer_add_builtin_macros(Lexer* lexer)
{
    // although __DATE__ and __TIME__ are technically dynamic, they only really
    // need to be calculated once, any more than that is just not necessary
    StaticString date;
    StaticString time;
    get_date_macro(&date);
    get_time_macro(&time);

    buffer_add_define_definition(lexer->builtin_defines, "__DATE__", date.ptr);
    buffer_add_define_definition(lexer->builtin_defines, "__TIME__", time.ptr);

    static_string_free(&date);
    static_string_free(&time);

    buffer_add_define_definition(lexer->builtin_defines, "__STDC__", "1");
    buffer_add_define_definition(lexer->builtin_defines, "__STDC_VERSION__", "199901L");
    buffer_add_define_definition(lexer->builtin_defines, "__STDC_HOSTED__", "1");
    
    // NOTE: add more of this stuff if needed (maybe we don't IDK)
}

void lexer_undef_builtin_macros(Lexer* lexer)
{
    // Nothing to do here??? since we only define bare minimum standard macros
    // might need to implement more in the future
}

void lexer_add_include_path(Lexer* lexer, StaticString* path);

void lexer_add_command_line_macro(Lexer* lexer, StaticString* definition)
{
    // TODO: change this so we actually use the StaticString properly 
    // TODO: instead of getting the ptr and cheesing it...
    char* current_ptr = definition->ptr;
    while (*current_ptr != '\0' && *current_ptr != '=')
    {
        current_ptr++;
    }

    if (*current_ptr == '\0')
    {
        // Simple case, macro has no '=' so we just put it in as is
        buffer_add_define(lexer->command_line_macros, definition->ptr);
    }
    else
    {   
        // TODO: below gross?

        // Complex case, macro has '=' so need to get the first and last part
        // of the command line argument

        // Copy the first part into a new string add define then free string
        StaticString first_part;
        static_string_copy_len(definition, &first_part, 
                current_ptr - definition->ptr);

        const char* last_part = current_ptr + 1;

        buffer_add_define_definition(lexer->command_line_macros, first_part.ptr, 
                last_part);

        static_string_free(&first_part);
    }
}

void lexer_add_command_line_undef(Lexer* lexer, StaticString* undef)
{
    buffer_add_undef_macro(lexer->command_line_macros, undef->ptr);
}

// void lexer_add_command_line_imacros(Lexer* lexer, StaticString* filename);

void lexer_add_command_line_include(Lexer* lexer, StaticString* filename)
{
    buffer_add_include(lexer->command_line_include, filename->ptr);
}

// for a nice quick skip to end of line
static void lexer_dispose_line(Lexer* lexer)
{
    lexer->has_line = false;
}

// TODO: make sure we register the line
static bool lexer_get_next_line(Lexer* lexer)
{
    assert(!lexer->has_line);   

    // Reset lexing position
    lexer->line_pos = 0;

    // Reset lexer line flags
    lexer->has_whitespace = false;
    lexer->is_line_start = true;
    
    lexer->is_preproc_line = false;
    lexer->can_lex_directive_name = false;
    lexer->can_lex_header_name = false;

retry:
    // No source means no lines possible
    if (!lexer->souce_stack)
    {
        return false;
    }

    // if we don't get a line pop top and retry
    if (!line_read_from_buffered_source(lexer->souce_stack, 
            &lexer->current_line))
    {
        lexer->souce_stack = buffered_source_pop(lexer->souce_stack);

        goto retry;
    }

    lexer->has_line = true;

    return true;
}

static int lexer_get_char(Lexer* lexer)
{
    if (!lexer->has_line && !lexer_get_next_line(lexer))
    {
        return EOF;
    }

    char c = line_get_char(&lexer->current_line, lexer->line_pos++);

    // If we get a newline we are done with this
    if (c == '\n')
    {
        assert(lexer->line_pos == line_get_length(&lexer->current_line));

        line_free(&lexer->current_line);

        lexer_dispose_line(lexer);
    }

    return c;
}

static int lexer_curr_char(Lexer* lexer)
{
    assert(lexer->has_line);

    return line_get_char(&lexer->current_line, lexer->line_pos);
}

static void lexer_unget_char(Lexer* lexer)
{
    assert(lexer->has_line);

    if (!lexer->line_pos)
    {
        panic("cannot unget char when at 0th position");
    }

    lexer->line_pos--;
}

static void tokenise_identifier(Lexer* lexer)
{
    int curr = '\0';
    // We are going to do it this way to see if there are any speed
    // advantages here or if we are just wasting time
    switch (curr)
    {
        // goto lex_keyword_fail;

// lex_keyword_fail:
        case 'a': case 'b': case 'c': case 'd':
        case 'e': case 'f': case 'g': case 'h':
        case 'i': case 'j': case 'k': case 'l':
        case 'm': case 'n': case 'o': case 'p':
        case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x':
        case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D':
        case 'E': case 'F': case 'G': case 'H':
        case 'I': case 'J': case 'K': case 'L':
        case 'M': case 'N': case 'O': case 'P':
        case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X':
        case 'Y': case 'Z':
        case '_':

            break;

        default:
            panic("not a valid identifier starter");
            break;
    }
}

static TokenType lexer_get_next_token(Lexer* lexer, TokenList* tokens);

void lexer_initialise(Lexer* lexer, void* line_map, void* location_map)
{
    // TODO: init macro map and other things here
    lexer->builtin_defines = buffer_new();
    lexer->command_line_macros = buffer_new();
    lexer->command_line_include = buffer_new();

    lexer_add_builtin_paths(lexer);

    lexer_add_builtin_macros(lexer);
}

void lexer_finalise(Lexer* lexer)
{
    // TODO: implement lexer finalisation before tokenising things
}

void lexer_close(Lexer* lexer)
{
    if (lexer->builtin_defines)
    {
        buffer_free(lexer->builtin_defines);
    }

    if (lexer->command_line_macros)
    {
        buffer_free(lexer->command_line_macros);
    }

    if (lexer->command_line_include)
    {
        buffer_free(lexer->command_line_include);
    }

    // Free all of our paths including our ignored paths
    include_directory_list_free(&lexer->quote_paths);
    include_directory_list_free(&lexer->bracket_paths);
    include_directory_list_free(&lexer->system_paths);
    include_directory_list_free(&lexer->after_paths);

    include_directory_list_free(&lexer->ignored_paths);

    while (lexer->souce_stack)
    {
        lexer->souce_stack = buffered_source_pop(lexer->souce_stack);
    }
}

int lexer_push_start_file(Lexer* lexer, StaticString* filename)
{
    if (!is_file(filename))
    {
        fatal_error("cannot find or open file '%s'", filename->ptr);

        return 1;
    }

    // We know it is a file here
    FILE* file = fopen(filename->ptr, "r");
    BufferedSource* start_source = buffered_source_from_file(file, 
            filename->ptr);

    lexer->souce_stack = buffered_source_push(lexer->souce_stack, start_source);
    
    return 0;
}

int lexer_tokenise(Lexer* lexer, TokenList* tokens)
{
    assert(lexer->souce_stack && "lexer doesn't have a source to lex from");

    int c;
    while (c = lexer_get_char(lexer), c != EOF)
    {
        // printf("%c", c);
        // printf("%s:%u:\n", lexer->current_line.source_real_name, lexer->current_line.real_line_no);
        // printf("%s", line_get_ptr(&lexer->current_line));
        // line_free(&lexer->current_line);
    }

    return 0;
}




