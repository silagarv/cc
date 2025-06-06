#include "lexer.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#include "util/buffer.h"
#include "util/static_string.h"

#include "driver/diagnostic.h"

#include "preprocessor/files.h"

void lexer_init(Lexer* lexer, void* line_map, void* location_map)
{
    // TODO: init macro map and other things here
}

void lexer_finish(Lexer* lexer)
{
    while (lexer->souce_stack)
    {
        lexer->souce_stack = buffered_source_pop(lexer->souce_stack);
    }
}

bool lexer_push_start_file(Lexer* lexer, StaticString* filename)
{
    if (!is_file(filename))
    {
        fatal_error("cannot find or open file '%s'", filename->ptr);

        return false;
    }

    // We know it is a file here
    FILE* file = fopen(filename->ptr, "r");
    BufferedSource* start_source = buffered_source_from_file(file, 
            filename->ptr);

    lexer->souce_stack = buffered_source_push(lexer->souce_stack, start_source);

    return true;
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

void lexer_add_builtin_macros(Lexer* lexer)
{
    Buffer* builtin_buffer = buffer_new();
    buffer_add_define_definition(builtin_buffer, "__STDC__", "1");
    buffer_add_define_definition(builtin_buffer, "__STDC_VERSION__", "199901L");
    // TODO: add more of this stuff

    
    buffer_free(builtin_buffer);
}

bool lexer_tokenise(Lexer* lexer, TokenList* tokens)
{
    Line line;
    while (line_read_from_buffered_source(lexer->souce_stack, &line))
    {
        printf("%s:%u:\n", line.source_real_name, line.real_line_no);
        printf("%s", line_get_ptr(&line));
        line_free(&line);
    }

    return true;
}




