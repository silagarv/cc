#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "driver/diagnostic.h"
#include "files/file_manager.h"
#include "files/filepath.h"
#include "files/source_manager.h"
#include "lex/identifier_table.h"
#include "lex/lexer.h"
#include "util/arena.h"
#include "util/buffer.h"
#include "util/str.h"

#include "lex/token.h"

// Print a quoted include filename into a buffer
static void buffer_add_include(Buffer* buffer, const char* filename)
{
    buffer_printf(buffer, "#include \"%s\"\n");
}

// Print a basic define into a buffer
static void buffer_add_define_simple(Buffer* buffer, const char* define)
{
    buffer_printf(buffer, "#define %s\n", define);
}

// Print a define into a buffer with a given value
static void buffer_add_define_value(Buffer* buffer, const char* define, 
        const char* value)
{
    buffer_printf(buffer, "#define %s %s\n", define, value);
}

static void preprocessor_add_defines(Preprocessor* pp)
{
    // Create buffer add defines and turn it into a filebuffer
    // Buffer predefine_buffer = buffer_new();

    // buffer_add_define_value(&predefine_buffer, "__STDC__", "1");
}


// TODO: this was a weird fix to need to make? Should I file a bug report or 
// TODO: even determine if this was a gcc bug.
void preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        SourceManager* sm, SourceFile* starting_file)
{
    pp->dm = dm;
    pp->sm = sm;
    pp->identifiers = identifier_table_create();
    pp->literal_arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE,
            ARENA_DEFAULT_ALIGNMENT);
    pp->lexer = lexer_create(dm, &pp->literal_arena, &pp->identifiers,
            starting_file);
}

void preprocessor_delete(Preprocessor* pp)
{
    arena_delete(&pp->literal_arena);
    identifier_table_delete(&pp->identifiers);
}

bool preprocessor_advance_token(Preprocessor* pp, Token* token)
{
    return lexer_get_next(&pp->lexer, token);
}

bool preprocessor_peek_token(Preprocessor* pp, Token* token)
{
    return lexer_peek(&pp->lexer, token);
}

TokenType preprocessor_peek_next_token_type(Preprocessor* pp)
{
    return lexer_get_next_next_type(&pp->lexer);
}

