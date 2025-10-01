#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "files/file_manager.h"
#include "files/filepath.h"
#include "files/source_manager.h"
#include "lex/lexer.h"
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


Preprocessor preprocessor_create(SourceManager* sm, 
        IdentifierTable* identifiers,SourceFile* starting_file)
{
    Preprocessor pp = (Preprocessor)
    {
        .sm = sm,
        .identifiers = identifiers,
        .lexer = lexer_create(identifiers, starting_file)
    };

    return pp;
}

bool preprocessor_advance_token(Preprocessor* pp, Token* token)
{
    return lexer_get_next(&pp->lexer, token);
}

TokenType preprocessor_peek_next_token_type(Preprocessor* pp)
{
    return lexer_get_next_next_type(&pp->lexer);
}

