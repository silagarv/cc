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
static void buffer_add_quote_include(Buffer* buffer, const char* filename)
{
    buffer_printf(buffer, "#include \"%s\"\n");
}

static void buffer_add_angle_include(Buffer* buffer, const char* filename)
{
    buffer_printf(buffer, "#include <%s>\n");
}

static void buffer_add_define_one(Buffer* buffer, const char* define)
{
    buffer_printf(buffer, "#define %s 1\n", define);
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
    Buffer predefs = buffer_new();

    // STDC defines
    buffer_add_define_one(&predefs, "__STDC__");
    buffer_add_define_value(&predefs, "__STDC_VERSION__", "199901L"); 

    // Endian defines
    buffer_add_define_value(&predefs, "__BYTE_ORDER__",
            "__ORDER_LITTLE_ENDIAN__");
    buffer_add_define_value(&predefs, "__ORDER_BIG_ENDIAN__", "4321");
    buffer_add_define_value(&predefs, "__ORDER_LITTLE_ENDIAN__", "1234");
    buffer_add_define_value(&predefs, "__ORDER_PDP_ENDIAN__", "3412");
    buffer_add_define_one(&predefs, "__LITTLE_ENDIAN__");

    // Pointer type defines
    buffer_add_define_value(&predefs, "__POINTER_WIDTH__", "64");

    buffer_add_define_value(&predefs, "__PTRDIFF_TYPE__", "long int");
    buffer_add_define_value(&predefs, "__PTRDIFF_WIDTH__", "64");

    // Misc defines
    buffer_add_define_one(&predefs, "__linux");
    buffer_add_define_one(&predefs, "__linux__");
    buffer_add_define_one(&predefs, "linux");
    buffer_add_define_one(&predefs, "__unix");
    buffer_add_define_one(&predefs, "__unix__");
    buffer_add_define_one(&predefs, "unix");
    buffer_add_define_one(&predefs, "__x86-64");
    buffer_add_define_one(&predefs, "__x86-64__");

    buffer_free(&predefs);
}

void preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        SourceManager* sm, SourceFile* starting_file)
{
    // preprocessor_add_defines(pp);

    pp->dm = dm;
    pp->sm = sm;
    pp->identifiers = identifier_table_create();
    pp->literal_arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE,
            ARENA_DEFAULT_ALIGNMENT);
    lexer_create(&pp->lexer, dm, &pp->literal_arena, &pp->identifiers,
            starting_file);
}

void preprocessor_delete(Preprocessor* pp)
{
    arena_delete(&pp->literal_arena);
    identifier_table_delete(&pp->identifiers);
}

bool preprocessor_advance_token(Preprocessor* pp, Token* token)
{
    // if (!lexer_get_next(&pp->lexer, token))
    // {
    //     return false;
    // }

    // if (token_is_type(token, TOK_HASH) && token_has_flag(token, TOKEN_FLAG_BOL))
    // {
    //     pp->lexer.lexing_directive = true;

    //     printf("hash at srart of line\n");
    //     while (!token_is_type(token, TOK_PP_EOD))
    //     {
    //         if (!lexer_get_next(&pp->lexer, token))
    //         {
    //             return false;
    //         }
    //     }

    //     return preprocessor_advance_token(pp, token);
    // }

    // return true;

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

