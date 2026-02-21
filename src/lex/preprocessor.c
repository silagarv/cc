#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "util/arena.h"
#include "util/buffer.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"

struct LexerStack {
    LexerStack* prev;
    Lexer lexer;
};

static bool lexer_stack_has_below(const LexerStack* lexers)
{
    assert(lexers != NULL);
    return lexers->prev != NULL;
}

// static void lexer_stack_push(LexerStack* lexers, Lexer lexer)
// {

// }

// static void lexer_stack_pop(LexerStack* lexers);



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

bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids)
{
    SourceFile* starting_file = source_manager_create_filepath(sm, main_file);
    if (starting_file == NULL)
    {
        diagnostic_error(dm, "no such file or directory: '%s'", main_file.path);
        return false;
    }

    // Since we have got a main file we can now set the source managers main
    // file.
    source_manager_set_main_file(sm, starting_file);

    pp->dm = dm;
    pp->lang = opts;    
    pp->sm = sm;
    pp->identifiers = ids;
    pp->literal_arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE,
            ARENA_DEFAULT_ALIGNMENT);
    pp->pp_allocator = arena_new(ARENA_DEFAULT_CHUNK_SIZE,
            ARENA_DEFAULT_ALIGNMENT);
    pp->lexers = NULL;
    lexer_create(&pp->lexer, dm, opts, &pp->literal_arena, pp->identifiers,
            starting_file);
    pp->cache = token_list(arena_new_default());

    return true;
}

void preprocessor_delete(Preprocessor* pp)
{
    token_list_free(&pp->cache); // Free first since some tokens use other arena
    arena_delete(&pp->literal_arena);
    arena_delete(&pp->pp_allocator);
}

static bool preprocessor_get_next(Preprocessor* pp, Token* token, bool cache)
{
    bool ret = lexer_get_next(&pp->lexer, token);
    if (token_is_type(token, TOK_IDENTIFIER))
    {
        token_classify_identifier(token);
    }

    if (cache)
    {
        token_list_push_back(&pp->cache, *token);
    }

    return ret;
}

bool preprocessor_advance_token(Preprocessor* pp, Token* token)
{
    // If we have cached tokens, pop them from the front of the list and thats
    // how we get the next token.
    if (!token_list_empty(&pp->cache))
    {
        *token = token_list_pop_front(&pp->cache);
        return true;
    }

    // Otherwise get but don't cache the next token.
    return preprocessor_get_next(pp, token, false);
}

bool preprocessor_peek_token(Preprocessor* pp, Token* token)
{
    // If we have cached tokens, simply peek the front token of the list and
    // return that token to us. As there is no work to do here.
    if (!token_list_empty(&pp->cache))
    {
        *token = token_list_peek_front(&pp->cache);
        return true;
    }

    // Otherwise get and cache the next token for us.
    return preprocessor_get_next(pp, token, true);
}

void preprocessor_insert_token(Preprocessor* pp, Token token)
{
    token_list_push_front(&pp->cache, token);
}

