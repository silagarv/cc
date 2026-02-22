#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "driver/warning.h"
#include "files/location.h"
#include "lex/pp_conditional.h"
#include "util/arena.h"
#include "util/buffer.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"

// An input stack represents the inputs that we are using during preprocessing.
// Each level on the input stack constails information about the current 
// processing of conditionals and the lexer for them
struct InputStack {
    InputStack* prev; // the previous input on the stack
    unsigned int depth; // the current input depth (starts at 0)
    Lexer lexer; // The lexer for this input
    ConditionalStack conidtionals; // The current conditionals for the input
};

InputStack* input_stack_prev(const InputStack* current)
{
    assert(current != NULL);
    return current->prev;
}

unsigned int input_stack_depth(const InputStack* current)
{
    assert(current != NULL);
    return current->depth;
}

ConditionalStack* input_stack_conditionals(const InputStack* current)
{
    assert(current != NULL);
    // This is okay sice it will never refer to properly const memory anyways
    return (ConditionalStack*) &current->conidtionals;
}

InputStack* input_stack_push(Arena* pp_allocator, InputStack* current,
        unsigned int depth, DiagnosticManager* dm, LangOptions* lang,
        Arena* literals, IdentifierTable* ids, SourceFile* source)
{
    InputStack* new_entry = arena_allocate_size(pp_allocator,
            sizeof(InputStack));
    *new_entry = (InputStack)
    {
        .prev = current,
        .depth = depth,
        .lexer = (Lexer) {0}, // Cannot create here due to lexer API
        .conidtionals = conditional_stack_create(pp_allocator)
    };

    // Also, don't forget to create our lexer. Note, that this cannot fail since
    // we already have all everything we need.
    lexer_create(&new_entry->lexer, dm, lang, literals, ids, source);

    return new_entry;
}

InputStack* input_stack_pop(const InputStack* current)
{
    return input_stack_prev(current);
}

bool input_stack_has_below(const InputStack* current)
{
    return input_stack_prev(current) != NULL;
}

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

static bool preprocessor_push_input(Preprocessor* pp, SourceFile* file)
{
    unsigned int depth;
    if (pp->inputs == NULL)
    {
        depth = 0;
    }
    else
    {
        depth = input_stack_depth(pp->inputs) + 1;
    }

    if (depth >= pp->max_depth)
    {
        return false;
    }

    pp->inputs = input_stack_push(&pp->pp_allocator, pp->inputs, depth, pp->dm,
            pp->lang, &pp->literal_arena, pp->identifiers, file);
    return true;
}

// Pop the current input from the from all of the inputs in the preprocessor.
// This also handles things like checking the conditional stack is empty before
// we continue on.
static void preprocessor_pop_input(Preprocessor* pp)
{
    // Get the current input from the input stack
    InputStack* current = pp->inputs;

    // Check for unterminated conditionals in this input
    while (!conditional_stack_empty(&current->conidtionals))
    {
        Location location = conditional_stack_location(&current->conidtionals);
        diagnostic_error_at(pp->dm, location, "unterminated conditional "
                "directive");
        conditional_stack_pop(&current->conidtionals);
    }

    // Finally, get the previous input and set that to our current
    pp->inputs = input_stack_prev(current);
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
    pp->max_depth = 200;
    pp->inputs = NULL;
    pp->cache = token_list(arena_new_default());

    // Finally, put our input onto the stack so that we are ready to start 
    // lexing our tokens. Don't check the return value as this should never
    // fail.
    preprocessor_push_input(pp, starting_file);

    assert(pp->inputs && "Should have an input");
    return true;
}

void preprocessor_delete(Preprocessor* pp)
{
    token_list_free(&pp->cache); // Free first since some tokens use other arena
    arena_delete(&pp->literal_arena);
    arena_delete(&pp->pp_allocator);
}

// Lex the next token from the preprocessor's stack of lexer and handle popping
// all of the lexers off the stack as needed. Note that this does no handling
// of macros or any expansion work and simply just uses the lexer information
// for the next lexer.
bool preprocessor_lex_next_token(Preprocessor* pp, Token* token)
{
    assert(pp->inputs && "Can only lex tokens when we have a lexer");

    bool ret;
    do
    {
        InputStack* current = pp->inputs;
        ret = lexer_get_next(&current->lexer, token);

        // If we get EOF on this file with an input below we should pop this
        // input and then try again. Otherwise we will send and premature eof
        // token that will stop the parser.
        if (token_is_type(token, TOK_EOF))
        {
            bool has_below = input_stack_has_below(current);
            preprocessor_pop_input(pp);
            
            if(has_below)
            {
                continue;
            }
        }

        // Otherwise, no issues getting the token just return it down below.
        break;
    }
    while (true);

    return ret;
}

void preprocessor_set_directive(Preprocessor* pp)
{
    InputStack* current = pp->inputs;
    lexer_set_directive(&current->lexer);
}

static void preprocessor_parse_directive(Preprocessor* pp, Token* token)
{
    assert(token_is_type(token, TOK_HASH));
    assert(token_has_flag(token, TOK_FLAG_BOL));

    // Get the location of the token's hash to preserve if for later.
    Location hash_loc = token_get_location(token);
    
    // Enter directive mode in the lexer. So that we can sucessfully parse it.
    // Without this we would not get the needed EOD token.
    preprocessor_set_directive(pp);
    do
    {
        preprocessor_lex_next_token(pp, token);
        assert(!token_is_type(token, TOK_EOF) && "Can't have eof in directive");
    }
    while (!token_is_type(token, TOK_PP_EOD));

    // Also produce a warning abouut preprocessing directives being ignored.
    diagnostic_warning_at(pp->dm, hash_loc, Wunimplemented,
            "preprocessing directives are not implemented; ignoring");
}

static bool preprocessor_get_next(Preprocessor* pp, Token* token, bool cache)
{
    // TODO: will also have to handle skipping of conditionals and such.
    bool ret = false;
    do
    {
        // First lex the token from the current lexer as the next actions we do
        // will be determined by the token we get.
        // ret = lexer_get_next(&pp->lexer, token);
        ret = preprocessor_lex_next_token(pp, token);

        // Check if we have got a preprocessing directive here. Preprocessing
        // directives can only occur when there is a hash as the first token
        // on that line. If we have one, parse it and then try again to get the
        // next token.
        if (token_is_type(token, TOK_HASH) 
                && token_has_flag(token, TOK_FLAG_BOL))
        {
            preprocessor_parse_directive(pp, token);
            continue;
        }

        // TODO: will need to handle situations like macro expansion also with
        // TODO: the possibility of function like macros as well... This may be
        // TODO: a bit tricky to do all at once.

        // Otherwise handle the situation as normal.
        if (token_is_type(token, TOK_IDENTIFIER))
        {
            token_classify_identifier(token);
        }

        if (cache)
        {
            token_list_push_back(&pp->cache, *token);
        }

        // We got our token with nothing significant to note. We are done here.
        break;
    }
    while (true);

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

