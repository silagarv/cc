#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "lex/macro_map.h"
#include "util/arena.h"
#include "util/buffer.h"
#include "util/vec.h"
#include "util/ptr_set.h"

#include "driver/warning.h"
#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"
#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"
#include "lex/pp_conditional.h"
#include "lex/macro.h"
#include "lex/directives.h"

#define PREPROCESSOR_MAX_DEPTH 200

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
    InputStack* new_entry = arena_malloc(pp_allocator,
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
static void buffer_add_define_empty(Buffer* buffer, const char* define)
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

void preprocessor_get_builtin_identifiers(Preprocessor* pp)
{
    assert(pp->identifiers != NULL && "Need the identifier table for this!");

    // All of these are a simple call to our identifier table to get all of 
    // these nice and easily for us.
    pp->id___VA_ARGS__ = identifier_table_get(pp->identifiers, "__VA_ARGS__");
    pp->id___LINE__ = identifier_table_get(pp->identifiers, "__LINE__");
    pp->id___FILE__ = identifier_table_get(pp->identifiers, "__FILE__");
    pp->id___DATE__ = identifier_table_get(pp->identifiers, "__DATE__");
    pp->id___TIME__ = identifier_table_get(pp->identifiers, "__TIME__");
    pp->id___INCLUDE_LEVEL__ = identifier_table_get(pp->identifiers,
            "__INCLUDE_LEVEL__");
    pp->id___COUNTER__ = identifier_table_get(pp->identifiers, "__COUNTER__");
    pp->id__Pragma = identifier_table_get(pp->identifiers, "_Pragma");
}

static void preprocessor_add_builtin_macro(Preprocessor* pp, Identifier* name,
        bool pragma)
{
    Arena* allocator = macro_map_allocator(&pp->macros);
    Macro* macro = macro_create_builtin(allocator, name, pragma);
    macro_map_do_define(&pp->macros, pp->dm, macro);
}

void preprocessor_add_builtin_macros(Preprocessor* pp)
{
    preprocessor_add_builtin_macro(pp, pp->id___LINE__, false);
    preprocessor_add_builtin_macro(pp, pp->id___FILE__, false);
    preprocessor_add_builtin_macro(pp, pp->id___DATE__, false);
    preprocessor_add_builtin_macro(pp, pp->id___TIME__, false);
    preprocessor_add_builtin_macro(pp, pp->id___INCLUDE_LEVEL__, false);
    preprocessor_add_builtin_macro(pp, pp->id___COUNTER__, false);
    preprocessor_add_builtin_macro(pp, pp->id__Pragma, true);
}

bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids)
{
    // Before doing anything clear the memory that the preprocessor holds so 
    // that we hopefully crash instead of getting erroneous results.
    *pp = (Preprocessor) {0};

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

    pp->literal_arena = arena_new_default();
    pp->pp_allocator = arena_new_default();
    
    pp->max_depth = PREPROCESSOR_MAX_DEPTH;
    pp->inputs = NULL;
    pp->cache = token_list(arena_new_default());

    // Initialise our important identifiers in the preprocessor.
    preprocessor_get_builtin_identifiers(pp);

    // Then add all the builtin macros to the preprocessor.
    pp->macros = macro_map_create();
    preprocessor_add_builtin_macros(pp);

    // Finally, put our input onto the stack so that we are ready to start 
    // lexing our tokens. Don't check the return value as this should never
    // fail.
    preprocessor_push_input(pp, starting_file);

    assert(pp->inputs && "Should have an input");
    return true;
}

void preprocessor_delete(Preprocessor* pp)
{
    macro_map_delete(&pp->macros);
    token_list_free(&pp->cache); // Free first since some tokens use other arena
    arena_delete(&pp->literal_arena);
    arena_delete(&pp->pp_allocator);
}

Arena* preprocessor_allocator(Preprocessor* pp)
{
    return &pp->pp_allocator;
}

void preprocessor_enter_directive(Preprocessor* pp)
{
    InputStack* current = pp->inputs;
    lexer_set_directive(&current->lexer);
}

// Lex the next token from the preprocessor's stack of lexer and handle popping
// all of the lexers off the stack as needed. Note that this does no handling
// of macros or any expansion work and simply just uses the lexer information
// for the next lexer.
bool preprocessor_next_raw_token(Preprocessor* pp, Token* token)
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

static bool preprocessor_get_next(Preprocessor* pp, Token* token, bool cache)
{
    // TODO: will also have to handle skipping of conditionals and such.
    bool ret = false;
    do
    {
        // First lex the token from the current lexer as the next actions we do
        // will be determined by the token we get.
        // ret = lexer_get_next(&pp->lexer, token);
        ret = preprocessor_next_raw_token(pp, token);

        // Check if we have got a preprocessing directive here. Preprocessing
        // directives can only occur when there is a hash as the first token
        // on that line. If we have one, parse it and then try again to get the
        // next token. Even if we are caching our token then we will need to
        // handle this directive to get to our next token. So handle it.
        if (preprocessor_directive_start(pp, token))
        {
            preprocessor_parse_directive(pp, token);
            continue;
        }

        // TODO: will need to handle situations like macro expansion also with
        // TODO: the possibility of function like macros as well... This may be
        // TODO: a bit tricky to do all at once.

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

