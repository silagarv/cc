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
#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"
#include "lex/include_stack.h"
#include "lex/macro.h"
#include "lex/directives.h"
#include "lex/expand.h"
#include "lex/include_stack.h"
#include "lex/macro_map.h"

#define PREPROCESSOR_MAX_DEPTH 200

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

void preprocessor_push_input(Preprocessor* pp, SourceFile* file)
{
    assert(include_vector_size(&pp->inputs) < pp->max_depth);

    // Simply create the include and push it only the preprocessor.
    Include include = include_create(pp->dm, pp->lang, &pp->literal_arena,
            pp->identifiers, file, NULL);
    include_vector_push(&pp->inputs, include);
}

// Pop the current input from the from all of the inputs in the preprocessor.
// This also handles things like checking the conditional stack is empty before
// we continue on.
void preprocessor_pop_input(Preprocessor* pp)
{
    // Get the current input from the input stack
    Include include = include_vector_pop(&pp->inputs);

    // Check for unterminated conditionals in this input
    while (!include_conditional_empty(&include))
    {
        Location location = include_pop_conditional(&include);
        diagnostic_error_at(pp->dm, location, "unterminated conditional "
                "directive");
    }

    // Don't forget to free any memory for the include!
    include_delete(&include);
}

bool preprocessor_has_input(Preprocessor* pp)
{
    return !include_vector_empty(&pp->inputs);
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

void preprocessor_add_builtin_macro(Preprocessor* pp, Identifier* name,
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
    pp->inputs = include_vector();
    pp->cache = token_list(arena_new_default());

    // Initialise our important identifiers in the preprocessor.
    preprocessor_get_builtin_identifiers(pp);

    // Then add all the builtin macros to the preprocessor and initialise the
    // counter macro.
    pp->macros = macro_map_create();
    pp->expander = macro_expander_create(sm);
    preprocessor_add_builtin_macros(pp);

    // Finally, put our input onto the stack so that we are ready to start 
    // lexing our tokens. Don't check the return value as this should never
    // fail.
    preprocessor_push_input(pp, starting_file);
    return true;
}

void preprocessor_delete(Preprocessor* pp)
{
    macro_expander_delete(&pp->expander);
    macro_map_delete(&pp->macros);
    include_vector_free_ptr(&pp->inputs, include_delete);
    token_list_free(&pp->cache); // Free first since some tokens use other arena
    arena_delete(&pp->literal_arena);
    arena_delete(&pp->pp_allocator);
}

DiagnosticManager* preprocessor_diagnostics(Preprocessor* pp)
{
    return pp->dm;
}

SourceManager* preprocessor_source_manager(Preprocessor* pp)
{
    return pp->sm;
}

Arena* preprocessor_allocator(Preprocessor* pp)
{
    return &pp->pp_allocator;
}

IncludeVector* preprocessor_inputs(Preprocessor* pp)
{
    return &pp->inputs;
}

unsigned int preprocessor_include_depth(const Preprocessor* pp)
{
    return include_vector_size(&pp->inputs);
}

MacroMap* preprocessor_macro_map(Preprocessor* pp)
{
    return &pp->macros;
}

void preprocessor_enter_directive(Preprocessor* pp)
{
    Include* current = include_vector_back(&pp->inputs);
    Lexer* lexer = include_get_lexer(current);
    lexer_set_directive(lexer);
}

void preprocessor_read_diagnostic_string(Preprocessor* pp, Buffer* buffer)
{
    Include* current = include_vector_back(&pp->inputs);
    Lexer* lexer = include_get_lexer(current);
    lexer_read_diagnostic_string(lexer, buffer);
}

// Lex the next token from the preprocessor's stack of lexer and handle popping
// all of the lexers off the stack as needed. Note that this does no handling
// of macros or any expansion work and simply just uses the lexer information
// for the next lexer.
bool preprocessor_next_raw_token(Preprocessor* pp, Token* token)
{
    assert(!include_vector_empty(&pp->inputs) && "need a lexer for tokens!");

    bool ret;
    do
    {
        Include* current = include_vector_back(&pp->inputs);
        ret = include_get_next(current, token);

        // If we get EOF on this file with an input below we should pop this
        // input and then try again. Otherwise we will send and premature eof
        // token that will stop the parser.
        if (token_is_type(token, TOK_EOF))
        {   
            preprocessor_pop_input(pp);
            
            // If we still have input we should not claim that we are at EOF
            // instead loop and try to get token again.
            // TODO: turn into tail recursion?
            if(preprocessor_has_input(pp))
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

bool preprocessor_should_expand(Preprocessor* pp, Token* token)
{
    if (!token_is_identifier_like(token))
    {
        return false;
    }

    Identifier* name = token_get_identifier(token);
    Macro* macro = macro_map_get_macro(&pp->macros, name);
    if (macro == NULL)
    {
        return false;
    }

    if (macro_disabled(macro))
    {
        return false;
    }

    return true;
}

void preprocessor_push_macro_expansion(Preprocessor* pp, Macro* macro,
        Location location)
{
    macro_expander_push(&pp->expander, macro, location);
}

void preprocessor_start_expansion(Preprocessor* pp, Token* token)
{
    Identifier* name = token_get_identifier(token);
    Macro* macro = macro_map_get_macro(&pp->macros, name);
    assert(macro != NULL && !macro_disabled(macro) && "need an alive macro!");
    
    if (!macro_function_like(macro))
    {
        preprocessor_push_macro_expansion(pp, macro, token_get_location(token));
        return;  
    }
    else
    {
        panic("function like macros are not implemented");
    }
}

bool preprocessor_is_expanding(Preprocessor* pp)
{
    return macro_expander_expanding(&pp->expander);
}

bool preprocessor_expand_next(Preprocessor* pp, Token* token)
{
    bool ret = macro_expander_next(&pp->expander, token);
    
    // If we did not get a token from the expander our macro expansion stack
    // was exhausted. Return false to indicate this
    if (!ret)
    {
        assert(!macro_expander_expanding(&pp->expander) && "still expanding?");
        return false;
    }

    // Here we should now detect recursive macro expansions from our macro
    // expansion. Note that disabled macros are automatically taken care of
    // here so we do not need to consider them. So here we start the expansion,
    // and then since we will need to requery the macro expander we will simply
    // recurse and try again.
    if (preprocessor_should_expand(pp, token))
    {
        preprocessor_start_expansion(pp, token);
        return preprocessor_expand_next(pp, token);
    }

    return true;
}

bool preprocessor_get_next(Preprocessor* pp, Token* token)
{
    bool ret = false;
    do
    {
        // If we are currently expanding, do the expansion and return the token
        // to the calling function to this one. Note that we need to check that
        // we get an expansion token to avoid us popping the expansion early and
        // then in the case of macros like #define bool bool, we would 
        // infinitely expand and pop that macro.
        if (preprocessor_is_expanding(pp))
        {
            if (preprocessor_expand_next(pp, token))
            {
                return true;
            }
        }

        // First lex the token from the current lexer as the next actions we do
        // will be determined by the token we get.
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

        // If we get a token that should be expanded, then go and start the 
        // preprocessor expansion.
        if (preprocessor_should_expand(pp, token))
        {
            preprocessor_start_expansion(pp, token);
            continue;
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
    return preprocessor_get_next(pp, token);
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
    bool ret = preprocessor_get_next(pp, token);

    // Make sure to add the token to our list of peeked tokens
    token_list_push_back(&pp->cache, *token);
    return ret;
}

void preprocessor_insert_token(Preprocessor* pp, Token token)
{
    token_list_push_front(&pp->cache, token);
}

