#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "util/arena.h"
#include "util/buffer.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"
#include "files/location.h"
#include "files/line_map.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"
#include "lex/include_stack.h"
#include "lex/macro.h"
#include "lex/directives.h"
#include "lex/header_finder.h"
#include "lex/include_stack.h"
#include "lex/macro_map.h"

#define PREPROCESSOR_MAX_DEPTH 200
#define PREPROCESSOR_COUNTER_MAX INT_MAX

// Structure which represents the tokens used in the argument to a macro 
// invocation. This struct does not own the tokens, rather these tokens are 
// collected by a macro expander from a token list. Also note that this is
// only counting the tokens that are actually part of the argument. For every
// valid macro argument a TOK_PP_ENDARG token exists at tokens[num_tokens]
typedef struct MacroArgs {
    Token* tokens;
    size_t num_tokens;
} MacroArgs;

// A structure to hold and represent the current state of a macro expansion. 
// This gives us the raw tokens from the body of the macro and 
struct MacroExpansion {
    Macro* macro; // The macro currently being expanded

    MacroArgs* args; // An allocated list of macro args structures which 
                     // represent the tokens that should be used for each 
                     // argument in the macro expansion. Or NULL if the macro is
                     // object like. Note that args is guaranteed to be as big
                     // as the number of macro parameters.

    TokenStream tokens; // The stream of tokens that we are currently expanding
                        // This also helps store the current position in the 
                        // expansion for us.

    Location location; // The location that this macro was invoked at.

    struct MacroExpansion* prev; // The previous macro expansion which is the
                                 // one that triggered this one or NULL
};

// Forward declaration for builtin macros and expansion of function like macros
// and also other functions which are needed in order for the preprocessor to
// work
TokenStream preprocessor_expand_builtin_macro(Preprocessor* pp,
        const Macro* macro, Location location);
TokenStream preprocessor_expand_function_macro(Preprocessor* pp,
        const Macro* macro, const MacroArgs* args, Location location);
bool preprocessor_get_next(Preprocessor* pp, Token* token);

MacroArgs macro_args_create(Token* tokens, size_t num_tokens)
{
    return (MacroArgs) { tokens, num_tokens };
}

TokenStream macro_args_get_stream(const MacroArgs* arg)
{
    return token_stream_create(arg->tokens, arg->num_tokens);
}

Token* macro_arg_tokens(const MacroArgs* arg)
{
    return arg->tokens;
}

size_t macro_arg_num_tokens(const MacroArgs* arg)
{
    return arg->num_tokens;
}

// Get the replacement list for a particular macro invocation. For object like
// macros this function get's it's replacement list directly from the macro
// whereas with builtin and function like macros, there is the possibility that
// we will have to call out to the preprocessor to help us expand the macros.
TokenStream macro_expansion_replacement_list(Arena* arena, Macro* macro,
        MacroArgs* args, Location location, Preprocessor* pp)
{
    // If we are a built in macro we will do some special processing by the 
    // preprocessor in order to properly be expanded. So go off and handle this
    // properly returning a replacement token stream for us to lex
    if (macro_builtin(macro))
    {
        assert(args == NULL && "builtin macro with arguments?");
        return preprocessor_expand_builtin_macro(pp, macro, location);
    }

    // If we are not a function like macro then the replacement list is simply
    // the replacement list of the macro (i.e. no macro parameters that we need
    // to be able to handle)
    if (!macro_function_like(macro))
    {
        assert(args == NULL && "object macro with arguments?");
        return macro_get_stream(macro);
    }
    
    return preprocessor_expand_function_macro(pp, macro, args, location);
}

MacroExpansion* macro_expansion_create(Arena* allocator, Macro* macro,
        MacroArgs* args, Location location, MacroExpansion* prev,
        Preprocessor* pp)
{
    // Determine the replacement list of the macro here. Note that we may have
    // to do some expansion of macros in the replacement list for this.
    TokenStream replacement_list = macro_expansion_replacement_list(allocator,
            macro, args, location, pp);

    // Finally, once we have go the replacement list finish creating the macro
    // expansion structure and push it.
    MacroExpansion* expansion = arena_malloc(allocator, sizeof(MacroExpansion));
    *expansion = (MacroExpansion)
    {
        .macro = macro,
        .args = args,
        .tokens = replacement_list,
        .location = location,
        .prev = prev
    };

    return expansion;
}

// Create a macro expansion object for an argument expansion
MacroExpansion* macro_expansion_create_arg(Arena* allocator, Token* tokens,
        size_t num_tokens, Location location, MacroExpansion* prev)
{
    MacroExpansion* expansion = arena_malloc(allocator, sizeof(MacroExpansion));
    *expansion = (MacroExpansion)
    {
        .macro = NULL,
        .args = NULL,
        .tokens = token_stream_create(tokens, num_tokens),
        .location = location,
        .prev = prev
    };

    return expansion;
}

Macro* macro_expansion_macro(const MacroExpansion* expansion)
{
    assert(expansion != NULL && "need expansion");
    return expansion->macro;
}

TokenStream* macro_expansion_tokens(MacroExpansion* expansion)
{
    assert(expansion != NULL && "need expansion");
    return &expansion->tokens;
}

Location macro_expansion_location(const MacroExpansion* expansion)
{
    assert(expansion != NULL && "need expansion");
    return expansion->location;   
}

MacroExpansion* macro_expansion_prev(const MacroExpansion* expansion)
{
    assert(expansion != NULL && "need expansion");
    return expansion->prev;
}

// Special function to test if we are doing an argument expansion
bool macro_expansion_arg(const MacroExpansion* expansion)
{
    return expansion->macro == NULL;
}

Token macro_expansion_consume(MacroExpansion* expansion)
{
    assert(!token_stream_end(&expansion->tokens) && "stream exhausted!");
    return token_stream_consume(&expansion->tokens);
}

Token macro_expansion_peek(const MacroExpansion* expansion)
{
    assert(!token_stream_end(&expansion->tokens) && "stream exhausted!");
    return token_stream_peek(&expansion->tokens);
}

bool macro_expansion_finished(const MacroExpansion* expansion)
{
    return token_stream_end(&expansion->tokens);
}

MacroExpander macro_expander_create(SourceManager* sm)
{
    return (MacroExpander) { arena_new_default(), NULL };
}

void macro_expander_delete(MacroExpander* expander)
{
    arena_delete(&expander->allocator);
}

Arena* macro_expander_allocator(MacroExpander* expander)
{
    return &expander->allocator;
}

bool macro_expander_expanding(const MacroExpander* expander)
{
    return expander->expansion != NULL;
}

void macro_expander_push(MacroExpander* expander, Macro* macro, MacroArgs* args,
        Location location, Preprocessor* pp)
{
    assert(!macro_disabled(macro) && "macro is currently expanding!");
    assert(!macro_pragma(macro) && "unhandled _Pragma???");

    // Before we go and push the macro we should check that the macro is not
    // empty. If it is empty we can simply return. This act's like we pushed,
    // expanded nothing and then popped it off.
    if (!macro_builtin(macro) && macro_num_tokens(macro) == 0)
    {
        return;
    }

    // Push to our macro stack with all of the given macro information.
    expander->expansion = macro_expansion_create(&expander->allocator, macro,
            args, location, expander->expansion, pp);

    // And then also remember to disable the macro once we push it.
    macro_disable(macro);
}

void macro_expander_push_arg(MacroExpander* expander, MacroArgs arg,
        Location location)
{
    Token* tokens = macro_arg_tokens(&arg);
    size_t num_tokens = macro_arg_num_tokens(&arg);
    assert(token_is_type(&tokens[num_tokens], TOK_PP_ARGEND) && "need argend!");

    // Push to our macro stack with all the macro expansion argument.
    expander->expansion = macro_expansion_create_arg(&expander->allocator,
            tokens, num_tokens + 1, location, expander->expansion);
}

void macro_expander_pop(MacroExpander* expander)
{
    assert(macro_expansion_finished(expander->expansion) && "unfinished invoc");
    assert(!macro_expansion_arg(expander->expansion) && "popping an arg!!");
    
    // Make sure to get the macro and re-enable it so we can expand it again
    Macro* macro = macro_expansion_macro(expander->expansion);
    macro_enable(macro);

    // Then pop the expansion off of the stack
    expander->expansion = macro_expansion_prev(expander->expansion);
}

void macro_expander_pop_arg(MacroExpander* expander)
{
    assert(macro_expansion_finished(expander->expansion) && "unfinished invoc");
    assert(macro_expansion_arg(expander->expansion) && "popping a macro!!");

    // Simply pop the current expansion off of the stack
    expander->expansion = macro_expansion_prev(expander->expansion);
}

bool macro_expander_next(MacroExpander* expander, Token* token)
{
    assert(macro_expander_expanding(expander) && "not expanding a macro?");

    // If the current macro expansion is finished then pop the current expansion
    // off and try again. Continue to do this until we get to an expansion that
    // is not finished.
    while (macro_expansion_finished(expander->expansion))
    {
        // Pop the expansion.
        macro_expander_pop(expander);
        
        // If we are no longer expanding that means we have popped the final
        // expansion off of the stack. Return false to indicated we didn't get
        // a token so that the caller can know that we are done.
        if (!macro_expander_expanding(expander))
        {
            return false;
        }
    }

    // Otherwise simply consume the token
    *token = macro_expansion_consume(expander->expansion);

    // Then we will always want to pop at this point if we have an argument type
    // expansion. Note that I think it may work without this? But I do not want
    // to risk it.
    if (macro_expansion_arg(expander->expansion))
    {
        if (macro_expansion_finished(expander->expansion))
        {
            macro_expander_pop_arg(expander);
        }
    }

    return true;
}

bool macro_expander_peek(MacroExpander* expander, Token* token)
{
    assert(macro_expander_expanding(expander) && "should be expanding");

    // Similar to the above code, whilst we are at the end of expansions we need
    // to be able peek tokens to see what we get. Note, that we don't pop them
    // off of the stack here, rather we travel down the stack until we find what
    // we're looking for.
    MacroExpansion* expansion = expander->expansion;
    while (macro_expansion_finished(expansion))
    {
        expansion = macro_expansion_prev(expansion);
        
        // NULL means theres no more expansions so no more macro tokens
        if (expansion == NULL)
        {
            return false;
        }        
    }
    
    // Otherwise peek the token on the expansion
    *token = macro_expansion_peek(expansion);
    return true;
}

void preprocessor_push_input(Preprocessor* pp, SourceFile* file)
{
    assert(preprocessor_include_depth(pp) < pp->max_depth 
            && "didn't do a check for an input depth thats too deep!");

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

void preprocessor_initialise_date_time(Preprocessor* pp)
{
    // Get an array containing all of the months
    static const char* const months[12] = { "Jan", "Feb", "Mar", "Apr", "May",
            "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    // Start by attemptin to get the time from the system.
    time_t t = time(NULL);
    struct tm* tm = NULL;
    if (t != (time_t) -1)
    {
        tm = localtime(&t);
    }

    // Print the date into the buffer if we go the struct and then create the
    // date buffer no matter what.
    char date[sizeof("mmm dd yyyy")] = "??? ?? ???";
    if (tm != NULL)
    {
        sprintf(date, "%s %2d %d", months[tm->tm_mon], tm->tm_mday,
                tm->tm_year + 1900);
    }
    Buffer date_buffer = buffer_from_format("\"%s\"\n", date);
    pp->file__DATE__ = source_manager_create_anonomous_buffer(pp->sm,
            date_buffer, LOCATION_INVALID);

    // The do something very similar for the time
    char time[sizeof("hh:mm:ss")] = "??:??:??";
    if (tm != NULL)
    {
        sprintf(date, "%2d:%2d:%2d", tm->tm_hour, tm->tm_min, tm->tm_sec);
    }
    Buffer time_buffer = buffer_from_format("\"%s\"\n", date);
    pp->file__TIME__ = source_manager_create_anonomous_buffer(pp->sm,
            time_buffer, LOCATION_INVALID);
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

static void preprocessor_push_builtins(Preprocessor* pp)
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
    buffer_add_define_one(&predefs, "__x86_64");
    buffer_add_define_one(&predefs, "__x86_64__");

    // Special stuff for ignoring attributes :$
    buffer_add_define_value(&predefs, "__attribute__(x)", "");

    // Now turn this buffer into a source file for us to use.
    SourceFile* source = source_manager_create_builtin_buffer(pp->sm, predefs);
    preprocessor_push_input(pp, source);
}

bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids)
{
    // Before doing anything clear the memory that the preprocessor holds so 
    // that we hopefully crash instead of getting erroneous results.
    *pp = (Preprocessor) {0};

    SourceFile* starting_file = source_manager_create_filepath(sm, main_file,
            LOCATION_INVALID);
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

    pp->hf = header_finder_create();
    
    pp->max_depth = PREPROCESSOR_MAX_DEPTH;
    pp->inputs = include_vector();
    pp->cache = token_list(arena_new_default());

    // Initialise our important identifiers in the preprocessor.
    preprocessor_get_builtin_identifiers(pp);

    // Then add all the builtin macros to the preprocessor and initialise the
    // counter, __DATE__, and __TIME__ macros
    pp->macros = macro_map_create();
    pp->expander = macro_expander_create(sm);
    preprocessor_add_builtin_macros(pp);
    pp->counter = 0;
    preprocessor_initialise_date_time(pp);
    pp->collecting_args = false;
    pp->fatal_error = true;

    // Put our input onto the stack so that we are ready to start lexing tokens
    preprocessor_push_input(pp, starting_file);

    // Now we will also want to create our builtin file and stack it to be on
    // top of this file. Note that I would actually prefer to do this first 
    // then stack the input but oh well.
    preprocessor_push_builtins(pp);
    return true;
}

void preprocessor_delete(Preprocessor* pp)
{
    macro_expander_delete(&pp->expander);
    macro_map_delete(&pp->macros);
    include_vector_free_ptr(&pp->inputs, include_delete);
    token_list_free(&pp->cache); // Free first since some tokens use other arena
    header_finder_delete(&pp->hf);
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

Include* preprocessor_current_input(Preprocessor* pp)
{
    return include_vector_back(preprocessor_inputs(pp));
}

unsigned int preprocessor_include_depth(const Preprocessor* pp)
{
    size_t size = include_vector_size(&pp->inputs);
    
    // In case this is somehow called for the instance of having no current 
    // inputs. Otherwise size-- is sufficient
    if (size > 0)
    {
        size--;
    }

    return size;
}

unsigned int preprocessor_max_include_depth(const Preprocessor* pp)
{
    return pp->max_depth;
}

MacroMap* preprocessor_macro_map(Preprocessor* pp)
{
    return &pp->macros;
}

MacroExpander* preprocessor_expander(Preprocessor* pp)
{
    return &pp->expander;
}

bool preprocessor_collecting_args(const Preprocessor* pp)
{
    return pp->collecting_args;
}

bool preprocessor_fatal_error(const Preprocessor* pp)
{
    return pp->fatal_error;
}

void preprocessor_set_fatal_error(Preprocessor* pp)
{
    pp->fatal_error = true;
}

// TODO: eventually if I want to implement pragma once, or the multiple include
// TODO: optimisatin, then I will have to have more to say then just a boolean
// TODO: of found / not found...
bool preprocessor_try_find_include(Preprocessor* pp, Filepath* path,
        bool angled, Location include_loc, SourceFile** include)
{
    // First get the current include and the source file that is relavent to
    // it. This is so that we can use the current file path as needed from it.
    Include* curr_file = preprocessor_current_input(pp);
    SourceFile* curr_sf = include_get_source(curr_file);
    Filepath* curr_path = source_file_get_name(curr_sf);

    // Don't forget the current search directory is super important!!!
    DirectoryEntry* dir = include_get_search_path(curr_file);

    // Okay now just get the header finder to do all of the work that needs to
    // be done. This is a fairly straight forward call to make.
    return header_finder_try_find_include(&pp->hf, pp->sm, curr_path, dir, path,
            angled, include_loc, include);
}

// FIXME: the token input is unused so do we really need it?
void preprocessor_do_include(Preprocessor* pp, Token* token, SourceFile* sf)
{
    // TODO: I think I could use this function to do multiple include 
    // TODO: optimisation checks if that is ever implemented.

    preprocessor_push_input(pp, sf);
}

void preprocessor_enter_directive(Preprocessor* pp)
{
    Include* current = include_vector_back(&pp->inputs);
    Lexer* lexer = include_get_lexer(current);
    lexer_set_directive(lexer);
}

void preprocessor_allow_headers(Preprocessor* pp)
{
    Include* current = include_vector_back(&pp->inputs);
    Lexer* lexer = include_get_lexer(current);
    lexer_set_header(lexer);
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
bool preprocessor_next_lexer_token(Preprocessor* pp, Token* token)
{
    assert(preprocessor_has_input(pp) && "need a lexer for tokens!");

    Include* current = include_vector_back(&pp->inputs);
    bool ret = include_get_next(current, token);

    // If we get EOF on this file with an input below we should pop this input
    // and then try again. Otherwise we will send and premature eof token that
    // will stop the parser.
    if (token_is_type(token, TOK_EOF))
    {   
        preprocessor_pop_input(pp);
        
        // If we still have input we should not claim that we are at EOF instead
        // tail recurse and attempt to get a token again.
        if(preprocessor_has_input(pp))
        {
            return preprocessor_next_lexer_token(pp, token);
        }
    }

    return ret;
}

bool preprocessor_next_unexpanded_token(Preprocessor* pp, Token* token)
{
    // If we're expanding then try to peek the token off of the expansion stack
    if (macro_expander_expanding(&pp->expander))
    {
        // True means we were able to get a token so just pass this on
        if (macro_expander_next(&pp->expander, token))
        {
            return true;
        }
    }

    // Otherwise we weren't expanding or failed to get a token from the expander
    // So then we should try to get a token from the preprocessor.
    // TODO: Is this correct and what we want to do? What if this pops a file
    // TODO: from the expansion stack and it's the last one?
    // FIXME: GCC reject's this and gives an unterminated macro expansion but
    // FIXME: Clang allows the collection across multiple files !!!
    return preprocessor_next_lexer_token(pp, token);
}

bool preprocessor_peek_raw_token(Preprocessor* pp, Token* token)
{
    // If we're expanding then try to peek the token off of the expansion stack
    if (macro_expander_expanding(&pp->expander))
    {
        // True means we were able to peek a token so just pass this on
        if (macro_expander_peek(&pp->expander, token))
        {
            return true;
        }
    }
    
    // Otherwise we weren't expanding or failed to peek a token from the 
    // expander (all macro's expanding were at the end of their tokens) So try
    // to peek a token from the preprocessor lexer.
    Include* current = include_vector_back(&pp->inputs);
    return include_peek_next(current, token);
}

// Is the current token one which can be macro expanded. Note that this will 
// also check for 
bool preprocessor_should_expand(Preprocessor* pp, Token* token)
{
    if (!token_is_identifier_like(token))
    {
        return false;
    }

    // C99 6.10.3.4 -> some tokens are no longer available for macro expansion
    // in some cases. So this is an important check to do.
    if (token_has_flag(token, TOK_FLAG_NOEXPAND))
    {
        return false;
    }

    // Get the identifier from the token, and subsequentyly any macro definition
    // that is currently around.
    Identifier* name = token_get_identifier(token);
    Macro* macro = macro_map_get_macro(&pp->macros, name);
    if (macro == NULL)
    {
        return false;
    }

    // C99 6.10.3.4 -> some tokens are no longer available for macro expansion
    // in some cases. `The non-replaced macro name preprocessing tokens are no
    // longer available for further replacement...`
    if (macro_disabled(macro))
    {
        // FIXME: should check that this does not break anything in an 
        // FIXME: unexpected ways...
        token_set_flag(token, TOK_FLAG_NOEXPAND);
        return false;
    }

    // Here we have a macro that is enabled. Now all thats left to do is to
    // check if it has a literal '(' following the macro. But we only need to
    // do this in the cases where we have a function like macro
    if (macro_function_like(macro))
    {
        // Peek the token in a fresh token since we don't want to overwrite our
        // current token if we don't want to expand it.
        Token peek_token;
        preprocessor_peek_raw_token(pp, &peek_token);
        if (!token_is_type(&peek_token, TOK_LPAREN))
        {
            return false;
        }
    }

    return true;
}

TokenStream preprocessor_expand_token_from_file(Preprocessor* pp,
        Location location, SourceFile* source, TokenType expected)
{
    assert(source != NULL && "need a source to expand a token from!");

    // Then since we now have a source file we can lex from create a lexer and
    // read all of the tokens from the source (should be a single token here)
    // Note: in theory we should never get a tokens that can be expanded again
    Lexer lexer;
    lexer_create(&lexer, pp->dm, pp->lang, &pp->literal_arena, pp->identifiers,
            source);

    // Next lex the number token and the theoretical EOF token from the lexer
    // we just created.
    Token token;
    lexer_get_next(&lexer, &token);
    assert(token_is_type(&token, expected) && "didn't get the expeced token");

    Token eof;
    lexer_get_next(&lexer, &eof);
    assert(token_is_type(&eof, TOK_EOF) && "got a token that wasn't eof?");

    // Finally, using the preprocessor macro expanding allocator, allocate our
    // replacement list for the token and then create our token stream with it
    Token* replacement = arena_malloc(macro_expander_allocator(&pp->expander),
            sizeof(Token));
    *replacement = token;
    return token_stream_create(replacement, 1);
}

TokenStream preprocessor_expand_unsigned(Preprocessor* pp, Location location,
        unsigned int value)
{
    // Then print the counter value into a buffer and from this buffer create
    // and lex the replacement list that we need.
    Buffer buffer = buffer_from_format("%u\n", value);
    SourceFile* source = source_manager_create_anonomous_buffer(pp->sm,
            buffer, LOCATION_INVALID);
    
    // Finally, go ahed and attempt to expand the token from the file.
    return preprocessor_expand_token_from_file(pp, location, source,
            TOK_NUMBER);
}

TokenStream preprocessor_expand_string(Preprocessor* pp, Location location,
        const char* value)
{
    // Then print the counter value into a buffer and from this buffer create
    // and lex the replacement list that we need.
    // FIXME: do we need to properly quote the value possible???
    Buffer buffer = buffer_from_format("\"%s\"\n", value);
    SourceFile* source = source_manager_create_anonomous_buffer(pp->sm,
            buffer, LOCATION_INVALID);
    
    return preprocessor_expand_token_from_file(pp, location, source,
            TOK_STRING);
}

TokenStream preprocessor_expand_line(Preprocessor* pp, Location location)
{
    // FIXME: add properly handling of resolving of the line number.
    // TODO: this should be implemented in the SourceManager.c file as currently
    // TODO: the diagnostic manager does some janky stuff in order to get the 
    // TODO: current line and column number.
    SourceFile* source = source_manager_from_location(pp->sm, location);
    assert(source != NULL && "got a location but no source?");
    unsigned int line = line_map_resolve_line(&source->line_map, location);
    return preprocessor_expand_unsigned(pp, location, line);    
}

TokenStream preprocessor_expand_file(Preprocessor* pp, Location location)
{
    // Initialise the filepath that we are going to print into.
    // FIXME: come back and implement this
    // TODO: like the line will need to do some work in the implementation of
    // TODO: the source manager so that this will give the correct path name
    // TODO: this in particular is more when we have a file that has line 
    // TODO: directives though
    SourceFile* source = source_manager_from_location(pp->sm, location);
    assert(source != NULL && "got a location but no source?");
    Filepath* path = source_file_get_name(source);
    return preprocessor_expand_string(pp, location, filepath_get_cstr(path));
}

TokenStream preprocessor_expand_date(Preprocessor* pp, Location location)
{
    assert(pp->file__DATE__ != NULL && "should have a date!");
    return preprocessor_expand_token_from_file(pp, location, pp->file__DATE__,
            TOK_STRING);
}

TokenStream preprocessor_expand_time(Preprocessor* pp, Location location)
{
    assert(pp->file__TIME__ != NULL && "should have a time!");
    return preprocessor_expand_token_from_file(pp, location, pp->file__TIME__,
            TOK_STRING);
}

TokenStream preprocessor_expand_include_level(Preprocessor* pp, Location location)
{
    unsigned int level = preprocessor_include_depth(pp);
    return preprocessor_expand_unsigned(pp, location, level);
}

TokenStream preprocessor_expand_counter(Preprocessor* pp, Location location)
{
    // Get the counter and increment it returning the result of expanding the 
    // original value.
    // FIXME: what if the counter overflows?
    unsigned int value = pp->counter++;
    return preprocessor_expand_unsigned(pp, location, value);
}

TokenStream preprocessor_expand_builtin_macro(Preprocessor* pp,
        const Macro* macro, Location location)
{
    Identifier* identifier = macro_name(macro);
    if (identifier == pp->id___LINE__)
    {
        return preprocessor_expand_line(pp, location);
    }
    else if (identifier == pp->id___FILE__)
    {
        return preprocessor_expand_file(pp, location);
    }
    else if (identifier == pp->id___DATE__)
    {
        return preprocessor_expand_date(pp, location);
    }
    else if (identifier == pp->id___TIME__)
    {
        return preprocessor_expand_time(pp, location);
    }
    else if (identifier == pp->id___INCLUDE_LEVEL__)
    {
        return preprocessor_expand_include_level(pp, location);
    }
    else if (identifier == pp->id___COUNTER__)
    {
        return preprocessor_expand_counter(pp, location);
    }
    assert(identifier != pp->id__Pragma && "pragma should've been handled");

    panic("unreachable; unhandled builtin macro type");
    return token_stream_create_empty();
}

void preprocessor_push_macro_arg(Preprocessor* pp, MacroArgs arg,
        Location location)
{
    macro_expander_push_arg(&pp->expander, arg, location);
}

// Returns true if we expanded to tokens and false otherwise. The only reason
// that this even returns anything is so that we can more easily handle the 
// token pasting operator.
bool preprocessor_expand_macro_arg(Preprocessor* pp, TokenList* result,
        size_t* count, MacroArgs arg, Location location)
{
    // This should be true since we first pull from the cache in 
    // preprocessor_get_next which should not be done so this is checked here.
    assert(token_list_empty(&pp->cache) && "non-empty cache???");

    // Track the initial count so that can know if we expanded to empty
    size_t initial_count = *count;

    // Push this macro argument as is to the expansion stack so that we can then
    // go and expand it nice and quickly.
    preprocessor_push_macro_arg(pp, arg, location);

    // Then keep grabbing tokens from the stream until we get to the argend 
    // token. This make expanding the arguments nice and simple from our 
    // perspective.
    Token tmp = {0};
    while (true)
    {
        preprocessor_get_next(pp, &tmp);
        assert(!token_is_type(&tmp, TOK_EOF) && "EOF expanding macro arg!");

        // Once we reach the end of the argument stop attempting to get more
        // tokens. If we do this we will be affecting the 
        if (token_is_type(&tmp, TOK_PP_ARGEND))
        {
            break;
        }

        // Add the token to the list and increase the current count of tokens
        token_list_push_back(result, tmp);
        (*count)++;
    }

    // If they are not equal that means we got tokens when we expanded :)
    return initial_count != *count;
}

Token preprocessor_stringify_macro_arg(Preprocessor* pp, MacroArgs arg,
        Location expansion_loc, Location param_loc)
{
    Token* tokens = macro_arg_tokens(&arg);
    size_t num_tokens = macro_arg_num_tokens(&arg);

    // Create a buffer and add an initial " character to it. Then for each of 
    // the tokens in the argument we should get their spelling and add it to the
    // buffer. Finally, add a trailing " character.
    Buffer arg_buff = buffer_new();
    buffer_add_char(&arg_buff, '"');
    // TODO: make a call to a function for the lexer to stringify the tokens
    // TODO: Note that we cannot make assumptions about where the macr arg 
    // TODO: tokens can come from so we may need alot of inputs into the 
    // TODO: function
    buffer_add_char(&arg_buff, '"');
    buffer_make_cstr(&arg_buff);

    // Then, we  will need to turn the buffer into a sourcefile so go do this.
    // One we have that, create a lexer (without diagnostics).
    SourceFile* string = source_manager_create_anonomous_buffer(pp->sm,
            arg_buff, param_loc);
    Lexer tmp_lex;
    lexer_create(&tmp_lex, /*dm*/NULL, pp->lang, &pp->literal_arena,
            pp->identifiers, string);

    // Lex the single string token from it.
    Token string_tok;
    lexer_get_next(&tmp_lex, &string_tok);
    assert(token_is_type(&string_tok, TOK_STRING) && "expected string token");

    Token eof_tok;
    lexer_get_next(&tmp_lex, &eof_tok);
    assert(token_is_type(&eof_tok, TOK_EOF) && "didn't get the EOF token");

    // FIXME: fix the tokens location when we do this and have that facility
    // FIXME: available.
    return string_tok;
}

// The algorithm for replacing the arguemtns and expanding function like macros
// can be a little tricky. The basic steps are follows. Note that we have 
// already collected all of our unexpanded macro arguments and checked that the
// macro has a replacement list which has at least one token in it.
TokenStream preprocessor_expand_function_macro(Preprocessor* pp,
        const Macro* macro, const MacroArgs* args, Location location)
{
    assert(macro_function_like(macro) && "not function macro?");

    // C99 6.10.3.1 Argument substution
    // After the arguments for the invocation of a function-like macro have been
    // identified, argument substitution takes place. A parameter in the 
    // replacement list, unless preceded by a # or ## preprocessing token or 
    // followed by a ## preprocessing token (see below), is replaced by the 
    // corresponding argument after all macros contained therein have been
    // expanded. Before being substituted, each argument’s preprocessing tokens
    // are completely macro replaced as if they formed the rest of the 
    // preprocessing file; no other preprocessing tokens are available

    // First do a check for a macro which doesn't use it's arguments for 
    // anything. In this case just return the stream. This avoid allocating
    // assitional memory that we might need.
    if (!macro_function_like_uses_args(macro))
    {
        return macro_get_stream(macro);
    }

    // Get the initial replacement list of the macro and scan through it until
    // FIXME: it would be good if TokenList could just track it's count since
    // FIXME: there are alot of situations list this that probably should be
    // FIXME: fixed to reduce errors and make code cleaner.
    TokenList result = token_list(arena_new_default());
    size_t count = 0;

    TokenStream replacement = macro_get_stream(macro);
    while (!token_stream_end(&replacement))
    {
        // FIXME: stringification and concatenation handling needed.
        // TODO: for stringification we need to do that first. And then 
        // TODO: concatenation sometimes needs to be removed if the LHS or the
        // TODO: RHS expands to and empty thing. But concatenation is handled
        // TODO: at a later stage. But we still need to potentially skip 
        // TODO: expanding an argument if we find them
        
        Token tok = token_stream_consume(&replacement);

        // If this token is a hash then it is followed by a macro parameter.
        // So let's go ahead and stringify this.
        if (token_is_type(&tok, TOK_HASH))
        {
            // Consume the token after it and get the param number and the 
            // subsequent MacroArg structure.
            Token param = token_stream_consume(&replacement);
            Identifier* param_name = token_get_identifier(&param);
            size_t param_num = macro_param_num(macro, param_name);
            MacroArgs arg = args[param_num];

            // Then go ahead and stringify the tokens.
            Token stringified = preprocessor_stringify_macro_arg(pp, arg,
                    location, token_get_location(&param));

            // Finally, simply  just push the token to the result list and
            // increase the results count.
            token_list_push_back(&result, stringified);
            count++;
            continue;
        }

        //If it is not a parameter name then simply add it to the replacement 
        // list for the later rescanning. 
        Identifier* name = token_get_identifier(&tok);
        size_t param;
        if (name == NULL || !macro_get_param_num(macro, name, &param))
        {
            token_list_push_back(&result, tok);
            count++;
            continue;
        }

        // FIXME: here is where we should care about if we have a '##' after, or
        // FIXME: before the macro argument list, so we should have this here.
        // NOTE: for this we will also really need to care about if the macro
        // NOTE: argument is empty or not since we will effectively have to
        // NOTE: delete the '##' token if the LHS or the RHS is empty...

        // Otherwise we need to macro expand the macros replacement list. Also
        // keep track of if we got tokens or not.
        bool non_empty= preprocessor_expand_macro_arg(pp, &result, &count,
                args[param], location);
    }

    // Now that we have the final replacement list for the macro (which is due
    // for rescanning) we can flatten our linked list into tokens.
    Token* tokens = token_list_flatten(macro_expander_allocator(&pp->expander),
            &result, count);
    
    // Free the replacement list.
    token_list_free(&result);

    // Finally, we can return the stream that we got.
    return token_stream_create(tokens, count);
}

void preprocessor_push_macro_expansion(Preprocessor* pp, Token* token,
        Macro* macro, MacroArgs* args)
{
    macro_expander_push(&pp->expander, macro, args, token_get_location(token),
            pp);
}

// Collect a token list into a series of macro arguments that we can used for
// the purposes of our macro expansion. This destroys the tmp_tokens list by
// removing all of the tokens out of it, but it does not free the memory 
// asociated with it and that still need to be done by the caller.
MacroArgs* preprocessor_collect_macro_arguments(Preprocessor* pp,
        TokenList* tmp_tokens, size_t token_count, size_t num_expected,
        bool variadic)
{
    // FIXME: if token_count is 0 this allocated alot of memory in the list's
    // FIXME: arena all for only one token.
    // If we didn't expect any arguemtns then just exit, we don't have anything
    // to really do here.
    if (num_expected == 0/*|| (num_expected == 1 && token_count == 0)*/)
    {
        assert(token_count == 0 && "expected no arguments but got tokens?");
        return NULL;
    }

    // Before we finish collecting macro arguments we will need to add the 
    // argend token onto the end of the token list. So create that fake token
    // and push it onto the end of the list
    Token end = {0};
    token_set_type(&end, TOK_PP_ARGEND);
    token_list_push_back(tmp_tokens, end);

    // Get the macro expanders allocator and make it responsible for storing
    // the tokens. And then allocate the arguments we are going to use and also 
    // flatten the token list so we can soon go and modify it and we will create
    // our macro arguments.
    Arena* allocator = macro_expander_allocator(&pp->expander);
    MacroArgs* args = arena_malloc(allocator, sizeof(MacroArgs) * num_expected);
    // Plus one for ARGEND so that we can have a terminator for the last arg.
    Token* tokens = token_list_flatten(allocator, tmp_tokens, token_count + 1);

    // For each of our arguments find the non-nested comma and turn it into a
    // special token which represents the end of an argument so that we can
    // check for this token when expandinf the arguments.
    size_t current_offset = 0;
    for (size_t arg = 0; arg < num_expected; arg++)
    {
        assert(current_offset <= token_count && "ran off the end!");

        // Whilst we are collecting the arguments for these tokens keep track
        // of the number of parentesis and make sure we don't run off the end
        // of the argument tokens.
        size_t start_offset = current_offset;
        size_t arg_tokens = 0;
        size_t paren_count = 0;
        while (current_offset != token_count)
        {
            // If we are variadic and collecting for the last argument ignore
            // any of the commas that we get and just go until the offset is
            // the token count.
            // Otherwise, check if we get an unnested comma. if this is the case
            // we are done collecting tokens for this argument.
            if (!(variadic && (arg == num_expected - 1))
                    && token_is_type(&tokens[current_offset], TOK_COMMA)
                    && paren_count == 0)
            {
                break;
            }

            // If we have an lparen or an rparen make sure to handle this 
            // properly and track the count of them.
            if (token_is_type(&tokens[current_offset], TOK_LPAREN))
            {
                paren_count++;
            }
            else if (token_is_type(&tokens[current_offset], TOK_RPAREN))
            {
                assert(paren_count != 0 && "no lparens but an rparen?");
                paren_count--;
            }

            // 'eat' this token
            arg_tokens++;
            current_offset++;
        }

        // If we're at the end we shouldn't have a comma so don't attempt to 
        // skip it at all
        if (current_offset != token_count)
        {
            assert(token_is_type(&tokens[current_offset], TOK_COMMA)
                    && "should be a comma but we don't have it?");
                
            // Set the token type to be ARGEND so that we have some kind of 
            // sentinal and know when the argument ends later.
            token_set_type(&tokens[current_offset], TOK_PP_ARGEND);
            current_offset++;
        }

        args[arg] = macro_args_create(tokens + start_offset, arg_tokens);
    }
    assert(current_offset == token_count && "didn't go through all tokens?");

    return args;
}

// Returns false if we failed to collect the macro arguments, otherwise true.
bool preprocessor_get_macro_arguments(Preprocessor* pp, Token* macro_tok,
        Macro* macro, MacroArgs** args)
{
    Token tmp_tok;
    preprocessor_next_unexpanded_token(pp, &tmp_tok);
    assert(token_is_type(&tmp_tok, TOK_LPAREN) && "should have got an '('???");

    // Get the number of params and if the macro is variadic from the macro
    // definition.
    size_t num_expected = macro_num_params(macro);
    bool variadic = macro_variadic(macro);

    // Create a token list to store all of our tokens
    TokenList tmp_tokens = token_list(arena_new_default());
    size_t token_count = 0;

    // Keep track of the number of nested parenthesis and the count of the 
    // number of commas
    size_t nested_paren = 0;

    // Keep track of the number of remaining arguments to collect tokens for
    size_t num_found = 0;

    Location too_many_args = LOCATION_INVALID;
    bool fatal_error = false;
    while (true)
    {
        preprocessor_next_unexpanded_token(pp, &tmp_tok);

        if (token_is_type(&tmp_tok, TOK_EOF)
                || token_is_type(&tmp_tok, TOK_PP_EOD))
        {
            diagnostic_error_at(pp->dm, token_get_location(&tmp_tok),
                    "unterminated function-like macro invocation");
            // Overwrite the macro token with the EOF token so that we don't
            // lose it. We have an error anyways and definitely want to recover
            // from it without crashing.
            *macro_tok = tmp_tok;
            fatal_error = true;
            break;
        }

        if (token_is_type(&tmp_tok, TOK_LPAREN))
        {
            nested_paren++;
        }
        else if (token_is_type(&tmp_tok, TOK_RPAREN))
        {
            // If we got a right paren with no nested parenthesis we are done
            // and should go check if it aligns with what we expect. Note that
            // I don't think we need to eat it once we are done with the macro
            // are collecting. Since this is our stream 
            // A(...args)
            //          ^
            // If EOF comes after this and we lex it, it will pop all our inputs
            // off of the stack which will then lead to an assertion failure
            // afterwards.
            if (nested_paren == 0)
            {
                break;
            }

            nested_paren--;
        }
        else if (token_is_type(&tmp_tok, TOK_COMMA))
        {
            // If we aren't within any nested parenthesis then increase the 
            // number of arguments found and 
            if (nested_paren == 0)
            {
                num_found++;

                // Now check if we recieved to many macro arguments and act
                // accordingly.
                if (num_found == num_expected && !variadic
                        && too_many_args == LOCATION_INVALID)
                {
                    too_many_args = token_get_location(&tmp_tok);
                    diagnostic_error_at(pp->dm, too_many_args, 
                            "too many arguments provided to function like "
                            "macro invocation");
                    fatal_error = true; // Too many args is fatal
                }
            }
        }

        // Simply add the token to the end of the list so we can collect them 
        // all effectively.
        token_list_push_back(&tmp_tokens, tmp_tok);
        token_count++;
    }

    // Here we need to complete our list of arguments increasing the number of
    // found arguments by 1 if our token list isn't empty i.e.
    // FOO() -> don't increase, 0 arguments
    // FOO(x) -> increase by 1
    // FOO(a,b) -> increase by 1
    if (token_count != 0)
    {
        // Since we increased the cound we need to check again that we recieved
        // the correct number of arguments
        num_found++;
        if (num_found > num_expected && !variadic
                && too_many_args == LOCATION_INVALID)
        {
            Token front = token_list_peek_front(&tmp_tokens);
            diagnostic_error_at(pp->dm, token_get_location(&front),  "too many "
                    "arguments provided to function like macro invocation");
            fatal_error = true; // Too many args is fatal
        }
    }

    // Down here, we should check that we didn't receive too little macro 
    // arguments since that is another possible source of error.
    if (!fatal_error && num_found == 0 && num_expected == 1)
    {
        // Macro argument 'exists' but is simply just empty e.g. #define FOO(x)
        // FOO() -> valid (also all good if `x` is `...`)
    }
    else if (!fatal_error && num_found < num_expected)
    {
        diagnostic_error_at(pp->dm, token_get_location(&tmp_tok), "too few "
                "arguments provided to function like macro invocation");
        fatal_error = true; // Too few args is fatal
    }

    // Now if we didn't have a fatal error then let's go and collect the macro
    // arguments into nice structures for use to use then.
    if (!fatal_error)
    {
        *args = preprocessor_collect_macro_arguments(pp, &tmp_tokens,
                token_count, num_expected, variadic);
        assert(token_list_empty(&tmp_tokens) && "didn't collect all tokens?");
    }

    // Finally, free the token list and get rid of it's memory
    token_list_free(&tmp_tokens);

    // If we got a fatal error return false, otherwise return true
    return !fatal_error;
}

bool preprocessor_handle_pragma_operator(Preprocessor* pp, Token* token)
{
    assert(token_get_identifier(token) == pp->id__Pragma && "not _Pragma?");
    panic("_Pragma is currently unimplemented");
    return true;
}

bool preprocessor_start_expansion(Preprocessor* pp, Token* token)
{
    Identifier* name = token_get_identifier(token);
    Macro* macro = macro_map_get_macro(&pp->macros, name);
    assert(macro != NULL && !macro_disabled(macro) && "need an alive macro!");
    
    // If we have a pragma builtin then we will need to handle that seperately
    if (macro_pragma(macro))
    {
        preprocessor_handle_pragma_operator(pp, token);
        return true;
    }

    // If we have a function like expansion then let's now go and collect the
    // arguments for it. Note that args may still be NULL after in some 
    // particular situations so is not a good way to detect if that failed or
    // not.
    MacroArgs* args = NULL;
    if (macro_function_like(macro))
    {
        if (!preprocessor_get_macro_arguments(pp, token, macro, &args))
        {
            return false;
        }
    }

    preprocessor_push_macro_expansion(pp, token, macro, args);
    return true;
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
        if (preprocessor_start_expansion(pp, token))
        {
            return preprocessor_expand_next(pp, token);
        }
    }

    return true;
}

// This functions is the primary loop of the preprocessor. This handles all of
// our typical preprocessor things like lexing, macro expansion, and handling
// of any preprocessing directives that we could encounter. This loop aims to
// be as simple as possible so that we can really go into each individual 
// component of the preprocessor as it's own entity (as much as possible);
bool preprocessor_get_next(Preprocessor* pp, Token* token)
{
    // If we have tokens in the cache we should pull those off. This mechanism
    // is primarily used for getting the peek token.
    if (!token_list_empty(&pp->cache))
    {
        *token = token_list_pop_front(&pp->cache);
        return true;
    }

    // If we don't have any input simply return false to say we got nothing.
    // This is mainly used as a fail safe in the event that all of our inputs
    // are popped from the stack and we discard the EOF token acidentally.
    if (!preprocessor_has_input(pp))
    {
        token_set_type(token, TOK_EOF);
        return false;
    }

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
    bool ret = preprocessor_next_lexer_token(pp, token);

    // Check if we have got a preprocessing directive here. Preprocessing
    // directives can only occur when there is a hash as the first token
    // on that line. If we have one, parse it and then try again to get the
    // next token. Even if we are caching our token then we will need to
    // handle this directive to get to our next token. So handle it.
    // FIXME: Is this okay for long chains at the start of a file when this
    // FIXME: isn't getting deleted by tail recursion?
    if (preprocessor_directive_start(pp, token))
    {
        preprocessor_parse_directive(pp, token);

        // If we got an EOF token after parsing a directive we should abort 
        // lexing as that means we got an include like directive that fully
        // failed to include anything at all.
        if (token_is_type(token, TOK_EOF))
        {
            assert(preprocessor_fatal_error(pp) && "EOF but not fatal?");
            return false;
        }
        
        return preprocessor_get_next(pp, token);
    }

    // If we get a token that should be expanded, then go and start the 
    // preprocessor expansion.
    if (preprocessor_should_expand(pp, token))
    {
        if (preprocessor_start_expansion(pp, token))
        {
            return preprocessor_get_next(pp, token);
        }
    }

    return ret;
}

bool preprocessor_advance_token(Preprocessor* pp, Token* token)
{
    return preprocessor_get_next(pp, token);
}

bool preprocessor_peek_token(Preprocessor* pp, Token* token)
{
    // Otherwise get and cache the next token for us.
    bool ret = preprocessor_get_next(pp, token);

    // Make sure to add the token to the start of the list so that we don't lose
    // our peek token as there is not way to go back currently.
    token_list_push_back(&pp->cache, *token);

    return ret;
}

void preprocessor_insert_token(Preprocessor* pp, Token token)
{
    preprocessor_insert_tokens(pp, &token, 1);
}

void preprocessor_insert_tokens(Preprocessor* pp, Token* tokens,
        size_t num_tokens)
{
    size_t remaining = num_tokens;
    while (remaining != 0)
    {
        // We must push the front as it is possible there could be tokens 
        // current in the preprocessor's cache which would mean we would pick
        // those off first each time we advance.
        token_list_push_front(&pp->cache, tokens[remaining - 1]);
        remaining--;
    }
}
