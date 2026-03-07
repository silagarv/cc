#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>
#include <threads.h>

#include "driver/warning.h"
#include "files/line_map.h"
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
    buffer_add_define_one(&predefs, "__x86_64");
    buffer_add_define_one(&predefs, "__x86_64__");

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
    pp->counter = 0;

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

    // Get the identifier from the token, and subsequentyly any macro definition
    // that is currently around.
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

// TODO: here put functions for pre-expanding macros
void preprocessor_expand_line(Preprocessor* pp, Location location)
{
    
}

void preprocessor_expand_file(Preprocessor* pp, Location location)
{
    
}

void preprocessor_expand_date(Preprocessor* pp)
{
    
}

void preprocessor_expand_time(Preprocessor* pp)
{
    
}

void preprocessor_expand_include_level(Preprocessor* pp, Location location)
{
    unsigned int lever = preprocessor_include_depth(pp);
}

TokenStream preprocessor_expand_counter(Preprocessor* pp, Location location)
{
    // First start by getting the counter and incrementing it.
    unsigned int counter = pp->counter++;
    assert(counter != 0 && "counter overflowed?");

    // Then print the counter value into a buffer and from this buffer create
    // and lex the replacement list that we need.
    Buffer counter_buffer = buffer_from_format("%u\n", counter);
    SourceFile* source = source_manager_create_anonomous_buffer(pp->sm,
            counter_buffer, LOCATION_INVALID);
    
    // Then since we now have a source file we can lex from create a lexer and
    // read all of the tokens from the source (should be a single token here)
    // Note: in theory we should never get a tokens that can be expanded again
    Lexer lexer;
    lexer_create(&lexer, pp->dm, pp->lang, &pp->literal_arena, pp->identifiers,
            source);

    // Next lex the number token and the theoretical EOF token from the lexer
    // we just created.
    Token number;
    lexer_get_next(&lexer, &number);
    assert(token_is_type(&number, TOK_NUMBER) && "not a number token?");

    Token eof;
    lexer_get_next(&lexer, &eof);
    assert(token_is_type(&number, TOK_EOF) && "got a token that wasn't eof?");

    // Finally, using the preprocessor macro expanding allocator, allocate our
    // replacement list for the token and then create our token stream with it
    Token* replacement = arena_malloc(macro_expander_allocator(&pp->expander),
            sizeof(Token));
    *replacement = number;
    return token_stream_create(replacement, 1);
}

void preprocessor_expand_pragma(Preprocessor* pp)
{
    
}

void preprocessor_push_macro_expansion(Preprocessor* pp, Token* token,
        Macro* macro, MacroArgs* args)
{
    macro_expander_push(&pp->expander, macro, args, token_get_location(token));
}

// Collect a token list into a series of macro arguments that we can used for
// the purposes of our macro expansion. This destroys the tmp_tokens list by
// removing all of the tokens out of it, but it does not free the memory 
// asociated with it and that still need to be done by the caller.
MacroArgs* preprocessor_collect_macro_arguments(Preprocessor* pp,
        TokenList* tmp_tokens, size_t token_count, size_t num_expected,
        bool variadic)
{
    // If we didn't expect any arguemtns then just exit, we don't have anything
    // to really do here.
    if (num_expected == 0 || (num_expected == 1 && token_count == 0))
    {
        assert(token_count == 0 && "expected no arguments but got tokens?");
        return NULL;
    }

    // Get the macro expanders allocator and make it responsible for storing
    // the tokens. And then allocate the arguments we are going to use and also 
    // flatten the token list so we can soon go and modify it and we will create
    // our macro arguments.
    Arena* allocator = macro_expander_allocator(&pp->expander);
    MacroArgs* args = arena_malloc(allocator, sizeof(MacroArgs) * num_expected);
    Token* tokens = token_list_flatten(allocator, tmp_tokens, token_count);

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
            current_offset++;
        }

        // TODO: should we modify the token stream to remove any of the excess 
        // TODO: commas so that we have some kind of sentinal to make sure we 
        // TODO: don't expand the comma ever by accident?
        args[arg] = macro_args_create(tokens + start_offset, arg_tokens);
    }
    assert(current_offset == token_count && "didn't go through all tokens?");

    return args;
}

MacroArgs* preprocessor_get_macro_arguments(Preprocessor* pp, Token* macro_tok,
        Macro* macro)
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

        if (token_is_type(&tmp_tok, TOK_EOF))
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
    MacroArgs* args = NULL;
    if (!fatal_error)
    {
        args = preprocessor_collect_macro_arguments(pp, &tmp_tokens,
                token_count, num_expected, variadic);
        assert(token_list_empty(&tmp_tokens) && "didn't collect all tokens?");
    }

    // Finally, free the token list and get rid of it's memory
    token_list_free(&tmp_tokens);

    // Then we can finally return our arguments
    return args;
}

bool preprocessor_start_expansion(Preprocessor* pp, Token* token)
{
    Identifier* name = token_get_identifier(token);
    Macro* macro = macro_map_get_macro(&pp->macros, name);
    assert(macro != NULL && !macro_disabled(macro) && "need an alive macro!");
   
    // If we have a function like expansion then let's now go and collect the
    // arguments for it.
    void* args = NULL;
    if (macro_function_like(macro))
    {
        args = preprocessor_get_macro_arguments(pp, token, macro);
        
        // Argument collection failed to produce anything
        if (args == NULL)
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
    if (preprocessor_directive_start(pp, token))
    {
        preprocessor_parse_directive(pp, token);
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

