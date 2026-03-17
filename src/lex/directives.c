#include "directives.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "util/arena.h"
#include "util/buffer.h"
#include "util/ptr_set.h"

#include "driver/warning.h"
#include "driver/diagnostic.h"

#include "files/filepath.h"
#include "files/source_manager.h"
#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/macro.h"
#include "lex/macro_map.h"
#include "lex/token.h"
#include "lex/preprocessor.h"
#include "lex/include_stack.h"
#include "lex/lexer.h"
#include "lex/pp_expression.h"

bool preprocessor_directive_start(Preprocessor* pp, Token* token)
{
    return token_is_directive_start(token);
}

void preprocessor_eat_to_eod(Preprocessor* pp, Token* token)
{
    while (!token_is_type(token, TOK_PP_EOD))
    {
        preprocessor_next_lexer_token(pp, token);
        assert(!token_is_type(token, TOK_EOF) && "EOF in directive");
    }
}

void preprocessor_expect_directive_end(Preprocessor* pp, const char* context)
{
    Token token;
    preprocessor_next_lexer_token(pp, &token);

    // If the next token was the end of directive we are ok
    if (token_is_type(&token, TOK_PP_EOD))
    {
        return;
    }

    // Otherwise warn about the extra tokens that we got.
    diagnostic_warning_at(pp->dm, token_get_location(&token), Wextra_tokens,
            "extra tokens at end of #%s directive", context);
    preprocessor_eat_to_eod(pp, &token);
}

// Nice littler helper function to get the name of one of our macro parameters.
// This returns true if it was successful and false otherwise. For some reason
// GCC, and Clang both don't error if we get e.g. '#ifdef defined' ??? 
bool preprocessor_get_macro_name(Preprocessor* pp, Token* token, bool defundef,
        Identifier** name, Location* loc)
{
    preprocessor_next_lexer_token(pp, token);

    // If we got the end of the directive we should error about a missing macro
    // name;
    if (token_is_type(token, TOK_PP_EOD))
    {
        diagnostic_error_at(pp->dm, token_get_location(token), "macro name "
                "missing");
        return false;
    }

    // If we don't have an identifier token then error and consume the rest
    if (!token_is_identifier_like(token))
    {
        diagnostic_error_at(pp->dm, token_get_location(token), "macro name "
                "must be an identifier");
        preprocessor_eat_to_eod(pp, token);
        return false;
    }

    // Get the name and location from the token.
    *name = token_get_identifier(token);
    *loc = token_get_location(token);

    // Only in the defien and undefine context do we check the macro name?
    if (defundef && identifier_get_pp_keyword(*name) == TOK_PP_defined)
    {
        diagnostic_error_at(pp->dm, *loc, "'defined' cannot be used as a macro "
                "name");
        preprocessor_eat_to_eod(pp, token);
        return false;
    }

    // We don't have any issues with the macro name.
    return true;
}

bool preprocessor_macro_has_params(Preprocessor* pp, Token* token)
{
    if (!token_is_type(token, TOK_LPAREN))
    {
        return false;
    }

    if (token_has_flag(token, TOK_FLAG_WHITESPACE))
    {
        return false;
    }

    return true;
}

void preprocessor_add_macro_params(Preprocessor* pp, Macro* macro,
        TokenList* names, size_t num_names, bool variadic)
{
    // Handle the case of having an empty token list.
    if (num_names == 0 && !variadic)
    {
        macro_set_params(macro, NULL, 0, false);
        return;
    }

    size_t num_alloced = num_names;
    if (variadic)
    {
        num_alloced++;
    }

    Arena* allocator = macro_map_allocator(preprocessor_macro_map(pp));
    Identifier** params = arena_malloc(allocator,
            sizeof(Identifier*) * num_alloced);
    for (size_t i = 0; i < num_names; i++)
    {
        Token token = token_list_pop_front(names);
        params[i] = token_get_identifier(&token);
    }
    assert(token_list_empty(names));

    // Handle the variadic case and give it the variadic parameter name
    if (variadic)
    {
        params[num_alloced - 1] = pp->id___VA_ARGS__;
    }

    // Finally set the parameters in the macro. Note we use `num_alloced` here
    // instead of the oficial count so that we can find the parameter named
    // __VA_ARGS__ if we need to stringize it for example.
    macro_set_params(macro, params, num_alloced, variadic);    
}

bool preprocessor_parse_macro_params(Preprocessor* pp, Token* token,
        Macro* macro)
{
    assert(preprocessor_macro_has_params(pp, token) && "should have params");

    // Now we get the next token from the PP. If it is a ')' then we simply 
    // optimize that case to not have to create our set of identifiers to make
    // it slightly quicker.
    preprocessor_next_lexer_token(pp, token);

    if (token_is_type(token, TOK_RPAREN))
    {
        // Make sure to set the params so we know we got a function like macro
        preprocessor_add_macro_params(pp, macro, NULL, 0, false);

        // Eat the ')' so it doesn't make it into the list.
        preprocessor_next_lexer_token(pp, token);
        return true;
    }

    // Here we are wanting to parse a macro parameter list. So let's create a 
    // set of all of our identifiers so that we can quickly know if we got a 
    // duplicate macro parameter. Then we will create a list of tokens to 
    // temporarily store our identifiers. Finally, keep track of if there were
    // any errors in parsing our parameters.
    PtrSet set = pointer_set_create();
    TokenList names = token_list(arena_new_default());
    size_t num_names = 0;
    bool variadic = false;

    bool okay = true;
    
    // To start note that we should have already consumed the '(' and we should
    // have also not gotten here if we got a ')' already. So the first token
    // should always be an identifier or dot's if this is valid.
    while (true)
    {
        assert(!token_is_type(token, TOK_EOF) && "EOF in parameter list");
        assert(!(token_is_type(token, TOK_RPAREN) && num_names == 0) 
                && "empty macro parameter list not handled?");
        assert(okay && "had error but parsing params?");

        // Handle eod in middle of parameter list e.g. #define FOO(
        if (token_is_type(token, TOK_PP_EOD))
        {
            diagnostic_error_at(pp->dm, token_get_location(token), "missing "
                    "')' in macro parameter list");
            okay = false;
            break;
        }

        // If we get an elipsis expected the end of the macro parameters
        if (token_is_type(token, TOK_ELIPSIS))
        {
            // The one thing we have to be careful here is to check if the 
            // __VA_ARGS__ identifier is in the set of macro names. This is not
            // really well defined so we will hard error if this occurs. This is
            // what GCC does and seems sensible enough. (apart from crashing
            // afterwards)
            // FIXME: clang allows this but internally it seems dubious to me...
            if (pointer_set_contains(&set, pp->id___VA_ARGS__))
            {
                diagnostic_error_at(pp->dm, token_get_location(token),
                        "duplicate macro parameter name '__VA_ARGS__'");
                okay = false;
                break;                
            }

            variadic = true;

            // skip part the '...'
            preprocessor_next_lexer_token(pp, token);

            // Now we like to eat the closing paren but we should also check
            // that we actually have it
            if (!token_is_type(token, TOK_RPAREN))
            {
                diagnostic_error_at(pp->dm, token_get_location(token),
                        "missing ')' in macro parameter list");
                okay = false;
            }
            else
            {
                // Eat the ')'
                assert(token_is_type(token, TOK_RPAREN));
                preprocessor_next_lexer_token(pp, token);
            }
            break;
        }

        // No we are wanting to make sure we actually have a macro parameter.
        if (!token_is_identifier_like(token))
        {
            diagnostic_error_at(pp->dm, token_get_location(token), "invalid "
                    "token in macro parameter list");
            okay = false;
            break;
        }

        // Okay, we know we have an identifier like token. So see if we got that
        // identifier before and then if everything is okay, push the token onto
        // our token list.
        Identifier* ident = token_get_identifier(token);
        if (pointer_set_contains(&set, ident))
        {
            diagnostic_error_at(pp->dm, token_get_location(token), "duplicate "
                    "macro parameter name '%s'", identifier_cstr(ident));
            okay = false;
            break;            
        }

        pointer_set_insert(&set, ident);
        token_list_push_back(&names, *token);
        num_names++;

        // Now the we have a parameter name in our identifier list the following
        // tokens should either be a ',' or a ')'. If we do not have either of
        // these error. Note that there are some GCC and Clang extensions that
        // make this different but we will not implement those.
        preprocessor_next_lexer_token(pp, token);

        if (token_is_type(token, TOK_RPAREN))
        {
            // Eat the ')' we are done.
            preprocessor_next_lexer_token(pp, token);
            break;
        }

        if (token_is_type(token, TOK_COMMA))
        {
            // Eat the ',' and then try to parse the next parameter.
            preprocessor_next_lexer_token(pp, token);
            continue;
        }

        // Error case
        diagnostic_error_at(pp->dm, token_get_location(token), "invalid token "
                "in macro parameter list");
        okay = false;
        break;
    }

    // If we successfully parsed the list then add the parameters to the macro.
    if (okay)
    {
        preprocessor_add_macro_params(pp, macro, &names, num_names, variadic);
    }

    // Free the memory for our token list and the pointer set.
    token_list_free(&names);
    pointer_set_delete(&set);

    return okay;
}

void preprocessor_add_macro_body(Preprocessor* pp, Macro* macro,
        TokenList* tmp_toks, size_t num_toks)
{
    Arena* allocator = macro_map_allocator(preprocessor_macro_map(pp));
    Token* tokens = token_list_flatten(allocator, tmp_toks, num_toks);
    macro_set_tokens(macro, tokens, num_toks);
}

bool preprocessor_parse_macro_body(Preprocessor* pp, Token* token, Macro* macro)
{
    // If we get the end of directive token then the macro has an empty body. In
    // this case there is nothing to do.
    if (token_is_type(token, TOK_PP_EOD))
    {
        return true;
    }

    // Now we want to parse the macro body. There are some restrictions that we
    // must follow though. If we are a function like macro we can only stringise
    // macro parameters. And the ## must not appear at the start or end of a 
    // macro body.
    TokenList tmp_tokens = token_list(arena_new_default());
    size_t num_tokens = 0;
    bool function_like = macro_function_like(macro);

    bool okay = true;
    while (!token_is_type(token, TOK_PP_EOD))
    {
        assert(!token_is_type(token, TOK_EOF) && "EOF in macro body??");
        assert(okay && "not okay but still parsing macro body??");

        token_list_push_back(&tmp_tokens, *token);
        num_tokens++;

        // First handle the stringize case but only when we the possibility of
        // having macro parameters.
        if (function_like && token_is_type(token, TOK_HASH))
        {
            // Eat the '#' so we can check if we got a macro parameter.
            preprocessor_next_lexer_token(pp, token);

            // Get the identifier from the token and see if we either got a non
            // identifier type token or the macro doesn't have it as a parameter
            Identifier* name = token_get_identifier(token);
            if (name == NULL || !macro_has_param(macro, name))
            {
                diagnostic_error_at(pp->dm, token_get_location(token), "'#' is "
                        "not followed by a macro parameter");
                okay = false;
                break;
            }

            // If we we're okay make sure that we again push back the token so
            // that this token is also included in the list.
            token_list_push_back(&tmp_tokens, *token);
            num_tokens++;
        }

        // Otherwise eat the token and get the next one.
        assert(!token_is_type(token, TOK_PP_EOD) && "current on an EOD token");
        preprocessor_next_lexer_token(pp, token);
    }
    assert(num_tokens != 0 && "non-empty body but no tokens?");

    // Okay now we have eaten all of the macros body then we will need to check
    // that the starting and ending token are not the ## token. Note that we 
    // don't bother checking if we have already got an error.
    Token first = token_list_peek_front(&tmp_tokens);
    Token last = token_list_peek_back(&tmp_tokens);
    if (okay && token_is_type(&first, TOK_HASH_HASH))
    {
        diagnostic_error_at(pp->dm, token_get_location(&first), "'##' cannot "
                "appear at start of macro expansion");
        okay = false;
    }
    else if (okay && token_is_type(&last, TOK_HASH_HASH))
    {
        diagnostic_error_at(pp->dm, token_get_location(&last), "'##' cannot "
                "appear at end of macro expansion");
        okay = false;
    }

    // now if we are still good add the body to the macro definition.
    if (okay)
    {
        preprocessor_add_macro_body(pp, macro, &tmp_tokens, num_tokens);
    }

    // make sure to free the memory we used for temporarily storing the tokens.
    token_list_free(&tmp_tokens);
    
    return okay;
}

bool preprocessor_parse_macro_params_and_body(Preprocessor* pp, Token* token,
        Macro* macro)
{
    // Now we want to maybe read the macro parameters and or body. If the next
    // token we get is the end of directive token then we should bail and return
    // true to indicate we are okay. Otherwise if we have a leading space then
    // we cannot be a parameterised macro.
    preprocessor_next_lexer_token(pp, token);
    if (token_is_type(token, TOK_PP_EOD))
    {
        return true;
    }

    if (preprocessor_macro_has_params(pp, token))
    {
        // If we failed to parse our parameters just skip to the end.
        if (!preprocessor_parse_macro_params(pp, token, macro))
        {
            preprocessor_eat_to_eod(pp, token);
            return false;
        }
    }

    // If we failed while parsing the macro body then just skip to the end
    if (!preprocessor_parse_macro_body(pp, token, macro))
    {
        preprocessor_eat_to_eod(pp, token);
        return false;
    }
    assert(token_is_type(token, TOK_PP_EOD) && "parsed macro but not at \\n??");

    // Now we have sucessfully parsed the macro it's body and parameters. So 
    // return true to indicate success !
    return true;
}

void preprocessor_handle_define(Preprocessor* pp, Token* token)
{
    // First try to get the name of the macro we are about to parse.
    Identifier* name;
    Location loc;
    if (!preprocessor_get_macro_name(pp, token, true, &name, &loc))
    {
        return;
    }

    // Now that we know we have the possibility to have a valid macro we should
    // go and parse the macro's parameters and macro body. If this fails that
    // means that we should not try to proprerly define the macro.
    // FIXME: I would like to use the macromap's allocator here instead
    Macro* macro = macro_create(preprocessor_allocator(pp), name, loc, NULL,
            0, false, NULL, 0, false, false);

    // Try to parse the macro parameters and body ignoring the macro entirely
    // if anything fails. This emulations GCC and Clang
    if (!preprocessor_parse_macro_params_and_body(pp, token, macro))
    {
        return;
    }

    // Okay we have a valid and fully formed macro now. We can go and try to
    // define the thing (or hande a redefinition or such).
    MacroMap* macros = preprocessor_macro_map(pp);
    DiagnosticManager* dm = preprocessor_diagnostics(pp);
    macro_map_do_define(macros, dm, macro);
}

void preprocessor_handle_undef(Preprocessor* pp, Token* token)
{
    // First try to get the name of the macro we are about to parse.
    Identifier* name;
    Location loc;
    if (!preprocessor_get_macro_name(pp, token, true, &name, &loc))
    {
        return;
    }

    // Now see if we are at the end of the directive line and handle that
    preprocessor_expect_directive_end(pp, "undef");

    // Finally go and actually handle the undefing
    MacroMap* macros = preprocessor_macro_map(pp);
    DiagnosticManager* dm = preprocessor_diagnostics(pp);
    macro_map_do_undefine(macros, dm, name, loc);
}

bool preprocessor_copy_path(Preprocessor* pp, String str, Location location,
        TokenType type, Filepath* path)
{
    // Decompose into pointer and length pairs.
    const char* ptr = string_get_ptr(&str);
    size_t len = string_get_len(&str);

    // We then need to check those for validity of being able to copy into the
    // path. If they are not valid then we return false but cannot issue a 
    // diagnostic
    char starting_delim = type == TOK_STRING ? '"' : '<';
    char ending_delim = type == TOK_STRING ? '"' : '>';
    assert(ptr[0] == starting_delim && "unusual token start?");

    // For case of #include <abc -> Clang does not double error! so do the same
    if (ptr[len - 1] != ending_delim)
    {
        return false;
    }
    
    // Otherwise let's go and try to copy the filepath into path. So first 
    // adjust our values so that we remove the starting delim and the length of
    // the delims.
    assert(len >= 2 && "name to short?");
    len -= 2;
    ptr += 1;

    if (len == 0)
    {
        diagnostic_error_at(pp->dm, location, "empty filename");
        return false;
    }

    int printed = snprintf(path->path, FILEPATH_LEN, "%.*s", (int) len, ptr);
    path->len = (size_t) printed;

    // Finally, also check if the filename is too long for us to be able to
    // handle. If it is error here and make into an EOF token.
    if (printed > FILEPATH_LEN)
    {
        diagnostic_fatal_error_at(pp->dm, location, "cannot include %s: file "
                "name too long", string_get_ptr(&str));
        return false;
    }

    assert(path->len < FILEPATH_LEN && "path length invalid value");
    return true;
}

bool preprocessor_copy_path_simple(Preprocessor* pp, Token* token,
        Filepath* path)
{
    if (!preprocessor_copy_path(pp, token_get_literal_node(token),
            token_get_location(token), token_get_type(token), path))
    {
        token_set_type(token, TOK_EOF);
        return false;
    }

    return true;
}

bool preprocessor_try_combine_tokens_for_header(Preprocessor* pp, Token* token,
        Filepath* path)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "concatenation of tokens to try to form a header is unimplemented");
    // FIXME: implement

    // TODO: use a buffer and print the current tokens spelling into the buffer
    // TODO: and that kind of thing...
    Buffer buffer = buffer_new();

    // FIXME: will probably just create a new buffer then go and lex a header
    // FIXME: name???
    buffer_free(&buffer);

    return false;
}

bool preprocessor_get_filename(Preprocessor* pp, Token* token, Filepath* path,
        bool* angled)
{
    // Before we advance the token make sure the lexer know's it's okay to parse
    // a header name. This will greatly help us out and save us alot of time
    // having to glue headers togeter alot.
    preprocessor_allow_headers(pp);
    preprocessor_advance_token(pp, token);

    if (token_is_type(token, TOK_STRING))
    {
        *angled = false;
        return preprocessor_copy_path_simple(pp, token, path);
    }
    else if (token_is_type(token, TOK_PP_HEADER_NAME))
    {
        *angled = true;
        return preprocessor_copy_path_simple(pp, token, path);
    }
    else if (token_is_type(token, TOK_LT))
    {
        // FIXME: Clang and GCC both done handle `<<` investigate this?
        *angled = true;
        return preprocessor_try_combine_tokens_for_header(pp, token, path);
    }
    else
    {
        diagnostic_error_at(pp->dm, token_get_location(token), "expected "
                "\"FILENAME\" or <FILENAME>");
        return false;
    }
}

void preprocessor_handle_include(Preprocessor* pp, Token* token)
{
    // Try and get the filename that we are wanting to include. If getting the
    // filename itself fails just eat to the end of the directive. Note that 
    // this doesn't attempt to include it so it is not yet a fatal error.
    Filepath path;
    bool angled;
    if (!preprocessor_get_filename(pp, token, &path, &angled))
    {
        preprocessor_eat_to_eod(pp, token);
        return;
    }

    // Check for the existance of a newline.
    preprocessor_expect_directive_end(pp, "include");

    // Okay we have an okay path and know if we are an angled header or not.
    // We will now need to go try and search for this header. This is probably
    // the most complex step in this process and will require us to do alot of
    // string processing and file statting :)
    // FIXME: Will we need to include information about the directory entry
    // FIXME: this was found in?
    SourceFile* include = NULL;
    if (!preprocessor_try_find_include(pp, &path, angled,
            token_get_location(token), &include))
    {
        diagnostic_fatal_error_at(pp->dm, token_get_location(token),
                "'%s' file not found", filepath_get_cstr(&path));

        // Set that we have a fatal error so we can't continue
        preprocessor_set_fatal_error(pp);
        token_set_type(token, TOK_EOF);
        return;
    }
    assert(include != NULL && "found include but didn't find it?");

    // In this case we should absolutely not do this!
    // FIXME: I don't really like this here but oh well...
    if (preprocessor_include_depth(pp) >= preprocessor_max_include_depth(pp))
    {
        diagnostic_fatal_error_at(pp->dm, token_get_location(token), "#include "
                "nested too deeply");

        // Set that we have a fatal error so we can't continue
        preprocessor_set_fatal_error(pp);
        token_set_type(token, TOK_EOF);
        return;
    }

    // Here we have found a file and know it exsists. We just need to actually 
    // got and do the include for it. This is the easiest part of the whole
    // process :O
    preprocessor_do_include(pp, token, include);
}

void preprocessor_handle_embed(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#embed is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

// This function is for skipping to the next conditional block and then we 
// determine what action we should take by trying to parse the directive that
// we are given. Note that we disable the input diagnostics so that we do
// not accidentally issue spurious diagnostics whilst in the this skipping mode.
// Note that this disables all lexer diagnostics as we consider all of the code
// in the conditional to be dead essentially.
// Also note that we don't want to randomly be popping this include off of the
// preprocessors stack so we should be careful to not use the preprocessing 
// functions for that.
// Additionally, this function only processes one skipped block at a time. When
// we encounter an unnested conditional then we call the parse_directive 
// function so that can properly evaluate the conditional. This may result
// in multiple calls of this function to stack up.
void preprocessor_skip_conditional_block(Preprocessor* pp, Include* include)
{
    // Make sure to disable our lexer diagnostics here.
    lexer_diable_diagnostics(include_get_lexer(include));

    // Now we need to go until we reach a directive that is relavent to us.
    // Note that we want to be as quick as we possibly can, however, this should
    // be fixed later.
    // FIXME: this double lexes every single token in the skipped conditional.
    // TODO: fix the above by giving the lexer the ability to lex EOF as many
    // TODO: times as it needs. This means we can hit eof here and then when
    // TODO: we return from directive parsing mode we relex EOF, and pop all
    // TODO: our conditionals.
    Token tmp_token;
    size_t nest = 0;
    while (true)
    {
        // Peek the next token telling us information about it
        include_peek_next(include, &tmp_token);

        // If we get an eof token we are done since we should not keep eating
        // at this point.
        if (token_is_type(&tmp_token, TOK_EOF))
        {
            break;
        }

        // If we get a hash token then we need to investigate what directive we
        // are going to get.
        if (!preprocessor_directive_start(pp, &tmp_token))
        {
            include_get_next(include, &tmp_token);
            continue;
        }

        // Otherwise we should check the directive type that we have. So start
        // by eating the hash and peeking the next token to see if it is 
        // something we should investigate.
        preprocessor_enter_directive(pp);

        Token hash_token = tmp_token;
        include_get_next(include, &tmp_token); // Eat '#'
        include_peek_next(include, &tmp_token); // Peek the next.

        // Not interesting if it's not a directive at all or some kind of 
        // error e.g. # 3, # \n etc...
        if (!token_is_identifier_like(&tmp_token))
        {
            continue;
        }

        // Get the keyword from the identifier and act based on the type of
        // condition it is but only if it was relavent.
        Identifier* directive = token_get_identifier(&tmp_token);
        TokenType pp_keyword = identifier_get_pp_keyword(directive);
        switch (pp_keyword)
        {
            // We are only interested in any conditional directives here. Note 
            // that for all of the directives that require either a macro name
            // or some kind of expression
            case TOK_PP_if:
            case TOK_PP_ifdef:
            case TOK_PP_ifndef:
            {
                // Handle any open conditionals here but do not process the 
                // directives since Clang and GCC do not. Instead we just trust
                // that the directives were okay.
                Location location = token_get_location(&tmp_token);
                include_push_conditional(include, location, false);
                nest++;

                preprocessor_eat_to_eod(pp, &tmp_token);
                continue;
            }

            case TOK_PP_elif:
            case TOK_PP_elifdef:
            case TOK_PP_elifndef:
            case TOK_PP_else:
            {
                // If this was our unnested endif then we are done as we get 
                // the handling directive loop to handle any directives which
                // could actually be of consequence to us.
                if (nest == 0)
                {
                    break;
                }

                // get the current conditional from the include so we know if
                // we have seen the else before
                Conditional* cond = include_get_current_conditional(include);

                // If we had the else than error about getting this directive
                // after the else like Clang and GCC do.
                if (conditional_had_else(cond))
                {
                    diagnostic_error_at(pp->dm, token_get_location(&tmp_token),
                            "#%s after #else", identifier_cstr(directive));
                }

                // If we were the else we want to make sure we handled any
                // invalid orderings of these that may show up.
                if (pp_keyword == TOK_PP_else)
                {
                    conditional_set_else(cond);
                }

                preprocessor_eat_to_eod(pp, &tmp_token);
                continue;
            }

            case TOK_PP_endif:
            {
                // If this was our unnested endif then we are done.
                if (nest == 0)
                {
                    break;
                }
                
                // Otherwise this was a nested endif and we should pop it
                // off of our conditional stack
                include_pop_conditional(include);
                nest--;

                preprocessor_eat_to_eod(pp, &tmp_token);
                continue;
            }

            // everything else is not interesting and should be ignored.
            default:
                continue;
        }

        // We have some kind of interesting pp directive that we should parse
        // so exit this loop here. So exit so we can reenable the lexer 
        // diagnostics for it. Also reset the temporary token to be the hash
        // token so that the parse_directive function doesnt freak out.
        tmp_token = hash_token;
        break;
    }
    
    // Make sure to reenabe lexer diagnostics before doing anything
    lexer_enable_diagnostics(include_get_lexer(include),
            preprocessor_diagnostics(pp));

    // If we didn't get an EOF token then we should have a directive
    if (!token_is_type(&tmp_token, TOK_EOF))
    {
        assert(preprocessor_directive_start(pp, &tmp_token) && "directive?");
        preprocessor_parse_directive(pp, &tmp_token);
    }
}

void preprocessor_handle_if(Preprocessor* pp, Token* token)
{
    // Save the if location for us so that we can push an include with a correct
    // location in the future.
    Location if_loc = token_get_location(token);

    // We want to get the next macro expanded token here
    preprocessor_advance_token(pp, token);
    
    // Now we will want to go attempt to evaluate the preprocessor directive
    // expression and return the sucess or failure of this. If we fail to parse
    // the expression, no recovery is performed so we will have to eat to EOD
    PPValue value = {0};
    if (!preprocessor_parse_expression(pp, token, &value))
    {
        preprocessor_eat_to_eod(pp, token);
    }
    assert(token_is_type(token, TOK_PP_EOD) && "not at EOD?");

    // Now no matter if we suceeded or not we need to handle the #if branch here
    // If we did not succeed parsing consider the value to be '0'
    // Get the current include and it's stack of conditionals
    Include* current = preprocessor_current_input(pp);
    ConditionalVector* conditionals = include_get_conditionals(current);
    if (!pp_value_get_success(&value) || pp_value_get_value(&value) == 0)
    {
        conditional_vector_push(conditionals, conditional_create(if_loc,
                /*branch taken*/false));
        preprocessor_skip_conditional_block(pp, current);
    }
    else
    {
        conditional_vector_push(conditionals, conditional_create(if_loc,
                /*branch taken*/true));
    }
}

void preprocessor_handle_ifdef_type(Preprocessor* pp, Token* token, bool ifdef)
{
    // Save the ifdef tok for pushing onto the conditional stack
    Location ifdef_loc = token_get_location(token);

    // Get the current include and it's stack of conditionals
    Include* current = preprocessor_current_input(pp);
    ConditionalVector* conditionals = include_get_conditionals(current);

    // Then try to read the macro name.
    Identifier* name = NULL;
    Location loc = LOCATION_INVALID;
    if (!preprocessor_get_macro_name(pp, token, false, &name, &loc))
    {
        // both GCC and Clang like to skip the entire conditional block if no 
        // macro name was given. So do this as well.
        conditional_vector_push(conditionals, conditional_create(ifdef_loc,
                false));
        preprocessor_skip_conditional_block(pp, current);
        return;
    }

    preprocessor_expect_directive_end(pp, ifdef ? "ifdef" : "ifndef");

    // Now we know we have a valid macro name so get if the current macro is
    // defined at the moment or not.
    bool defined = macro_map_is_defined(preprocessor_macro_map(pp), name);

    // Here if were defined and in ifdef then we want to read the next section.
    // if we aren't defined and in an ifndef section then read it as well. 
    // Otherwise we should skip the next section. For both situations we will
    // need to create and entry on the conditional stack which reflects the 
    // current situation.
    if (defined == ifdef)
    {
        // Push a conditional entry saying that we have read one of the branches
        // in the conditional and just stop here. Since we don't want to skip
        // a branch we 
        conditional_vector_push(conditionals, conditional_create(ifdef_loc,
                /*branch taken*/true));
    }
    else
    {
        // Push a conditional entry saying that we have skipped this one and
        // then go and do the skipping.
        conditional_vector_push(conditionals, conditional_create(ifdef_loc,
                /*branch taken*/false));
        preprocessor_skip_conditional_block(pp, current);
    }
}

void preprocessor_handle_ifdef(Preprocessor* pp, Token* token)
{
    preprocessor_handle_ifdef_type(pp, token, true);
}

void preprocessor_handle_ifndef(Preprocessor* pp, Token* token)
{
    preprocessor_handle_ifdef_type(pp, token, false);
}

bool preprocessor_handle_spurious_conditional(Preprocessor* pp, Token* token,
        Include* current, const char* context)
{
    if (include_conditional_empty(current))
    {
        Location location = token_get_location(token);
        diagnostic_error_at(pp->dm, location, "#%s without #if", context);
        return true;
    }
    
    return false;
}

void preprocessor_handle_elifdef(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#elifdef is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_elifndef(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#elifndef is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_elif(Preprocessor* pp, Token* token)
{
    // Save the if location for us so that we can push an include with a correct
    // location in the future.
    Location elif_loc = token_get_location(token);

    // We want to get the next macro expanded token here
    preprocessor_advance_token(pp, token);
    
    // Now we will want to go attempt to evaluate the preprocessor directive
    // expression and return the sucess or failure of this. If we fail to parse
    // the expression, no recovery is performed so we will have to eat to EOD
    PPValue value = {0};
    if (!preprocessor_parse_expression(pp, token, &value))
    {
        preprocessor_eat_to_eod(pp, token);
    }
    assert(token_is_type(token, TOK_PP_EOD) && "not at EOD?");

    // If we failed the parsing, or the value was 0, or we have already taken
    // a branch skip the block that we just got.
    Include* current = preprocessor_current_input(pp);
    ConditionalVector* conditionals = include_get_conditionals(current);
    if (!pp_value_get_success(&value) || pp_value_get_value(&value) == 0
            || conditional_taken(conditional_vector_back(conditionals)))
    {
        preprocessor_skip_conditional_block(pp, current);
    }

    // Otherwise we have nothing left to do so just 'enter' the block we got
    // and handle it as needed.
}

void preprocessor_handle_else(Preprocessor* pp, Token* token)
{
    // We should not have any extra tokens at the end of the directive
    preprocessor_expect_directive_end(pp, "endif");

    // Get the current input from the preprocessor and check if this endif is
    // valid or not. If it is invalid we have nothing to do.
    Include* include = preprocessor_current_input(pp);
    if (preprocessor_handle_spurious_conditional(pp, token, include, "else"))
    {
        return;
    }

    // Get the current conditional from the include and determine if we should
    // take the branch or not.
    Conditional* conditional = include_get_current_conditional(include);

    // Also remember if the conditional had else as we are going to update it
    // now no matter what happens
    bool had_else = conditional_had_else(conditional);
    conditional_set_else(conditional);

    // If we previously had an else directive then error about that. Also skip
    // the block like clang seems to do.
    if (had_else)
    {
        Location location = token_get_location(token);
        diagnostic_error_at(pp->dm, location, "#else after #else");
        preprocessor_skip_conditional_block(pp, include);
        return;
    }

    // If we have already have a conditional path taken we should skip this path
    if (conditional_taken(conditional))
    {
        preprocessor_skip_conditional_block(pp, include);
        return;
    }
}

void preprocessor_handle_endif(Preprocessor* pp, Token* token)
{
    // We should not have any extra tokens at the end of the directive
    preprocessor_expect_directive_end(pp, "endif");

    // Get the current input from the preprocessor and check if this endif is
    // valid or not. If it is invalid we have nothing to do.
    Include* include = preprocessor_current_input(pp);
    if (preprocessor_handle_spurious_conditional(pp, token, include, "endif"))
    {
        return;
    }

    // If this include is valid, for endif's we simply pop off the top of the
    // include stack.
    include_pop_conditional(include);
}

void preprocessor_handle_line(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#line is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_diagnostic(Preprocessor* pp, Token* token, bool warn)
{
    // First read the diagnostic string from the lexer using the preprocessor.
    Buffer buffer = buffer_new();
    preprocessor_read_diagnostic_string(pp, &buffer);

    // This is fine since the location of the token is unaffected
    Location location = token_get_location(token);
    char* diag = buffer_get_ptr(&buffer);
    if (!warn)
    {
        diagnostic_error_at(pp->dm, location, "%s", diag);
    }
    else
    {
        diagnostic_warning_at(pp->dm, location, Whash_warning, "%s", diag);
    }
    buffer_free(&buffer);

    // Finally eat the newline to get us out of directive mode. 
    preprocessor_next_lexer_token(pp, token);
    assert(token_is_type(token, TOK_PP_EOD) && "not at the end of the line??");
}

void preprocessor_handle_error(Preprocessor* pp, Token* token)
{
    preprocessor_handle_diagnostic(pp, token, false);
}

void preprocessor_handle_warning(Preprocessor* pp, Token* token)
{
    preprocessor_handle_diagnostic(pp, token, true);
}

void preprocessor_handle_pragma(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#pragma is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_gnu_line_directive(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token),
            Wunimplemented, "GNU style line directives are unimplemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_parse_directive(Preprocessor* pp, Token* token)
{
    assert(preprocessor_directive_start(pp, token));

    // Enter directive mode in the lexer. So that we can sucessfully parse it.
    // Without this we would not get the needed EOD token.
    preprocessor_enter_directive(pp);

    // Now get the directive token from the preprocessor.
    preprocessor_next_lexer_token(pp, token);

    // We got the null directive. # \n
    if (token_is_type(token, TOK_PP_EOD))
    {
        return;
    }

    // Otherwise we should have a nice identifier like token
    if (token_is_identifier_like(token))
    {
        Identifier* directive = token_get_identifier(token);
        switch (identifier_get_pp_keyword(directive))
        {
            case TOK_PP_define:
                preprocessor_handle_define(pp, token);
                return;

            case TOK_PP_undef:
                preprocessor_handle_undef(pp, token);
                return;

            case TOK_PP_include:
                preprocessor_handle_include(pp, token);
                return;

            case TOK_PP_embed:
                preprocessor_handle_embed(pp, token);
                return;

            case TOK_PP_if:
                preprocessor_handle_if(pp, token);
                return;

            case TOK_PP_ifdef:
                preprocessor_handle_ifdef(pp, token);
                return;

            case TOK_PP_ifndef:
                preprocessor_handle_ifndef(pp, token);
                return;

            case TOK_PP_elifdef:
                preprocessor_handle_elifdef(pp, token);
                return;

            case TOK_PP_elifndef:
                preprocessor_handle_elifndef(pp, token);
                return;

            case TOK_PP_elif:
                preprocessor_handle_elif(pp, token);
                return;

            case TOK_PP_else:
                preprocessor_handle_else(pp, token);
                return;

            case TOK_PP_endif:
                preprocessor_handle_endif(pp, token);
                return;

            case TOK_PP_line:
                preprocessor_handle_line(pp, token);
                return;
            
            case TOK_PP_error:
                preprocessor_handle_error(pp, token);
                return;

            case TOK_PP_warning:
                preprocessor_handle_warning(pp, token);
                return;

            case TOK_PP_pragma:
                preprocessor_handle_pragma(pp, token);
                return;

            default:
                break;                
        }
    }

    // GNU line directives which are not implemented and will require alot more
    // preprocessor support to start implementing.
    if (token_is_type(token, TOK_NUMBER))
    {
        preprocessor_handle_gnu_line_directive(pp, token);
        return;
    }

    // Otherwise we failed to get a good directive, so just eat to the end of 
    // the line and carry on like nothing happened.
    diagnostic_error_at(pp->dm, token_get_location(token), "invalid "
            "preprocessing directive");
    preprocessor_eat_to_eod(pp, token);
    return;
}
