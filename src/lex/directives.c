#include "directives.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "driver/warning.h"
#include "files/filepath.h"
#include "files/source_manager.h"
#include "util/arena.h"
#include "util/buffer.h"
#include "util/ptr_set.h"

#include "driver/diagnostic.h"

#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/macro.h"
#include "lex/macro_map.h"
#include "lex/token.h"
#include "lex/preprocessor.h"
#include "util/str.h"

bool preprocessor_directive_start(Preprocessor* pp, Token* token)
{
    return token_is_type(token, TOK_HASH) 
            && token_has_flag(token, TOK_FLAG_BOL);
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
    Macro* macro = macro_create(preprocessor_allocator(pp), name, loc, NULL,
            0, false, NULL, 0, false, false);

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

void preprocessor_handle_if(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#if is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_ifdef(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#ifdef is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_ifndef(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#ifndef is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
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
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#elif is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_else(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#else is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
}

void preprocessor_handle_endif(Preprocessor* pp, Token* token)
{
    diagnostic_warning_at(pp->dm, token_get_location(token), Wunimplemented,
            "#endif is valid but not implemented");
    preprocessor_eat_to_eod(pp, token);
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
