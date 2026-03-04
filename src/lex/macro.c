#include "macro.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "files/location.h"
#include "lex/identifier_table.h"
#include "util/arena.h"

#include "lex/token.h"

Macro* macro_create(Arena* allocator, Identifier* name, Location location,
        Identifier** params, size_t num_params, bool variadic, Token* tokens,
        size_t num_tokens, bool builtin, bool pragma)
{
    Macro* macro = arena_malloc(allocator, sizeof(Macro));
    *macro = (Macro)
    {
        .name = name,
        .location = location,
        .params = params,
        .num_params = num_params,
        .variadic = variadic,
        .tokens = tokens,
        .num_tokens = num_tokens,
        .function_like = false,
        .builtin_macro = builtin,
        .pragma = pragma,
        .disabled = false
    };

    return macro;
}

Macro* macro_create_builtin(Arena* allocator, Identifier* name, bool pragma)
{
    return macro_create(allocator, name, LOCATION_INVALID, NULL, 0, false, NULL,
            0, true, pragma);
}

Identifier* macro_name(const Macro* macro)
{
    return macro->name;
}

Location macro_location(const Macro* macro)
{
    return macro->location;
}

Identifier** macro_params(const Macro* macro)
{
    return macro->params;
}

size_t macro_num_params(const Macro* macro)
{
    return macro->num_params;
}

bool macro_variadic(const Macro* macro)
{
    return macro->variadic;
}

void macro_set_params(Macro* macro, Identifier** params, size_t num_params,
        bool variadic)
{
    macro->params = params;
    macro->num_params = num_params;
    macro->variadic = variadic;

    // Make sure to set this flag. Since params can be NULL and this can be
    // valid
    macro->function_like = true;
}

Token* macro_tokens(const Macro* macro)
{
    return macro->tokens;
}

size_t macro_num_tokens(const Macro* macro)
{
    return macro->num_tokens;
}

void macro_set_tokens(Macro* macro, Token* tokens, size_t num_tokens)
{
    assert(tokens && num_tokens && "should not be setting tokens");

    macro->tokens = tokens;
    macro->num_tokens = num_tokens;
}

bool macro_function_like(const Macro* macro)
{
    return macro->function_like;
}

bool macro_builtin(const Macro* macro)
{
    return macro->builtin_macro;
}

bool macro_pragma(const Macro* macro)
{
    return macro->pragma;
}

bool macro_disabled(const Macro* macro)
{
    return macro->disabled;
}

void macro_disable(Macro* macro)
{
    macro->disabled = true;
}

void macro_enable(Macro* macro)
{
    macro->disabled = false;
}

bool macro_has_param(const Macro* macro, const Identifier* identifier)
{
    Identifier** params = macro_params(macro);
    size_t num_params = macro_num_params(macro);

    for (size_t i = 0; i < num_params; i++)
    {
        if (params[i] == identifier)
        {
            return true;
        }
    }

    return false;
}

size_t macro_get_param_num(const Macro* macro, const Identifier* identifier)
{
    assert(macro_has_param(macro, identifier) && "Don't have this param??");

    Identifier** params = macro_params(macro);
    size_t num_params = macro_num_params(macro);

    for (size_t i = 0; i < num_params; i++)
    {
        if (params[i] == identifier)
        {
            return i;
        }
    }

    assert(false && "uncreacable");
    return (size_t)(-1);
}

bool macro_definitions_equal(const Macro* macro1, const Macro* macro2)
{
    // Check that everything about the macros is the same. Start with the easy
    // stuff so that any obviously different macros are weeded out fast
    if (macro_name(macro1) != macro_name(macro2))
    {
        return false;
    }

    if (macro_builtin(macro1) != macro_builtin(macro2))
    {
        return false;
    }

    if (macro_pragma(macro1) != macro_pragma(macro2))
    {
        return false;
    }
    
    // Now check parameters
    if (macro_variadic(macro1) != macro_variadic(macro2))
    {
        return false;
    }

    if (macro_num_params(macro1) != macro_num_params(macro2))
    {
        return false;
    }

    size_t num_params = macro_num_params(macro1);
    Identifier** params1 = macro_params(macro1);
    Identifier** params2 = macro_params(macro2);
    for (size_t i = 0; i < num_params; i++)
    {
        if (params1[i] != params2[i])
        {
            return false;
        }
    }

    // Now for the replacement list
    if (macro_num_tokens(macro1) != macro_num_tokens(macro2))
    {
        return false;
    }

    size_t num_tokens = macro_num_tokens(macro1);
    Token* tokens1 = macro_tokens(macro1);
    Token* tokens2 = macro_tokens(macro2);
    for (size_t i = 0; i < num_tokens; i++)
    {
        if (!tokens_equal(&tokens1[i], &tokens2[i]))
        {
            return false;
        }
    }

    // We have checked that everything about the macros is the same
    return true;
}

TokenStream macro_get_stream(const Macro* macro)
{
    return token_stream_create(macro->tokens, macro->num_tokens);
}
