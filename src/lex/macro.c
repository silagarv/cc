#include "macro.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "lex/identifier_table.h"
#include "util/arena.h"
#include "util/buffer.h"

#include "lex/token.h"

Macro* macro_create(Arena* allocator, Identifier* name, Location location,
        Identifier** params, size_t num_params, bool variadic, Token* tokens,
        size_t num_tokens, bool builtin, bool pragma)
{
    Macro* macro = arena_allocate_size(allocator, sizeof(Macro));
    *macro = (Macro)
    {
        .name = name,
        .location = location,
        .params = params,
        .num_params = num_params,
        .variadic = variadic,
        .tokens = tokens,
        .num_tokens = num_tokens,
        .builtin_macro = builtin,
        .pragma = pragma
    };

    return macro;
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

Token* macro_tokens(const Macro* macro)
{
    return macro->tokens;
}

size_t macro_num_tokens(const Macro* macro)
{
    return macro->num_tokens;
}

bool macro_builtin(const Macro* macro)
{
    return macro->builtin_macro;
}

bool macro_pragma(const Macro* macro)
{
    return macro->pragma;
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
        if (/*tokens_equal(tokens1[i], tokens2[i])*/1)
        {
            return false;
        }
    }

    // We have checked that everything about the macros is the same
    return true;
}

Token macro_stringify_arg(const Token* tokens, size_t num_tokens)
{
    return (Token) {0};
}

// A routine to destringize a string literal token that was put in the _Pragma
// operator which is done as following.
// 1. delete the L prefix if present
// 2. delete leading and trailing quotes
// 3. \" replaced by ", and \\ replaced by \.
// 4. then produce their PP tokens i.e. once in buffer we can lex that :)
Buffer destringize_pragma_literal(const Token* token)
{
    assert(token_is_string(token));

    // We know the buffer will be strictly less than this length
    const String* literal = &token->data.literal->value;
    const size_t token_length = literal->len;
    Buffer buffer = buffer_new_size(token_length);

    size_t pos = 0;

    // Ignore the 'L'
    if (token->type == TOK_WIDE_STRING)
    {
        assert(string_get(literal, pos) == 'L');
        pos++;
    }
    
    // Now get the ending position and the correct starting position
    assert(string_get(literal, pos) == '"');
    assert(string_get(literal, token_length - 1) == '"');

    // Skip the starting delimiter and get the ending position
    pos++;
    const size_t ending_pos = token_length - 1;

    while (pos != ending_pos)
    {
        char current = string_get(literal, pos);

        pos++;

        // Note that we cannot end a string literal with a \" so this should
        // never overread the literals buffer
        if (current == '\\')
        {
            char next = string_get(literal, pos);

            pos++;

            if (next == '"' || next == '\\')
            {
                buffer_add_char(&buffer, next);
            }
            else
            {
                buffer_add_char(&buffer, current);
                buffer_add_char(&buffer, next);
            }

            continue;
        }

        buffer_add_char(&buffer, current);
    }

    // Put a newline in the buffer and finish it off
    buffer_add_char(&buffer, '\n');
    buffer_make_cstr(&buffer);

    return buffer;
}


