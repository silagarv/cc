#include "macro.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "util/buffer.h"

#include "lex/token.h"

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


