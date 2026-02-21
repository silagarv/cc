#ifndef MACRO_H
#define MACRO_H

#include <stdbool.h>
#include <stddef.h>

#include "util/arena.h"
#include "util/buffer.h"

#include "files/location.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

// All of the information that we need for macros. Contains everything we need 
// to know for it.
typedef struct Macro {
    Identifier* name; // The name of the macro defined
    Location location; // The location this macro was defined at.

    Identifier** params; // the names of the paramaeters in a function macro
    size_t num_params; // the number of paramaeters this macro has
    bool variadic; // is this macro variadic e.g. #define A(a, ...) ...

    Token* tokens; // The list of replacement tokens in the macro
    size_t num_tokens; // the number of replacement tokens in the macro

    bool builtin_macro; // Does this require special handling at all?
                        // This is mainly used for __LINE__, __COUNTER__, etc...
                        // macros since they need to be specially expanded
    bool pragma; // Are we the _Pragma macro
} Macro;

Macro* macro_create(Arena* allocator, Identifier* name, Location location,
        Identifier** params, size_t num_params, bool variadic, Token* tokens,
        size_t num_tokens, bool builtin, bool pragma);
Identifier* macro_name(const Macro* macro);
Location macro_location(const Macro* macro);
Identifier** macro_params(const Macro* macro);
size_t macro_num_params(const Macro* macro);
bool macro_variadic(const Macro* macro);
Token* macro_tokens(const Macro* macro);
size_t macro_num_tokens(const Macro* macro);
bool macro_builtin(const Macro* macro);
bool macro_pragma(const Macro* macro);
bool macro_has_param(const Macro* macro, const Identifier* identifier);

// Function to check if two macro definitios are equal
// TODO: will need to implement a function for checking if two tokens are
// TODO: equal...
bool macro_definitions_equal(const Macro* macro1, const Macro* macro2);

Token macro_stringify_arg(const Token* tokens, size_t num_tokens);
Token macro_concat_tokens(const Token* token1, const Token* token2);

// Destringize a pragma literal token as we will consider this to be macro like
// in terms of the preprocessor
Buffer destringize_pragma_literal(const Token* token);


#endif /* MACRO_H */
