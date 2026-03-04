#ifndef MACRO_H
#define MACRO_H

#include <stdbool.h>
#include <stddef.h>

#include "util/arena.h"

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

    bool function_like; // true if this macro is a function like macro
    bool builtin_macro; // Does this require special handling at all?
                        // This is mainly used for __LINE__, __COUNTER__, etc...
                        // macros since they need to be specially expanded
    bool pragma; // Are we the _Pragma macro

    bool disabled; // Is this macro currently disabled? Set to false on creation
} Macro;

Macro* macro_create(Arena* allocator, Identifier* name, Location location,
        Identifier** params, size_t num_params, bool variadic, Token* tokens,
        size_t num_tokens, bool builtin, bool pragma);
Macro* macro_create_builtin(Arena* allocator, Identifier* name, bool pragma);

Identifier* macro_name(const Macro* macro);
Location macro_location(const Macro* macro);

Identifier** macro_params(const Macro* macro);
size_t macro_num_params(const Macro* macro);
bool macro_variadic(const Macro* macro);

// Set the parameters in the macro but also set it's function_like flag to be
// true. Important for macros like #define FOO()
void macro_set_params(Macro* macro, Identifier** params, size_t num_params,
        bool variadic);

Token* macro_tokens(const Macro* macro);
size_t macro_num_tokens(const Macro* macro);

void macro_set_tokens(Macro* macro, Token* tokens, size_t num_tokens);

bool macro_function_like(const Macro* macro);
bool macro_builtin(const Macro* macro);
bool macro_pragma(const Macro* macro);
bool macro_disabled(const Macro* macro);

void macro_disable(Macro* macro);
void macro_enable(Macro* macro);

bool macro_has_param(const Macro* macro, const Identifier* identifier);
size_t macro_get_param_num(const Macro* macro, const Identifier* identifier);

// Function to check if two macro definitios are equal
// TODO: will need to implement a function for checking if two tokens are
// TODO: equal...
bool macro_definitions_equal(const Macro* macro1, const Macro* macro2);

// Get the tokens from the macro as a token stream for the purposes of expansion
TokenStream macro_get_stream(const Macro* macro);

#endif /* MACRO_H */
