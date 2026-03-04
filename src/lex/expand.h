#ifndef EXPAND_H
#define EXPAND_H

#include <stddef.h>

#include "files/location.h"
#include "util/arena.h"

#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/macro.h"

// typedef struct MacroArgs {
//     Token* tokens;
//     size_t num_tokens;
// } MacroArgs;

// A structure to hold and represent the current state of a macro expansion. 
// This gives us the raw tokens from the body of the macro and 
typedef struct MacroExpansion {
    Macro* macro; // The macro currently being expanded

    TokenStream tokens; // The stream of tokens that we are currently expanding
                        // This also helps store the current position in the 
                        // expansion for us.

    Location location; // The location that this macro was invoked at.

    struct MacroExpansion* prev; // The previous macro expansion which is the
                                 // one that triggered this one or NULL
} MacroExpansion;

typedef struct MacroExpander {
    Arena allocator; // The allocator used for macro expansion and building our
                     // macro expansion stack.

    // TODO: the source manager is not used yet, however, when it comes to 
    // TODO: token pasting and other things it will be important
    SourceManager* sm; // The source manager. Used for creating new buffers and
                       // setting up the locations for the tokens produced by
                       // the macro expansion as needed.

    // TODO: I should add all of the preprocessor's important identifiers in
    // TODO: here so that they can function properly.

    unsigned int counter; // The current value of the counter macro, this is 
                          // initially set to 0 and // incrememnted each time 
                          // the __COUNTER__ macro is used.

    MacroExpansion* expansion; // the current macro expansion or NULL if we are
                               // done expanding
} MacroExpander;

MacroExpansion* macro_expansion_create(Arena* allocator, Macro* macro,
        Location location, MacroExpansion* prev);

Macro* macro_expansion_macro(const MacroExpansion* expansion);
TokenStream* macro_expansion_tokens(MacroExpansion* expansion);
Location macro_expansion_location(const MacroExpansion* expansion);
MacroExpansion* macro_expansion_prev(const MacroExpansion* expansion);

Token macro_expansion_consume(MacroExpansion* expansion);
Token macro_expansion_peek(const MacroExpansion* expansion);
bool macro_expansion_finished(const MacroExpansion* expansion);

MacroExpander macro_expander_create(SourceManager* sm);
void macro_expander_delete(MacroExpander* expander);

Arena* macro_expander_allocator(MacroExpander* expander);
unsigned int macro_expander_counter(const MacroExpander* expander);
bool macro_expander_expanding(const MacroExpander* expander);

void macro_expander_push(MacroExpander* expander, Macro* macro,
        Location location);

bool macro_expander_next(MacroExpander* expander, Token* token);

#endif /* EXPAND_H */
