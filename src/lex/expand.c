#include "expand.h"

#include <assert.h>

#include "util/arena.h"

#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/macro.h"

MacroExpansion* macro_expansion_create(Arena* allocator, Macro* macro,
        Location location, MacroExpansion* prev)
{
    MacroExpansion* expansion = arena_malloc(allocator, sizeof(MacroExpansion));
    *expansion = (MacroExpansion)
    {
        .macro = macro,
        .tokens = macro_get_stream(macro),
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
    return (MacroExpander) { arena_new_default(), sm, 0, NULL };
}

void macro_expander_delete(MacroExpander* expander)
{
    arena_delete(&expander->allocator);
}

Arena* macro_expander_allocator(MacroExpander* expander)
{
    return &expander->allocator;
}

unsigned int macro_expander_counter(const MacroExpander* expander)
{
    return expander->counter;
}

bool macro_expander_expanding(const MacroExpander* expander)
{
    return expander->expansion != NULL;
}

void macro_expander_push(MacroExpander* expander, Macro* macro,
        Location location)
{
    assert(!macro_disabled(macro) && "macro is currently expanding!");

    // Before we go and push the macro we should check that the macro is not
    // empty. If it is empty we can simply return. This act's like we pushed,
    // expanded nothing and then popped it off.
    if (macro_num_tokens(macro) == 0)
    {
        return;
    }

    // Push to our macro stack
    expander->expansion = macro_expansion_create(&expander->allocator, macro,
                location, expander->expansion);

    // And then also remember to disable the macro once we push it.
    macro_disable(macro);
}

void macro_expander_pop(MacroExpander* expander)
{
    assert(macro_expansion_finished(expander->expansion) && "unfinished invoc");
    
    // Make sure to get the macro and re-enable it so we can expand it again
    Macro* macro = macro_expansion_macro(expander->expansion);
    macro_enable(macro);

    // Then pop the expansion off of the stack
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
    return true;
}

