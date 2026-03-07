#include "expand.h"

#include <assert.h>

#include "files/location.h"
#include "util/arena.h"

#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/macro.h"

MacroArgs macro_args_create(Token* tokens, size_t num_tokens)
{
    return (MacroArgs) { tokens, num_tokens };
}

TokenStream macro_args_get_stream(const MacroArgs* args)
{
    return token_stream_create(args->tokens, args->num_tokens);
}

TokenStream macro_expansion_calculate_replacement(Arena* arena, Macro* macro,
        MacroArgs* args, Location location)
{
    // If we are a built in macro we will do some special processing by the 
    // preprocessor in order to properly be expanded. So go off and handle this
    // properly returning a replacement token stream for us to lex
    if (macro_builtin(macro))
    {
        // TODO: builtin macros will need to be specifically dealt with.
        return macro_get_stream(macro);
    }

    // If we are not a function like macro then the replacement list is simply
    // the replacement list of the macro (i.e. no macro parameters that we need
    // to be able to handle)
    if (!macro_function_like(macro))
    {
        return macro_get_stream(macro);
    }

    return macro_get_stream(macro);
}

MacroExpansion* macro_expansion_create(Arena* allocator, Macro* macro,
        MacroArgs* args, Location location, MacroExpansion* prev)
{
    assert((macro_num_params(macro) ? args != NULL : true)
            && "macro has params but no args found?");

    // Determine the replacement list of the macro here. Note that we may have
    // to do some expansion of macros in the replacement list for this.
    TokenStream replacement_list = macro_expansion_calculate_replacement(
            allocator, macro, args, location);

    // Finally, once we have go the replacement list finish creating the macro
    // expansion structure and push it.
    MacroExpansion* expansion = arena_malloc(allocator, sizeof(MacroExpansion));
    *expansion = (MacroExpansion)
    {
        .macro = macro,
        .tokens = replacement_list,
        .args = args,
        .location = location,
        .prev = prev
    };

    // TODO: we also need to handle preexpansion of macro arguments somehow?

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

void macro_expander_increment_counter(MacroExpander* expander)
{
    expander->counter++;
}

bool macro_expander_expanding(const MacroExpander* expander)
{
    return expander->expansion != NULL;
}

void macro_expander_push(MacroExpander* expander, Macro* macro, MacroArgs* args,
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

    // Push to our macro stack with all of the given macro information.
    expander->expansion = macro_expansion_create(&expander->allocator, macro,
            args, location, expander->expansion);

    // And then also remember to disable the macro once we push it.
    // FIXME: I think this may have to be moved above to disable this macro as
    // FIXME: we are (possibly) expanding it's parameters
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

bool macro_expander_peek(MacroExpander* expander, Token* token)
{
    assert(macro_expander_expanding(expander) && "should be expanding");

    // Similar to the above code, whilst we are at the end of expansions we need
    // to be able peek tokens to see what we get. Note, that we don't pop them
    // off of the stack here, rather we travel down the stack until we find what
    // we're looking for.
    MacroExpansion* expansion = expander->expansion;
    while (macro_expansion_finished(expansion))
    {
        expansion = macro_expansion_prev(expansion);
        
        // NULL means theres no more expansions so no more macro tokens
        if (expansion == NULL)
        {
            return false;
        }        
    }
    
    // Otherwise peek the token on the expansion
    *token = macro_expansion_peek(expansion);
    return true;
}

