#include "include_stack.h"

#include <stddef.h>
#include <assert.h>

#include "files/location.h"

#include "lex/lexer.h"

// Make sure to create the implementations for the vectors!
vector_of_impl(Conditional, Conditional, conditional)
vector_of_impl(Include, Include, include)

Conditional conditional_create(Location location)
{
    return (Conditional) { location };
}

Location conditional_location(const Conditional* conditional)
{
    return conditional->location;
}

// General include handling functions.
Include include_create(DiagnosticManager* dm, LangOptions* opts,
        Arena* literals, IdentifierTable* names, SourceFile* source,
        DirectoryEntry* entry)
{
    // Create the include given that we already have everything else.
    Include include = {0};
    lexer_create(&include.lexer, dm, opts, literals, names, source);
    include.search_path = entry;
    include.cond = conditional_vector();
    return include;
}

void include_delete(Include* include)
{
    conditional_vector_free(&include->cond, NULL);
}

Lexer* include_get_lexer(Include* include)
{
    assert(include != NULL && "include is NULL");
    return &include->lexer;
}
DirectoryEntry* include_get_search_path(Include* include)
{
    assert(include != NULL && "include is NULL");
    return include->search_path;
}

ConditionalVector* include_get_conditionals(Include* include)
{
    assert(include != NULL && "include is NULL");
    return &include->cond;
}

bool include_get_next(Include* include, Token* token)
{
    assert(include != NULL && token != NULL && "need both input and token!");
    return lexer_get_next(&include->lexer, token);
}

bool include_peek_next(Include* include, Token* token)
{
    assert(include != NULL && token != NULL && "need both input and token!");
    return lexer_peek_next(&include->lexer, token);
}

// Functions for pushing, popping, and general handling of conditionals
// TODO: these will remain unimplemented.
void include_push_conditional(Include* include, Location location)
{
    conditional_vector_push(&include->cond, conditional_create(location));
}

Location include_pop_conditional(Include* include)
{
    Conditional conditional = conditional_vector_pop(&include->cond);
    return conditional_location(&conditional);
}

bool include_conditional_empty(const Include* include)
{
    return conditional_vector_empty(&include->cond);
}
