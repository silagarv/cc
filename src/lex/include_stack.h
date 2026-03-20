#ifndef INCLUDE_STACK_H
#define INCLUDE_STACK_H

#include "driver/diagnostic.h"
#include "driver/lang.h"
#include "files/location.h"
#include "files/source_manager.h"
#include "lex/identifier_table.h"
#include "lex/token.h"
#include "util/arena.h"
#include "util/vec.h"

#include "lex/lexer.h"
#include "lex/header_finder.h"

// This conditional struct is here and not in it's own file since it is tied to
// an input lexer. But, I do not want to put it with the lexer because then it
// feels to me like the lexer would `know` too much and would be doing things
// outside of it's scope in my opinion.
typedef struct Conditional {
    Location location; // The location of the `if` in this conditional.
    bool taken; // Have we taken one of the paths of the conditional yet?
    bool had_else; // Have we seen an 'else' branch in the conditional yet?
} Conditional;

vector_of_decl(Conditional, Conditional, conditional);

// This represents an input in the stack of includes that we currently have 
// active.
typedef struct Include {
    SourceFile* sf; // The logical `source` that is used for this include.

    // TODO: will need to see if this changes when includes are actually 
    // TODO: implemented as I'm not sure what that implementation will actually
    // TODO: look like
    DirectoryEntry* search_path;

    Lexer lexer; // The controlling lexer for this include.

    ConditionalVector cond; // The current stack of conditionals. Note that any
                            // unhadnled conditionals are dealth with at the end
                            // of a specific include.
} Include;

vector_of_decl(Include, Include, include);

Conditional conditional_create(Location location, bool taken);
Location conditional_location(const Conditional* conditional);
bool conditional_taken(const Conditional* conditional);
bool conditional_had_else(const Conditional* conditional);

void conditional_set_taken(Conditional* conditional);
void conditional_set_else(Conditional* conditional);

// General include handling functions.
Include include_create(DiagnosticManager* dm, LangOptions* opts,
        IdentifierTable* names, SourceFile* source, DirectoryEntry* entry);
void include_delete(Include* include);

SourceFile* include_get_source(Include* include);
DirectoryEntry* include_get_search_path(Include* include);
Lexer* include_get_lexer(Include* include);
ConditionalVector* include_get_conditionals(Include* include);

bool include_get_next(Include* include, Token* token);
bool include_peek_next(Include* include, Token* token);
void include_skip_to_end_of_line(Include* include);

// Functions for pushing, popping, and general handling of conditionals
// TODO: these will remain unimplemented.
void include_push_conditional(Include* include, Location location, bool taken);
Location include_pop_conditional(Include* include);
bool include_conditional_empty(const Include* include);
Conditional* include_get_current_conditional(Include* include);

#endif /* INCLUDE_STACK_H */
