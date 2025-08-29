#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "util/str.h"

#include "parse/symbol.h"

/* Scope:
 * 4 kinds of scopes: function, file, block, and function prototype
 *
 * function: only a label name is in here
 * everything else is determined by the placement of the identifier
 */

// TODO: below is probably not needed...
typedef enum ScopeType {
    SCOPE_ERROR,
    SCOPE_FUNCTION,
    SCOPE_FILE,
    SCOPE_BLOCK,
    SCOPE_FUNCTION_PROTOTYPE
} ScopeType;

// Add some sort of a symbol table in here
typedef struct Scope {
    // The parent scope to this one or NULL if this the `parent` is simply the
    // translation unit itself. 
    struct Scope* parent;

    // We will need to have some information about the scope below
    uint32_t depth;

    // TODO: we will have to have a thing we're we can put things in this scope.
} Scope;

// Create a new scope available on the stack
Scope scope_new(Scope* parent);

// Free all of the memory within the scope
void scope_free(Scope* scope);

Symbol* scope_lookup(Scope* scope, String* name);

bool scope_contains(Scope* scope, String* name);

bool scope_add_symbol(Scope* scope, Symbol* symbol);

#endif /* SCOPE_H */
