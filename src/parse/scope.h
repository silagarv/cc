#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>

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
    size_t level;

    
} Scope;

Scope* scope_new(Scope* parent);
void scope_free(Scope* scope);

Symbol* scope_lookup(Scope* scope, String* name);

bool scope_contains(Scope* scope, String* name);

bool scope_add_symbol(Scope* scope, Symbol* symbol);

#endif /* SCOPE_H */
