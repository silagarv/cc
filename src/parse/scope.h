#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>

#include "util/str.h"

#include "parse/symbol.h"

// Add some sort of a symbol table in here
struct Scope {
    SymbolTable symbols;

    struct Scope* parent;
};
typedef struct Scope Scope;

Scope* scope_new(Scope* parent);
void scope_free(Scope* scope);

Symbol* scope_lookup(Scope* scope, String* name);

bool scope_add_symbol(Scope* scope, Symbol* symbol);

#endif /* SCOPE_H */
