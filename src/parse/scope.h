#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "parse/declaration.h"
#include "util/str.h"

#include "lex/identifier_table.h"

#include "parse/symbol.h"

// TODO: below is probably not needed...
typedef enum ScopeType {
    SCOPE_FUNCTION,
    SCOPE_FILE,
    SCOPE_BLOCK,
    SCOPE_FUNCTION_PROTOTYPE
} ScopeType;

// Add some sort of a symbol table in here
typedef struct Scope {
    // The type of scope this is.
    ScopeType type;

    // the current file scope, should be same for all scopes
    struct Scope* file;

    // The current function prototype scope
    struct Scope* function_prototype;

    // Pushed when a new block gets introduced.
    struct Scope* block;

    // the current function scope (used for labels)
    struct Scope* function;

    // The symbol table for this scope containing all of the different symbols
    // within the scope. 
    SymbolTable symbols;

    // The direct parent scope of this one, or null.
    struct Scope* parent;
} Scope;

// Create a new file scope this should only really need to be called once for
// each translation unit and calling this multiple times in each unit may lead
// to errors.
Scope scope_new_file(void);

// TODO: work out how this will work...
Scope scope_new_block(Scope* parent_file, Scope* parent_function_prototype,
        Scope* parent_block, Scope* parent_function);

// TODO: work out how this will work since we can have function prototypes both
// in file scope and in block scope... e.g. declared in a function as well!!!
Scope scope_new_function_prototype(Scope* parent_file, 
        Scope* parent_function_prototype, Scope* parent_block, 
        Scope* parent_function);

// Create a new function scope, in order to do this we require that we have a
// parent block scope. This is since the standard requires that we have function
// definitions (not declarations) in a block scope. So we will need to create a
// block scope in the file before we create one of these scopes.
Scope scope_new_function(Scope* parent_file, Scope* parent_function_prototype,
        Scope* parent_block, Scope* parent_function);

Declaration* scope_lookup(const Scope* scope, Identifier* name);

bool scope_contains(const Scope* scope, Identifier* name);

bool scope_add_symbol(const Scope* scope, Declaration* declaration);

#endif /* SCOPE_H */
