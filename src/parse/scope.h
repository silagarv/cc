#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "parse/declaration.h"
#include "util/hash_map.h"
#include "util/ptr_set.h"
#include "util/str.h"

#include "lex/identifier_table.h"

#include "parse/symbol.h"

// TODO: below is probably not needed...
typedef enum ScopeFlags {
    SCOPE_DECL = 1 << 0,
    SCOPE_BREAK = 1 << 1,
    SCOPE_CONTINUE = 1 << 2,
    SCOPE_SWITCH = 1 << 3,
    SCOPE_FUNCTION = 1 << 4,
    SCOPE_MEMBER = 1 << 5, // Note this is also for unions too (for the members)
} ScopeFlags;

// Add some sort of a symbol table in here
typedef struct Scope {
    // The flags determining what can do in this scope
    ScopeFlags flags;

    // All of the symbol tables within this scope. The specific scope should be
    // referenced by the relavent enumeration constant
    SymbolTable ordinairy;
    SymbolTable tag;
    SymbolTable members;

    // The direct parent scope of this one, or null if at file level
    struct Scope* parent;
} Scope;

// The way we will keep track of labels within functions.
typedef struct FunctionScope {
    SymbolTable label_declarations; // All labels even from goto's
    PtrSet used_label_idents; // the identifiers of all of our used labels
    DeclarationVector used_labels; // The labels we have actually used.
} FunctionScope;

bool scope_is(const Scope* scope, ScopeFlags flags);

// Create a new file scope this should only really need to be called once for
// each translation unit and calling this multiple times in each unit may lead
// to errors.
Scope scope_new_file(void);

Scope scope_new_block(Scope* parent);

// TODO: work out how this will work since we can have function prototypes both
// in file scope and in block scope... e.g. declared in a function as well!!!
Scope scope_new_function_prototype(Scope* parent_file);

// Create a new function scope, in order to do this we require that we have a
// parent block scope. This is since the standard requires that we have function
// definitions (not declarations) in a block scope. So we will need to create a
// block scope in the file before we create one of these scopes.
Scope scope_new_function(Scope* parent_file, Scope* parent_function_prototype,
        Scope* parent_block, Scope* parent_function);

void scope_delete(Scope* scope);

void scope_set_parent(Scope* scope, Scope* parent);
Scope* scope_get_parent(Scope* scope);

Declaration* scope_lookup_ordinairy(Scope* scope, Identifier* name,
        bool recursive);
Declaration* scope_lookup_tag(Scope* scope, Identifier* name,
        bool recursive);
Declaration* scope_lookup_member(Scope* scope, Identifier* name);

// Should be changed to type + recursive
bool scope_contains(const Scope* scope, Identifier* name);

bool scope_add_symbol(const Scope* scope, Declaration* declaration);

FunctionScope function_scope_create(void);
void function_scope_delete(FunctionScope* scope);

// Look up a label in the function scope returning labels which have been
// implicitly created.
Declaration* function_scope_lookup(FunctionScope* scope, Identifier* ident,
        bool allow_implicit);

// Insert a label in the function scope additing it to our actual labels if it
// was not implicitly created.
void function_scope_insert(FunctionScope* scope, Declaration* label);

#endif /* SCOPE_H */
