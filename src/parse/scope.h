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
    SCOPE_FILE = 1 << 0, // File scope?
    SCOPE_BLOCK = 1 << 1, // Block scope
    SCOPE_FUNCTION = 1 << 2, // Function declaration
    SCOPE_MEMBER = 1 << 3, // structs / unions
    SCOPE_FOR = 1 << 4, // for loop statements (AND declarations)
    SCOPE_WHILE = 1 << 5, // while loop statments
    SCOPE_DO_WHILE = 1 << 6, // do/while loop statements
    SCOPE_SWITCH = 1 << 7, // switch scope
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

    // Which symbol tables are active and need to be destroyed when popping the
    // scope
    bool has_ordinairy;
    bool has_tag;
    bool has_members;

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

Scope scope_new_file(void);
Scope scope_new_block(void);
Scope scope_new_function_prototype(void);
Scope scope_new_member(void);
Scope scope_new_for(void);
Scope scope_new_while(void);
Scope scope_new_do_while(void);
Scope scope_new_switch(void);

void scope_delete(Scope* scope);

void scope_set_parent(Scope* scope, Scope* parent);
Scope* scope_get_parent(Scope* scope);

bool scope_declaration_allowed(Scope* scope);
bool scope_break_allowed(Scope* scope);
bool scope_continue_allowed(Scope* scope);

Scope* scope_get_scope_type(Scope* scope, ScopeFlags flags);

Scope* scope_get_switch(Scope* scope);
Scope* scope_get_break(Scope* scope);
Scope* scope_get_continue(Scope* scope);

Declaration* scope_lookup_ordinairy(Scope* scope, Identifier* name,
        bool recursive);
Declaration* scope_lookup_tag(Scope* scope, Identifier* name,
        bool recursive);
Declaration* scope_lookup_member(Scope* scope, Identifier* name);

void scope_insert_ordinairy(Scope* scope, Declaration* declaration);
void scope_insert_tag(Scope* scope, Declaration* declaration);

// Should be changed to type + recursive
bool scope_contains(const Scope* scope, Identifier* name);
bool scope_add_symbol(const Scope* scope, Declaration* declaration);

// Function scopes are specially for function labels
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
