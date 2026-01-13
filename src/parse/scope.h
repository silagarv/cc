#ifndef SCOPE_H
#define SCOPE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "parse/ast_allocator.h"
#include "util/hash_map.h"
#include "util/ptr_set.h"
#include "util/str.h"

#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/symbol.h"

typedef enum ScopeFlags {
    SCOPE_EXTERN = 1 << 0, // Scope for externel declarations and defns
    SCOPE_FILE = 1 << 1, // File scope?
    SCOPE_BLOCK = 1 << 2, // Block scope
    SCOPE_FUNCTION = 1 << 3, // Function declaration
    SCOPE_MEMBER = 1 << 4, // structs / unions
    SCOPE_IF = 1 << 5, // for if statements
    SCOPE_FOR = 1 << 6, // for loop statements (AND declarations)
    SCOPE_WHILE = 1 << 7, // while loop statments
    SCOPE_DO_WHILE = 1 << 8, // do/while loop statements
    SCOPE_SWITCH = 1 << 9, // switch scope
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

    // Below we store a linked list of all of the declarations in this scope so
    // that we can have a list of all of the declarations that we could possibly
    // need. This helps us since we will will possibly need to later use this in
    // order to recontruct the parameter lists and any other declarations that
    // are allowed in functions.

    // TODO: lets change the below to a declaration list. So that we can then
    // TODO: use the delcaration link on declarations for chaining declarations
    // TODO: of the same type.

    // A list of all of the declarations in this scope.
    DeclarationList all_decls;
} Scope;

// The way we will keep track of labels within functions.
typedef struct FunctionScope {
    Declaration* function; // the function that we are parsing at the moment

    SymbolTable label_declarations; // All labels even from goto's
    PtrSet used_label_idents; // the identifiers of all of our used labels
    DeclarationVector used_labels; // The labels we have actually used.
} FunctionScope;

bool scope_is(const Scope* scope, ScopeFlags flags);

Scope scope(AstAllocator* allocator, ScopeFlags flags);
Scope scope_extern(AstAllocator* allocator);
Scope scope_file(AstAllocator* allocator);
Scope scope_block(AstAllocator* allocator);
Scope scope_function_prototype(AstAllocator* allocator);
Scope scope_member(AstAllocator* allocator);
Scope scope_if(AstAllocator* allocator);
Scope scope_for(AstAllocator* allocator);
Scope scope_while(AstAllocator* allocator);
Scope scope_do_while(AstAllocator* allocator);
Scope scope_switch(AstAllocator* allocator);

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
void scope_insert_member(Scope* scope, Declaration* declaration);

// get the declaration list from a scope
DeclarationList scope_get_declarations(const Scope* scope);
void scope_add_declaration(Scope* scope, Declaration* decl);

// Function scopes are specially for function labels
FunctionScope function_scope_create(Declaration* function);
void function_scope_delete(FunctionScope* scope);

Declaration* function_scope_get_function(const FunctionScope* scope);

// Look up a label in the function scope returning labels which have been
// implicitly created.
Declaration* function_scope_lookup(FunctionScope* scope, Identifier* ident,
        bool allow_implicit);

// Insert a label in the function scope additing it to our actual labels if it
// was not implicitly created.
void function_scope_insert(FunctionScope* scope, Declaration* label);

#endif /* SCOPE_H */
