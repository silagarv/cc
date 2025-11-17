#include "scope.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "util/ptr_set.h"

#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/symbol.h"

bool scope_is(const Scope* scope, ScopeFlags flags)
{
    return (scope->flags & flags) != 0;
}

Scope scope_new_file(void)
{
    // File scope should only have the normal namespcae and the tag namespace.
    Scope scope = (Scope)
    {
        .flags = SCOPE_FILE,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
    };

    return scope;
}

// TODO: should scopes inherit the previous scopes flags. I think it might be
// dependant on the type of scope that we have but definitely something to
// think about. e.g. additional block scopes within functions should not
// inherit the function flag for labels. But scopes should inherit for example
// switch and that kind of thing.
Scope scope_new_block(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_BLOCK,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
    };

    return scope;
}

Scope scope_new_function_prototype(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_FUNCTION,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
    };

    return scope;
}

Scope scope_new_member(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_MEMBER,
        .parent = NULL,
        .ordinairy = {0},
        .tag = {0},
        .members = symbol_table_create(),
        .has_ordinairy = false,
        .has_tag = false,
        .has_members = true,
    };

    return scope;
}

Scope scope_new_for(void)
{
     Scope scope = (Scope)
    {
        .flags = SCOPE_FOR,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
    };

    return scope;
}

Scope scope_new_while(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_WHILE,
        .parent = NULL,
        .ordinairy = {0},
        .tag = {0},
        .members = {0},
        .has_ordinairy = false,
        .has_tag = false,
        .has_members = false,
    };

    return scope;
}

Scope scope_new_do_while(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_DO_WHILE,
        .parent = NULL,
        .ordinairy = {0},
        .tag = {0},
        .members = {0},
        .has_ordinairy = false,
        .has_tag = false,
        .has_members = false,
    };

    return scope;
}

Scope scope_new_switch(void)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_SWITCH,
        .parent = NULL,
        .ordinairy = {0},
        .tag = {0},
        .members = {0},
        .has_ordinairy = false,
        .has_tag = false,
        .has_members = false,
    };

    return scope;
}

void scope_delete(Scope* scope)
{
    if (scope->has_ordinairy)
    {
        symbol_table_delete(&scope->ordinairy);
    }

    if (scope->has_tag)
    {
        symbol_table_delete(&scope->tag);
    }

    if (scope->has_members)
    {
        symbol_table_delete(&scope->members);
    }
}

void scope_set_parent(Scope* scope, Scope* parent)
{
    assert(scope->parent == NULL);

    scope->parent = parent;
}

Scope* scope_get_parent(Scope* scope)
{
    return scope->parent;
}

static bool scope_allows_common(const Scope* scope, const ScopeFlags* flags,
        size_t num_flags)
{
    for (size_t i = 0; i < num_flags; i++)
    {
        if (scope_is(scope, flags[i]))
        {
            return true;
        }
    }

    return false;
}

static bool scope_allows_declaration(const Scope* scope)
{
    static const ScopeFlags decl_scopes[] = {SCOPE_FILE, SCOPE_FUNCTION,
            SCOPE_BLOCK, SCOPE_FOR};
    static const size_t num_scopes = sizeof(decl_scopes) / 
            sizeof(decl_scopes[0]);

    return scope_allows_common(scope, decl_scopes, num_scopes);
}

static bool scope_allows_break(const Scope* scope)
{
    static const ScopeFlags break_scopes[] = {SCOPE_FOR, SCOPE_WHILE,
            SCOPE_DO_WHILE, SCOPE_SWITCH};
    static const size_t num_scopes = sizeof(break_scopes) / 
            sizeof(break_scopes[0]);

    return scope_allows_common(scope, break_scopes, num_scopes);
}

static bool scope_allows_continue(const Scope* scope)
{
    static const ScopeFlags continue_scopes[] = {SCOPE_FOR, SCOPE_WHILE,
            SCOPE_DO_WHILE};
    static const size_t num_scopes = sizeof(continue_scopes) / 
            sizeof(continue_scopes[0]);

    return scope_allows_common(scope, continue_scopes, num_scopes);
}

static Scope* scope_get_allowed_scope(Scope* scope,
        bool scope_allows_function(const Scope* scope),
        Scope* scope_get_next_allowed_function(Scope* scope))
{
    if (scope_allows_function(scope))
    {
        return scope;
    }

    return scope_get_next_allowed_function(scope);
}

static Scope* scope_get_next_allowed_scope(Scope* scope,
        bool scope_allows_function(const Scope* scope))
{
    Scope* current = scope_get_parent(scope);
    
    while (current != NULL)
    {
        if (scope_allows_function(current))
        {
            return current;
        }

        current = scope_get_parent(current);
    }

    return NULL;
}

static Scope* scope_get_next_declaration_scope(Scope* scope)
{
    return scope_get_next_allowed_scope(scope, scope_allows_declaration);
}

static Scope* scope_get_declaration_scope(Scope* scope)
{
    return scope_get_allowed_scope(scope, scope_allows_declaration,
            scope_get_next_declaration_scope);
}

static Scope* scope_get_next_break_scope(Scope* scope)
{
    return scope_get_next_allowed_scope(scope, scope_allows_break);
}

static Scope* scope_get_break_scope(Scope* scope)
{
    return scope_get_allowed_scope(scope, scope_allows_break,
            scope_get_next_break_scope);
}

static Scope* scope_get_next_continue_scope(Scope* scope)
{
    return scope_get_next_allowed_scope(scope, scope_allows_continue);
}

static Scope* scope_get_continue_scope(Scope* scope)
{
    return scope_get_allowed_scope(scope, scope_allows_continue,
            scope_get_next_continue_scope);
}

bool scope_declaration_allowed(Scope* scope)
{
    Scope* decl_scope = scope_get_declaration_scope(scope);

    return decl_scope != NULL;
}

bool scope_break_allowed(Scope* scope)
{
    Scope* break_scope = scope_get_break_scope(scope);

    return break_scope != NULL;
}

bool scope_continue_allowed(Scope* scope)
{
    Scope* continue_scope = scope_get_continue_scope(scope);

    return continue_scope != NULL;
}

Scope* scope_get_scope_type(Scope* scope, ScopeFlags flags)
{
    Scope* current = scope;
    assert(current != NULL);
    do
    {
        if (scope_is(current, flags))
        {
            return current;
        }

        current = scope_get_parent(current);
    }
    while (current != NULL);
    
    return NULL;
}

Scope* scope_get_switch(Scope* scope)
{
    return scope_get_scope_type(scope, SCOPE_SWITCH);
}

Scope* scope_get_break(Scope* scope)
{
    ScopeFlags breakable = SCOPE_FOR 
        | SCOPE_WHILE 
        | SCOPE_DO_WHILE 
        | SCOPE_SWITCH;

    return scope_get_scope_type(scope, breakable);
}

Scope* scope_get_continue(Scope* scope)
{
    ScopeFlags continuable = SCOPE_FOR 
        | SCOPE_WHILE 
        | SCOPE_DO_WHILE;

    return scope_get_scope_type(scope, continuable);
}

Declaration* scope_lookup_ordinairy(Scope* scope, Identifier* name,
        bool recursive)
{
    Scope* current = scope_get_declaration_scope(scope);

    // First iteration should never be null even on the top file scope. This is
    // since, the file scope can contain declarations so we should always have
    // an initial declaration scope
    assert(current != NULL);

    while (current != NULL)
    {
        Declaration* symbol = symbol_table_lookup(&current->ordinairy, name);
        if (symbol)
        {
            return symbol;
        }

        if (!recursive)
        {
            break;
        }

        current = scope_get_next_declaration_scope(current);
    }

    return NULL;
}

Declaration* scope_lookup_tag(Scope* scope, Identifier* name,
        bool recursive)
{
    Scope* current = scope_get_declaration_scope(scope);

    while (current != NULL)
    {
        Declaration* symbol = symbol_table_lookup(&current->tag, name);
        if (symbol)
        {
            return symbol;
        }

        if (!recursive)
        {
            break;
        }

        current = scope_get_next_declaration_scope(current);
    }

    return NULL;
}

void scope_insert_ordinairy(Scope* scope, Declaration* declaration)
{
    Scope* victim = scope_get_declaration_scope(scope);

    assert(!scope_lookup_ordinairy(victim, declaration->base.identifier,
            false));

    symbol_table_insert(&victim->ordinairy, declaration);
}

void scope_insert_tag(Scope* scope, Declaration* declaration)
{
    Scope* victim = scope_get_declaration_scope(scope);

    assert(!scope_lookup_tag(victim, declaration->base.identifier, false));

    symbol_table_insert(&victim->tag, declaration);
}

Declaration* scope_lookup_member(Scope* scope, Identifier* name)
{
    return symbol_table_lookup(&scope->members, name);
}

// Function scope functions.

FunctionScope function_scope_create(void)
{
    FunctionScope scope = (FunctionScope)
    {
        .label_declarations = symbol_table_create(),
        .used_label_idents = pointer_set_create(),
        .used_labels = declaration_vector_create(1)
    };

    return scope;
}

void function_scope_delete(FunctionScope* scope)
{
    symbol_table_delete(&scope->label_declarations);
    pointer_set_delete(&scope->used_label_idents);
    declaration_vector_free(&scope->used_labels, NULL);
}

// Look up a label in the function scope returning labels which have been
// implicitly created.
Declaration* function_scope_lookup(FunctionScope* scope, Identifier* ident,
        bool allow_implicit)
{
    Declaration* decl = symbol_table_lookup(&scope->label_declarations, ident);

    // If we're not allowed implicit but it is return null. Otherwise we are
    // free to return the label even if it is implicit (no issues)
    if (!allow_implicit && decl->base.implicit)
    {
        return NULL;
    }

    // Otherwise return the declaration (even if null)
    return decl;
}

// Insert a label in the function scope additing it to our seen labels
void function_scope_insert(FunctionScope* scope, Declaration* label)
{
    symbol_table_insert(&scope->label_declarations, label);

    // If we have got the label already we are good to leave this here.
    if (pointer_set_contains(&scope->used_label_idents, label->base.identifier))
    {
        return;
    }

    // Otherwise add the identifier to the label set and the declaration to
    // our vector. This avoid duplicate warnings of undeclared labels
    pointer_set_insert(&scope->used_label_idents, label->base.identifier);
    declaration_vector_push(&scope->used_labels, label);
}
