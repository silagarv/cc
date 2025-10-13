#include "scope.h"

#include <string.h>
#include <assert.h>

#include "util/ptr_set.h"

#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/symbol.h"

bool scope_is(const Scope* scope, ScopeFlags flags)
{
    return scope->flags & flags;
}

Scope scope_new_file(void)
{
    // File scope should only have the normal namespcae and the tag namespace.
    Scope scope = (Scope)
    {
        .flags = SCOPE_DECL,
        .ordinairy = {0},
        .tag = {0},
        .members = {0},
        .parent = NULL
    };

    return scope;
}

// TODO: should scopes inherit the previous scopes flags. I think it might be
// dependant on the type of scope that we have but definitely something to
// think about. e.g. additional block scopes within functions should not
// inherit the function flag for labels. But scopes should inherit for example
// switch and that kind of thing.
Scope scope_new_block(Scope* previous)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_DECL,
        .parent = previous
    };

    return scope;
}

void scope_delete(Scope* scope)
{
    // TODO: work this out...
}

void scope_set_parent(Scope* scope, Scope* parent)
{
    assert(scope->parent == NULL && "scope should not have a parent");

    scope->parent = parent;
}

Scope* scope_get_parent(Scope* scope)
{
    return scope->parent;
}

Declaration* scope_lookup_ordinairy(Scope* scope, Identifier* name,
        bool recursive)
{
    for (Scope* current = scope; current; scope = current->parent)
    {
        Declaration* symbol = symbol_table_lookup(&scope->ordinairy, name);
        if (symbol)
        {
            return symbol;
        }

        if (!recursive)
        {
            break;
        }
    }

    return NULL;
}

Declaration* scope_lookup_tag(Scope* scope, Identifier* name,
        bool recursive)
{
    for (Scope* current = scope; current; scope = current->parent)
    {
        Declaration* symbol = symbol_table_lookup(&scope->tag, name);
        if (symbol)
        {
            return symbol;
        }

        if (!recursive)
        {
            break;
        }
    }

    return NULL;
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
