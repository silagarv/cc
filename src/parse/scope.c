#include "scope.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "parse/ast_allocator.h"
#include "util/ptr_set.h"

#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/symbol.h"

bool scope_is(const Scope* scope, ScopeFlags flags)
{
    if (scope == NULL)
    {
        return false;
    }

    return (scope->flags & flags) != 0;
}

Scope scope_extern(AstAllocator* allocator)
{
    // extern scope should only have the normal namespcae
    Scope scope = (Scope)
    {
        .flags = SCOPE_EXTERN,
        .ordinairy = symbol_table_create(),
        .tag = {0},
        .members = {0},
        .has_ordinairy = true,
        .has_tag = false,
        .has_members = false,
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_file(AstAllocator* allocator)
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
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_block(AstAllocator* allocator)
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
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_function_prototype(AstAllocator* allocator)
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
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_member(AstAllocator* allocator)
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
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_if(AstAllocator* allocator)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_IF,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_for(AstAllocator* allocator)
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
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_while(AstAllocator* allocator)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_WHILE,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_do_while(AstAllocator* allocator)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_DO_WHILE,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
        .all_decls = declaration_list(allocator)
    };

    return scope;
}

Scope scope_switch(AstAllocator* allocator)
{
    Scope scope = (Scope)
    {
        .flags = SCOPE_SWITCH,
        .parent = NULL,
        .ordinairy = symbol_table_create(),
        .tag = symbol_table_create(),
        .members = {0},
        .has_ordinairy = true,
        .has_tag = true,
        .has_members = false,
        .all_decls = declaration_list(allocator)
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
    assert(!scope_is(parent, SCOPE_EXTERN)); // No pushing on extern scope!
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
    static const ScopeFlags decl_scopes[] = {SCOPE_EXTERN, SCOPE_FILE,
            SCOPE_BLOCK, SCOPE_FUNCTION, SCOPE_IF, SCOPE_FOR, SCOPE_WHILE,
            SCOPE_DO_WHILE, SCOPE_SWITCH};
    static const size_t num_scopes = 
            sizeof(decl_scopes) / sizeof(decl_scopes[0]);

    return scope_allows_common(scope, decl_scopes, num_scopes);
}

static bool scope_allows_member(const Scope* scope)
{
    static const ScopeFlags decl_scopes[] = {SCOPE_MEMBER};

    return scope_allows_common(scope, decl_scopes, 1);
}

static bool scope_allows_break(const Scope* scope)
{
    static const ScopeFlags break_scopes[] = {SCOPE_FOR, SCOPE_WHILE,
            SCOPE_DO_WHILE, SCOPE_SWITCH};
    static const size_t num_scopes =
            sizeof(break_scopes) / sizeof(break_scopes[0]);

    return scope_allows_common(scope, break_scopes, num_scopes);
}

static bool scope_allows_continue(const Scope* scope)
{
    static const ScopeFlags continue_scopes[] = {SCOPE_FOR, SCOPE_WHILE,
            SCOPE_DO_WHILE};
    static const size_t num_scopes = 
            sizeof(continue_scopes) / sizeof(continue_scopes[0]);

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

static Scope* scope_get_next_member_scope(Scope* scope)
{
    return scope_get_next_allowed_scope(scope, scope_allows_member);
}

static Scope* scope_get_member_scope(Scope* scope)
{
    return scope_get_allowed_scope(scope, scope_allows_member,
            scope_get_next_member_scope);
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

Declaration* scope_lookup_member(Scope* scope, Identifier* name)
{
    return symbol_table_lookup(&scope->members, name);
}

void scope_insert_ordinairy(Scope* scope, Declaration* declaration)
{
    Scope* victim = scope_get_declaration_scope(scope);

    assert(!scope_lookup_ordinairy(victim, declaration->base.identifier,
            false));
    symbol_table_insert(&victim->ordinairy, declaration);
    scope_add_declaration(victim, declaration);
}

void scope_insert_tag(Scope* scope, Declaration* declaration)
{
    Scope* victim = scope_get_declaration_scope(scope);

    assert(!scope_lookup_tag(victim, declaration->base.identifier, false));
    symbol_table_insert(&victim->tag, declaration);
    scope_add_declaration(victim, declaration);
}

void scope_insert_member(Scope* scope, Declaration* declaration)
{
    Scope* victim = scope_get_member_scope(scope);
    assert(scope_is(scope, SCOPE_MEMBER));

    assert(!scope_lookup_member(victim, declaration->base.identifier));

    symbol_table_insert(&victim->members, declaration);
    scope_add_declaration(victim, declaration);
}

DeclarationList scope_get_declarations(const Scope* scope)
{
    return scope->all_decls;
}

void scope_add_declaration(Scope* scope, Declaration* decl)
{
    declaration_list_push(&scope->all_decls, decl);
}

// Function scope functions.

FunctionScope function_scope_create(Declaration* function)
{
    assert(declaration_is(function, DECLARATION_FUNCTION));

    FunctionScope scope = (FunctionScope)
    {
        .function = function,
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

Declaration* function_scope_get_function(const FunctionScope* scope)
{
    return scope->function;
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
