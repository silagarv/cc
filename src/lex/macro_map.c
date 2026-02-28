#include "macro_map.h"

#include <assert.h>
#include <stdint.h>

#include "driver/warning.h"
#include "util/arena.h"
#include "util/hash_map.h"

#include "driver/diagnostic.h"

#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/macro.h"

typedef enum MacroStateType {
    MACRO_STATE_DEFINE,
    MACRO_STATE_UNDEFINE
} MacroStateType;

typedef struct MacroState {
    MacroStateType type; // If this is a define or undefine.

    // A union holding either a define or an undefine operation. if type is a
    // define then the define field will be active, otherwise the undefine field
    // is the active one.
    union {
        Macro* define;
        Location undef;
    } state;

    struct MacroState* prev; // The previous macro state.
} MacroState;

typedef struct MacroMapEntry {
    Identifier* name; // the identifier this entry corrosponds too 
    MacroState* state; // the currently active state
} MacroMapEntry;

static MacroState* macro_state_create_define(Arena* allocator, MacroState* prev,
        Macro* macro)
{
    MacroState* state = arena_malloc(allocator, sizeof(MacroState));
    *state = (MacroState)
    {
        .type = MACRO_STATE_DEFINE,
        .state.define = macro,
        .prev = prev
    };

    return state;
}

static MacroState* macro_state_create_undefine(Arena* allocator,
        MacroState* prev, Location location)
{
    MacroState* state = arena_malloc(allocator, sizeof(MacroState));
    *state = (MacroState)
    {
        .type = MACRO_STATE_UNDEFINE,
        .state.undef = location,
        .prev = prev
    };

    return state;
}

static bool macro_state_is_defined(const MacroState* state)
{
    if (state == NULL)
    {
        return false;
    }

    return state->type == MACRO_STATE_DEFINE;
}

static Macro* macro_state_get_macro(const MacroState* state)
{
    assert(macro_state_is_defined(state) && "not a define!");
    return state->state.define;
}

static Location macro_state_get_location(const MacroState* state)
{
    assert(!macro_state_is_defined(state) && "actually a define!");
    return state->state.undef;
}

static MacroState* macro_state_get_previous(const MacroState* state)
{
    assert(state != NULL && "require a macro state");
    return state->prev;
}

static MacroMapEntry* macro_map_entry_create(Arena* allocator, Identifier* name)
{
    MacroMapEntry* entry = arena_malloc(allocator, sizeof(MacroMapEntry));
    *entry = (MacroMapEntry)
    {
        .name = name,
        .state = NULL
    };

    return entry;
}

static void macro_map_entry_push_define(MacroMapEntry* entry, Arena* allocator,
        Macro* macro)
{
    assert(entry != NULL && "bad entry");
    entry->state = macro_state_create_define(allocator, entry->state, macro);
}

static void macro_map_entry_push_undef(MacroMapEntry* entry, Arena* allocator,
        Location location)
{
    assert(entry != NULL && "bad entry");
    entry->state = macro_state_create_undefine(allocator, entry->state,
            location);
}

static Identifier* macro_map_entry_get_identifier(const MacroMapEntry* entry)
{
    assert(entry != NULL && "bad entry");
    return entry->name;
}

static MacroState* macro_map_entry_get_state(const MacroMapEntry* entry)
{
    assert(entry != NULL && "bad entry");
    return entry->state;
}

static bool macro_map_entry_cmp(const void* key1, const void* key2)
{
    return key1 == key2;
}

MacroMap macro_map_create(void)
{
    MacroMap map = (MacroMap)
    {
        .allocator = arena_new_default(),
        .map = hash_map_create(identifier_get_hash, macro_map_entry_cmp, NULL)
    };

    return map;
}

void macro_map_delete(MacroMap* macros)
{
    hash_map_delete(&macros->map);
    arena_delete(&macros->allocator);
}

Arena* macro_map_allocator(MacroMap* macros)
{
    return &macros->allocator;
}

static void macro_map_insert_entry(MacroMap* macros, MacroMapEntry* entry)
{
    assert(!hash_map_contains(&macros->map,
            macro_map_entry_get_identifier(entry)) && "inserting but contains");
    
    Identifier* id = macro_map_entry_get_identifier(entry);
    hash_map_insert(&macros->map, id, entry);
}

static MacroMapEntry* macro_map_get_entry(MacroMap* macros, Identifier* id)
{
    return hash_map_get(&macros->map, id);
}

Macro* macro_map_get_macro(MacroMap* macros, Identifier* identifier)
{
    // If we don't get a macro map entry return NULl this was never defined as
    // a macro.
    MacroMapEntry* entry = macro_map_get_entry(macros, identifier);
    if (entry == NULL)
    {
        return NULL;
    }
    
    MacroState* state = macro_map_entry_get_state(entry);
    assert(state != NULL && "have entry but no state?");

    // If it's not currently defined return no macro.
    if (!macro_state_is_defined(state))
    {
        return NULL;
    }

    // Otherwise get the macro from the state
    return macro_state_get_macro(state);
}

static void macro_map_add_new_entry(MacroMap* macros, Macro* macro)
{
    Identifier* name = macro_name(macro);
    MacroMapEntry* entry = macro_map_entry_create(macro_map_allocator(macros),
            name);

    macro_map_entry_push_define(entry, macro_map_allocator(macros), macro);

    // Finally, actually insert the define into the macro map
    hash_map_insert(&macros->map, name, entry);
}

static void macro_map_do_redefine(MacroMap* macros, MacroMapEntry* entry,
        Macro* macro)
{
    // Simply push the define onto the stack of current information.
    macro_map_entry_push_define(entry, macro_map_allocator(macros), macro);
}

void macro_map_do_define(MacroMap* macros, DiagnosticManager* dm, Macro* macro)
{
    // First get the macro map entry from the map
    Identifier* name = macro_name(macro);
    MacroMapEntry* entry = macro_map_get_entry(macros, name);
    
    // This is the first time defining the macro, so we can just go and add a 
    // new entry in
    if (entry == NULL)
    {
        macro_map_add_new_entry(macros, macro);
        return;
    }

    // For now just warn if it is actually a redefinition. This will need to be
    // imprtoved for later
    MacroState* state = macro_map_entry_get_state(entry);
    if (macro_state_is_defined(state))
    {
        Macro* old_macro = macro_state_get_macro(state);
        if (!macro_definitions_equal(macro, old_macro))
        {
            if (macro_builtin(old_macro))
            {
                diagnostic_warning_at(dm, macro_location(macro),
                        Wbuiltin_macro_redefined, "redefining builtin macro");
            }
            else
            {
                diagnostic_warning_at(dm, macro_location(macro),
                        Wmacro_redefined, "'%s' macro redefinied",
                        identifier_cstr(macro_name(macro)));
            }
        }
    }

    // Otherwise we are possibly redefining a current macro. Get the state from
    // the entry.
    macro_map_do_redefine(macros, entry, macro);
}

void macro_map_do_undefine(MacroMap* macros, DiagnosticManager* dm,
        Identifier* name, Location location)
{
    // First get the macro map entry from the map
    MacroMapEntry* entry = macro_map_get_entry(macros, name);
    
    // If this macro was never defined do nothing.
    if (entry == NULL)
    {
        return;
    }

    // Otherwise get the state and if we previously had an undefinition anyways
    // just do nothing again.
    MacroState* state = macro_map_entry_get_state(entry);
    if (!macro_state_is_defined(state))
    {
        return;
    }

    // Finally get the old macro and determine if it was a builtin or not
    Macro* old_macro = macro_state_get_macro(state);
    if (macro_builtin(old_macro))
    {
        diagnostic_warning_at(dm, location, Wbuiltin_macro_redefined,
                "undefining builtin macro");
    }

    // Finally actually push the undefinition onto our state
    macro_map_entry_push_undef(entry, macro_map_allocator(macros), location);
}
