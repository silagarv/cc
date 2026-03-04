#ifndef MACRO_MAP_H
#define MACRO_MAP_H

#include "util/arena.h"
#include "util/hash_map.h"

#include "driver/diagnostic.h"

#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/macro.h"

// Our macro map struct which holds all of the information necessary for 
// handling macros. Makes sure that all defines and undefines are recorded for
// all of our macros.
typedef struct MacroMap {
    Arena allocator; // The allocator for the macro map. Holds all of our macro
                     // related allocations for us so we can delete our macro
                     // related information all at once.

    HashMap map; // The hashmap that stores the Identifier* to macro map entry
                 // states. and handles the finding of the current state of
                 // our macros.

    // TODO: add a macro vector so that we can print all of the macros used?
} MacroMap;

// Functions for creating and destroying our macro map.
MacroMap macro_map_create(void);
void macro_map_delete(MacroMap* macros);

// Get the allocator asociated with this macro map so that we can use it for 
// anyhing related to our macros.
Arena* macro_map_allocator(MacroMap* macros);

// Function to get the current active macro definition for an identifer
Macro* macro_map_get_macro(MacroMap* macros, Identifier* identifier);
bool macro_map_is_defined(MacroMap* macros, Identifier* identifier);

// Functions that do all the work or defining and undefining our macros for us.
// The need to take in a diagnostic manager so that we can handle any warnings
// or error that might have occured as a result of modifying the current macro
// definitions.
void macro_map_do_define(MacroMap* macros, DiagnosticManager* dm, Macro* macro);
void macro_map_do_undefine(MacroMap* macros, DiagnosticManager* dm,
        Identifier* name, Location location);

#endif /* MACRO_MAP_H */
