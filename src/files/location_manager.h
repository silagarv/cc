#ifndef LOCATION_MANAGER_H
#define LOCATION_MANAGER_H

#include <stddef.h>
#include <stdbool.h>

#include "files/location.h"
#include "files/source_manager.h"
#include "files/line_map.h"

typedef struct SourceFileEntry {
    SourceFile* file; // the file this represents
    LineMap map; // Map from location to triplet
} SourceFileEntry;

// A structure to represent a Macro map. (not yet complete)
typedef struct MacroMapEntry {
    Location start_location;
} MacroMapEntry;

// A structure to represent a location range of either a source file or of a
// macro map (although this second part is not yet implemented)
typedef struct LocationEntry {
    bool is_file; // if we have a file or a macro map
    Location starting_location; // the starting location of the location entry
    
    // A union to represent a location entry type either being a source file
    // or a macro map. This union can always be discriminated by is_file
    union LocationEntryType {
        SourceFileEntry source_file;
        MacroMapEntry macro_map;
    } type;
} LocationEntry;

// A structure to manage different location offset and provide us with a way
// to go from locations to text / files. We also want to be able to accurately
// track macro expansion and got back from these expansions back to the original
// source text in order to be able to produce as helpful diagnostics as possible
// for the user of the compiler
typedef struct LocationManager {
    LocationEntry* location_offsets; // 
    size_t num_offsets;
    size_t cap_offsets;
} LocationManager;

#endif /* LOCATION_MANAGER */
