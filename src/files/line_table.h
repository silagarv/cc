#ifndef LINE_TABLE_H
#define LINE_TABLE_H

#include <stdint.h>

#include "files/filepath.h"
#include "util/vec.h"
#include "util/hash_map.h"

#include "files/location.h"

// Structure to hold all of new file names that we have been given in the #line
// directives. This keeps track of all of the new filenames so that we can save
// memory and make sure we aren't storing alot of duplicate filenames all over
// the place.

typedef uint32_t LineOverrideFileId;

#define LINE_OVERRIDE_FILE_ID_INVALID ((LineOverrideFileId) 0)

// A map of all of the different filenames that we might have. This is meant to
// be used in the source manager to help avoid any duplication of #line 
// directive filesnames. This is fine since this should only be used by the 
// source manager.
typedef struct LineOverrideFilenames {
    LineOverrideFileId next_id; // the next id we are going to give out
    HashMap filenames; // the map of filenames to id's
} LineOverrideFilenames;

// A structure representing a line entry for a #line directive. And any other
// information that we might need to asociate with it.
typedef struct LineOverride {
    // The starting location of the line override. This is the exact location
    // where the new line number takes place. This should occur at the position
    // directly after the newline character. The only exception to this is if
    // the file does not contain any more lines. In that case it would just be
    // invalid location.
    Location location;

    // This is the line number given for the file at the start of the next line.
    uint32_t line_no;

    // The id for the filepath given in the line override. If not filepath has
    // been specified so far this is given the invalid file id. If not specified
    // for this override then it defaults to the previous file id in order to
    // increase lookup speed, i.e. we don't have to search back through the
    // whole vector to find the most recent one.
    LineOverrideFileId id;
} LineOverride;

vector_of_decl(LineOverride, LineOverride, line_override);

// The structure we will use in order to store our line overrides.
typedef struct LineOverrideTable {
    LineOverrideVector overrides; // a list of all the overrides we have
} LineOverrideTable;

// Check if the id given to a file it is valid.
bool line_override_file_id_is_valid(LineOverrideFileId id);

// Create a strcture to store all of the line override filenames
LineOverrideFilenames line_override_filenames_create(void);

// Delete a store of line override filenames
void line_override_filenames_free(LineOverrideFilenames* lofs);

// Get the id asociated with a filepath inserting it if not already in the table
// otherwise simply getting the id asociated with it.
LineOverrideFileId line_override_filenames_get(LineOverrideFilenames* lofs,
        Filepath path);

// Create a new line override
LineOverride line_override_create(Location loc, uint32_t line_no, 
        LineOverrideFileId file);

// Create a line override table
LineOverrideTable line_override_table_create(void);

// Delete a line override table
void line_override_table_delete(LineOverrideTable* table);

void line_override_table_add_path(Location loc, uint32_t line_no, Filepath* path);
void line_override_table_add_no_path(Location loc, uint32_t line_no);

// Get the line override from the table that we are looking for.
LineOverride line_override_table_lookup(const LineOverrideTable* table, Location loc);

#endif /* LINE_TABLE_H */
