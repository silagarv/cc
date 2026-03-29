#ifndef LINE_TABLE_H
#define LINE_TABLE_H

#include <stdint.h>

#include "files/filepath.h"
#include "util/vec.h"
#include "util/hash_map.h"

#include "files/location.h"

#define LINE_OVERRIDE_FILE_ID_INVALID ((LineOverrideFileId) 0)

typedef uint32_t LineOverrideFileId;
typedef struct LineOverrideFilename LineOverrideFilename;

vector_of_decl(LineOverrideFilename*, LineOverrideFilename,
        line_override_filename);

// A map of all of the different filenames that we might have. This is meant to
// be used in the source manager to help avoid any duplication of #line 
// directive filesnames. This is fine since this should only be used by the 
// source manager.
typedef struct LineOverrideFilenames {
    LineOverrideFileId next_id; // the next id we are going to give out
    HashMap filenames; // the map of filenames to id's
    LineOverrideFilenameVector names;
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

    // The ending location of the line override
    Location ending_loc;

    // This is the line number given for the file at the start of the next line.
    uint32_t line_no;

    // The id for the filepath given in the line override. If not filepath has
    // been specified so far this is given the invalid file id. If not specified
    // for this override then it defaults to the previous file id in order to
    // increase lookup speed, i.e. we don't have to search back through the
    // whole vector to find the most recent one.
    LineOverrideFileId id;

    // The real line of the line override
    uint32_t real_line;
} LineOverride;

vector_of_decl(LineOverride, LineOverride, line_override);

// Check if the id given to a file it is valid.
bool line_override_file_id_is_valid(LineOverrideFileId id);

// Create a strcture to store all of the line override filenames
LineOverrideFilenames line_override_filenames_create(void);

// Delete a store of line override filenames
void line_override_filenames_free(LineOverrideFilenames* lofs);

// Get the id asociated with a filepath inserting it if not already in the table
// otherwise simply getting the id asociated with it.
LineOverrideFileId line_override_filenames_get(LineOverrideFilenames* lofs, 
        const Filepath* path);
Filepath* line_override_filenames_from_id(LineOverrideFilenames* lofs,
        LineOverrideFileId id);

// Create a new line override
LineOverride line_override_create(Location loc, Location end, uint32_t line_no, 
        LineOverrideFileId file, uint32_t real_line);
Location line_override_location(const LineOverride* override);
uint32_t line_override_line_no(const LineOverride* override);
LineOverrideFileId line_override_file_id(const LineOverride* override);
uint32_t line_override_real_line(const LineOverride* override);

int line_override_compare(const Location* loc, const LineOverride* override);

#endif /* LINE_TABLE_H */
