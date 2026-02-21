#ifndef SOURCE_MANAGER_H
#define SOURCE_MANAGER_H

#include "util/buffer.h"
#include "util/vec.h"

#include "files/location.h"
#include "files/filepath.h"
#include "files/file_manager.h"
#include "files/line_map.h"
#include "files/line_table.h"
#include <stdint.h>

// TODO: make a line overrride map structure as well

typedef uint32_t SourceFileId;

// A structure representing a source that we are going to use in compilation
// this structure is made to be nice and lean and contains pointers to data that
// we are wanting to store elsewhere mainly
typedef struct SourceFile {
    SourceFileId id; // the id of the source file we got
    Location start_location; // the location that we consider the start of file
    Location end_location; // the ending location of this source file.
    Location included_location; // where the file was included from (if needed)
    FileBuffer* file_buffer; // the underlying file that we have
    LineMap line_map; // The line map for this source file
    LineOverrideTable line_overrides; // The override table
} SourceFile;

// Create a vector of source file pointers since the id's will always be 
// sequential and we can easily index by the id.
vector_of_decl(SourceFile*, SourceFile, source_file);

// TODO: how are we planning on tracking macro invocations??? Like we could have
// tokens point to the macro function for now, or we could have some kind of 
// structure in source manager?

// A macro entry struct is used so that we are able to track macro invocations
// as they happen within the source. This is used to help give accurate and
// useful diagnostics when tokens have been expanded from macros.
typedef struct MacroEntry {
    // This location given is the actual id we will refer to this macro entry by
    // Note that this location will always return true for location_is_macro
    // so that we are able to give each token a macro entry
    Location location;

    // The location of the macro in the source code. For object like macros this
    // location is the same. But for function like macros the start is the start
    // of the macro name and the end is location of the closing paren ')' for
    // the macro invocation. Both of these locations are source locations.
    Location macro_start_location;
    Location macro_end_location;
} MacroEntry;

vector_of_decl(MacroEntry, MacroEntry, macro_entry);

// A structure to hold all of the needed data to retrieve and store files for
// later use. This is made so that we can hold any and all information about files
// locations and source of data (even if they are not physical files on disk)
typedef struct SourceManager {
    // The file manager for us to be able to keep track of all of the different
    // files within the compilation unit and so that we can track everything.
    FileManager fm;

    // The next location we are going to give out to the source_file. This is
    // something that the source manager tracks internally in order to give
    // valid locations to source files. Note that the will never be a macro
    // location and it will always be considered valid
    Location next_source_location;

    // Used to keep track of the next SourceFileId that we are going to assign
    SourceFileId next_id;

    // TODO: we would like to add a vector of all of our source_files so that
    // we can easily keep track of them and retrieve them if we need
    SourceFileVector sources;

    // A map which contains all of the filenames that we are using for line
    // overrides along with their id's. This is used to map from an override
    // filename id to an actual filename.
    LineOverrideFilenames override_filenames;

    // The information below will be used for tracking macro expansions. But
    // this is not currently yet complete / supported...
    Location next_macro_location;
    MacroEntryVector macro_entries;
} SourceManager;

// This is the location that we believe a location to fully and completely 
// resolve to. This should resolve any #line directives, including the filename
// and also resolve and line changes as a result of the directives. Should also
// make sure that the location of the include is resolved too.
typedef struct VirtualLocation {
    Filepath* path;
    uint32_t line;
    uint32_t col;
    Location include_location;
} VirtualLocation;

/* SourceFile */

// This will be used to create a user made buffer either through commmand line
// defines and includes (or builtin defines and that kind of thing) or through
// token concatenation and pasting.
SourceFile* source_file_create(SourceFileId id, Location start, Location include,
        FileBuffer* buffer);

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file);

// Get the id of a source file
SourceFileId source_file_get_id(const SourceFile* sf);

// Get the FileBuffer* of a source file
FileBuffer* source_file_get_buffer(const SourceFile* sf);

// Get the base filename of a source file
Filepath* source_file_get_name(const SourceFile* sf);

Location source_file_get_start_location(const SourceFile* sf);

// Get if a source file was included from another location
bool source_file_is_include(const SourceFile* sf);

// Get the include location of a source file, note that a source file must
// actually be considered and include (by function ..._is_include) in order
// for this function to return a valid value.
Location source_file_get_include_location(const SourceFile* sf);

/* SourceManager */

// Create a source manager 
SourceManager source_manager(void);

// Delete a source manager and all of the data ascociated with it
void source_manager_delete(SourceManager* sm);

// Some function prototypes for creating new sources which have not been included
// from within another source file. This effectively just creates a new source
// at the end of the array which is marked as not being included from another
// file.
SourceFile* source_manager_create_filepath(SourceManager* sm,
        Filepath path);
SourceFile* source_manager_create_builtin_buffer(SourceManager* sm,
        Buffer buffer);
SourceFile* source_manager_create_command_line_buffer(SourceManager* sm,
        Buffer buffer);
SourceFile* source_manager_create_anonomous_buffer(SourceManager* sm,
        Buffer buffer, Location include);

// Look up a source file from a given id
SourceFile* source_manager_from_id(SourceManager* sm, SourceFileId id);

// Look up the source file from a given location
SourceFile* source_manager_from_location(SourceManager* sm, Location loc);

#endif /* SOURCE_MANAGER_H */
