#ifndef SOURCE_MANAGER_H
#define SOURCE_MANAGER_H

#include "files/location.h"
#include "files/filepath.h"
#include "files/file_manager.h"
#include "files/line_map.h"

// TODO: make a line overrride map structure as well

typedef uint32_t SourceFileId;

// A structure representing a source that we are going to use in compilation
// this structure is made to be nice and lean and contains pointers to data that
// we are wanting to store elsewhere mainly
typedef struct SourceFile {
    SourceFileId id; // the id of the source file we got
    Location included_location; // where the file was included from (if needed)
    FileBuffer* file_buffer; // the underlying file that we have
    LineMap line_map; // The line map for this source file
} SourceFile;

// A structure to hold all of the needed data to retrieve and store files for
// later use. This is made so that we can hold any and all information about files
// locations and source of data (even if they are not physical files on disk)
typedef struct SourceManager {
    // The file manager for us to be able to keep track of all of the different
    // files within the compilation unit and so that we can track everything.
    FileManager fm;

    // The highest location we have given out. We can never agian give out a
    // location that is lower than this highest location. This is in order to
    // avoid any conflicts with existing locations
    Location highest_location;

    // TODO: we would like to add a vector of all of our source_files so that
    // we can easily keep track of them and retrieve them if we need

} SourceManager;

/* SourceFile */

// This will be used to create a user made buffer either through commmand line
// defines and includes (or builtin defines and that kind of thing) or through
// token concatenation and pasting
SourceFile* source_file_create(SourceFileId id, FileBuffer* buffer, Location include);

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file);

/* SourceManager */

// Create a source manager 
SourceManager source_manager(void);

// Delete a source manager and all of the data ascociated with it
void source_manager_delete(SourceManager* sm);

FileBuffer* source_manager_get(SourceManager* sm, Filepath* path);

#endif /* SOURCE_MANAGER_H */
