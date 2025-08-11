#ifndef SOURCE_MANAGER_H
#define SOURCE_MANAGER_H

#include "files/file_buffer.h"
#include "files/filepath.h"
#include "util/hash_map.h"

#include "files/location.h"

// TODO: make a line overrride map structure as well

// A structure to hold all of the needed data to retrieve and store files for
// later use. This is made so that we can hold any and all information about files
// locations and source of data (even if they are not physical files on disk)
typedef struct SourceManager {
    // The current working directory as an absolute path so that we can store
    // the full location of files in the filemap
    Filepath cwd;

    // The map of real files that we have created in which the we store a 
    // FileBuffer struct. The keys of the map is a pointer to the filepath and
    // the data in the map is the entire filebuffer structure
    HashMap filemap;

    // The highest location we have given out. We can never agian give out a
    // location that is lower than this highest location. This is in order to
    // avoid any conflicts with existing locations
    Location highest_location;

    // TODO: we would like to add a vector of all of our source_files so that
    // we can easily keep track of them and retrieve them if we need
} SourceManager;

// Create a source manager 
SourceManager source_manager(void);

// Delete a source manager and all of the data ascociated with it
void source_manager_delete(SourceManager* sm);

FileBuffer* source_manager_get(SourceManager* sm, Filepath* path);

#endif /* SOURCE_MANAGER_H */
