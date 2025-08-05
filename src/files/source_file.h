#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/buffer.h"

#include "files/filepath.h"
#include "files/file_buffer.h"
#include "files/location.h"

typedef uint32_t SourceFileId;

// A structure representing a source that we are going to use in compilation
typedef struct SourceFile {
    SourceFileId id; // the id of the source file we got
    Location start_location; // the starting location of the file

    FileBuffer* file_buffer; // the underlying file that we have

    // TODO: we want to get rid of all of the below eventually
    Filepath name; // The exact path to the file

    char* contents; // the start contents of the file
    char* end_contents; // the end contents of the file
    size_t contents_size; // the exact size of the file
} SourceFile;

// Try to create a source file from an absolute path and return it even if it
// is not a valid file. This will help us to cache results even if they are
// invalid
SourceFile* source_file_try_create(Filepath* path);

// This will be used to create a user made buffer either through commmand line
// defines and includes (or builtin defines and that kind of thing) or through
// token concatenation and pasting
SourceFile* source_file_from_buffer(Filepath* name, Buffer buffer);

#endif /* SOURCE_FILE_H */
