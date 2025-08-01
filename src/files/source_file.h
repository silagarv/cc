#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/filepath.h"
#include "lex/location.h"

// A structure representing a source that we are going to use in compilation
typedef struct SourceFile {
    Filepath name; // The exact path to the file

    bool has_contents; // Did we successfully read the file?

    char* contents; // the start contents of the file
    char* end_contents; // the end contents of the file
    size_t contents_size; // the exact size of the file
} SourceFile;

typedef uint32_t LogicalFileId;

// An instance of the file within the compiler
typedef struct LogicalFile {
    LogicalFileId id; // the logical id of the file

    SourceFile* source; // a pointer to the source file

    Location start_location; // the location given to the start of the file
} LogicalFile;

typedef struct FileManager {
    Filepath current_directory; // The invocation directory of the program

    SourceFile** files; // A list of files
    size_t num_files;
    size_t cap_files;



    
} FileManager;

// Try to create a source file from an absolute path and return it even if it
// is not a valid file. This will help us to cache results even if they are
// invalid
SourceFile* source_file_try_create(Filepath* path);

#endif /* SOURCE_FILE_H */
