#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/filepath.h"

typedef uint32_t SourceFileId;

typedef struct SourceFile {
    SourceFileId id; // the id of the source file

    Filepath name; // exact path of the file

    char* contents; // File contents
    char* end_contents; // the end of the file contents
    size_t contents_size; // length of the file
} SourceFile;

typedef struct FileManager {
    Filepath current_path; // the current path so we know where to look

    
} FileManager;

bool source_file(SourceFile* file, Filepath* path);

#endif /* SOURCE_FILE_H */
