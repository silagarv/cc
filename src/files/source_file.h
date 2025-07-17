#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include <stddef.h>
#include <stdint.h>

#include "files/filepath.h"

typedef uint32_t SourceFileId;

typedef struct SourceFile {
    SourceFileId id;

    Filepath name;

    char* contents;
    char* end_contents;

    size_t contents_size;

} SourceFile;

#endif /* SOURCE_FILE_H */
