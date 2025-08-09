#include "source_file.h"

#include <stdlib.h>

#include "util/xmalloc.h"

#include "files/file_buffer.h"
#include "files/location.h"

SourceFile* source_file_create(SourceFileId id, FileBuffer* buffer, Location include)
{
    SourceFile* file = xmalloc(sizeof(SourceFile));
    *file = (SourceFile) 
    {
        .id = id,
        .file_buffer = buffer,
        .included_location = include
    };

    return file;
}

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file)
{
    free(file);
}

