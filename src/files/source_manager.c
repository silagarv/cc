#include "source_manager.h"

#include <string.h>
#include <stdlib.h>

#include "files/line_map.h"
#include "util/panic.h"
#include "util/vec.h"
#include "util/xmalloc.h"
#include "util/hash.h"
#include "util/hash_map.h"

#include "files/filepath.h"
#include "files/file_manager.h"

// Instantiate the vector implementation with all of the function definition.

vector_of_impl(SourceFile*, SourceFile, source_file)

/* SourceFile */

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
    line_map_delete(&file->line_map);
    free(file);
}

/* SourceManager */

SourceManager source_manager(void)
{
    SourceManager sm = (SourceManager)
    {
        .fm = file_manager_create(),
        .highest_location = 0
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{
    file_manager_free(&sm->fm);
}
