#include "source_manager.h"

#include <string.h>
#include <stdlib.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/hash.h"
#include "util/hash_map.h"

#include "files/filepath.h"
#include "files/file_manager.h"

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
    free(file);
}

/* SourceManager */

SourceManager source_manager(void)
{
    SourceManager sm = (SourceManager)
    {
        //fm = hash_map_create(filemap_hash_function, 
         //       filemap_key_compare_function, filemap_free_function),
        .highest_location = 0
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{

}
