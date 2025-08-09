#include "source_manager.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "util/panic.h"
#include "util/hash.h"
#include "util/hash_map.h"

#include "files/filepath.h"
#include "files/file_buffer.h"

// The function used in the filemap in order to be able to hash the keys
static uint32_t filemap_hash_function(const void* fp)
{
    const Filepath* path = fp;
    return cstring_get_hash(path->path, path->len);
}

// the function used in the filemap to compare the two filepaths
static bool filemap_key_compare_function(const void* fp1, const void* fp2)
{
    const Filepath* buffer1 = fp1;
    const Filepath* buffer2 = fp2;

    if (buffer1->len != buffer2->len)
    {
        return false;
    }

    return (strncmp(buffer1->path, buffer2->path, buffer1->len) == 0);
}

// The function used to free the entries within the filemap
static void filemap_free_function(void* key, void* data)
{
    // Since key is not a malloced pointer there is nothing to free here
    // also noting that data contains key as well so we don't want to double
    // free
    (void) key;

    // Here the data is a FileBuffer* so we can just go and free that
    file_buffer_free(data);
}

SourceManager source_manager(void)
{
    SourceManager sm;

    if (!filepath_get_current_path(&sm.cwd)) 
    {
        panic("unable to determine the current working directory");
    }

    sm = (SourceManager)
    {
        .filemap = hash_map_create(filemap_hash_function, 
                filemap_key_compare_function, filemap_free_function),
        .highest_location = 0
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{
    hash_map_delete(&sm->filemap);
}
