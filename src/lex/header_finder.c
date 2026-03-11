#include "header_finder.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "files/source_manager.h"
#include "util/arena.h"

#include "files/filepath.h"

HeaderFinder header_finder_create(void)
{
    return (HeaderFinder) { arena_new_default(), NULL, NULL, NULL, NULL };
}
void header_finder_delete(HeaderFinder* hf)
{
    // Delete arean and set the underlying memory to be all NULL's
    arena_delete(&hf->allocator);
    *hf = (HeaderFinder) {0};
}

// Add a directory path to the header finder
void header_finder_add_directory(HeaderFinder* hf, const Filepath* path)
{
    // FIXME: implement
}

bool header_finder_try_find_include(HeaderFinder* hf, SourceManager* sm,
        const Filepath* current_path, DirectoryEntry* current_directory,
        const Filepath* given_path, bool angled, Location include_loc,
        SourceFile** include)
{
    assert(current_directory == NULL && "should be unimplemented for now...");

    // Get the current filepath as a string so that we are able to extract the
    // current directory it appears to be in.
    // FIXME: cast is okay since a Filepath is never really mutable
    const char* curr_path_cstr = filepath_get_cstr((Filepath*) current_path);
    const char* last_slash = strrchr(curr_path_cstr, '/');

    // We will now try to construct the filepath in the to_find variable. This
    // will allow us to create and concatenate different paths.
    Filepath path = {0};
    char* path_buff = filepath_get_cstr(&path);
    size_t len = 0;
    if (last_slash != NULL)
    {
        // Try by printing the first part of the path into the filepath
        ptrdiff_t dir_len = last_slash + 1 - curr_path_cstr;
        len += (size_t) snprintf(path_buff, FILEPATH_LEN, "%.*s",
                (int) (dir_len), curr_path_cstr);
    }

    // Now we will want to print the rest of the given path into the buffer.
    const char* given_path_cstr = filepath_get_cstr((Filepath*) given_path);
    len += (size_t) snprintf(path_buff + len, FILEPATH_LEN - len, "%s",
            given_path_cstr);

    // If the length is too large to handle just return false at this point
    if (len > FILEPATH_LEN)
    {
        return false;
    }

    // DON'T FORGET TO SET PATH LEN!!! This will cause bad bugs
    path.len = len;

    // Here try to obtain the source file using the source manage and the given
    // path from the user since we are finished attempting to construct it.
    *include = source_manager_create_filepath(sm, path, include_loc);
    assert((*include == NULL 
            ? true : !strcmp(path_buff, (*include)->file_buffer->path.path))
            && "got a file but it's different from what we expect???");
    return *include != NULL;
}
