#include "header_finder.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "files/location.h"
#include "util/arena.h"

#include "files/source_manager.h"
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

DirectoryEntry* header_finder_quote(HeaderFinder* hf)
{
    return hf->quote;
}

DirectoryEntry* header_finder_angled(HeaderFinder* hf)
{
    return hf->angled;
}

DirectoryEntry* header_finder_system(HeaderFinder* hf)
{
    return hf->system;
}

DirectoryEntry* header_finder_after(HeaderFinder* hf)
{
    return hf->after;
}

DirectoryEntry* header_finder_iter(HeaderFinder* hf, DirectoryEntryType start)
{
    switch (start)
    {
        case DIRECTORY_ENTRY_QUOTE:
            return header_finder_quote(hf);
            
        case DIRECTORY_ENTRY_ANGLED:
            return header_finder_angled(hf);

        case DIRECTORY_ENTRY_SYSTEM:
            return header_finder_system(hf);

        case DIRECTORY_ENTRY_AFTER:
            return header_finder_angled(hf);

        default:
            panic("unreachable");
            return NULL;
    }
}

bool try_build_path(Filepath* destination, const char* dir, size_t dir_len,
        const char* path, size_t path_len)
{
    if (dir_len + path_len + 1 > FILENAME_MAX)
    {
        return false;
    }

    destination->len = snprintf(destination->path, FILENAME_MAX, "%.*s/%.*s",
            (int) dir_len, dir, (int) path_len, path);
    assert(destination->len <= FILENAME_MAX && "didn't detect long path?");
    return true;
}

bool header_finder_try_current_path(HeaderFinder* hf, SourceManager* sm,
        const Filepath* current_path, const Filepath* given_path,
        Location include_loc, SourceFile** include)
{
    // Get the current filepath as a string so that we are able to extract the
    // current directory it appears to be in.
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

bool header_finder_try_find_include(HeaderFinder* hf, SourceManager* sm,
        const Filepath* current_path, DirectoryEntry* current_directory,
        const Filepath* given_path, bool angled, Location include_loc,
        SourceFile** include)
{
    assert(current_directory == NULL && "should be unimplemented for now...");
    assert(*include == NULL && "shouldn't start with our include already...");

    // If we are not an angled include then we should try to search the current 
    // path first. Then, if we find it in there then we are good to go. There
    // is nothing more that we need to do.
    if (!angled)
    {
        if (header_finder_try_current_path(hf, sm, current_path, given_path,
                include_loc, include))
        {
            return true;
        }
    }

    // Otherwise we should start iterating through our search directories
    // DirectoryEntryType start_dir = angled 
    //         ? DIRECTORY_ENTRY_ANGLED 
    //         : DIRECTORY_ENTRY_QUOTE;
    // for (DirectoryEntry* iter = header_finder_iter(hf, start_dir);
    //         iter != NULL; 
    //         iter = directory_entry_next(iter))
    for (size_t i = 0; i < 4; i++)
    {
        assert(*include == NULL && "shouldn't have found include yet");

        // For now just simulate use having the include directory as I do not 
        // want to redo the command line parser yet...
        Filepath dir;
        if (i == 0)
        {
            dir = filepath_from_cstring("/usr/lib/gcc/x86_64-linux-gnu/13/include");
        }
        else if (i == 1)
        {
            dir = filepath_from_cstring("/usr/local/include");
        }
        else if (i == 2)
        {
            dir = filepath_from_cstring("/usr/include/x86_64-linux-gnu");
        }
        else if (i == 3)
        {
            dir = filepath_from_cstring("/usr/include");
        }
        
        // First try to build the given path from the search path. If this fails
        // just simply continue.
        Filepath path = {0};
        if (!try_build_path(&path, dir.path, dir.len, given_path->path,
                given_path->len))
        {
            continue;
        }

        // Then try to get the include, finishing if we get it.
        *include = source_manager_create_filepath(sm, path, include_loc);

        // If we get the include we can immediately be done as there is nothing
        // else that we would need to do.
        if (*include != NULL)
        {
            // FIXME: here we should set the secound output param when we get 
            // FIXME: that.
            return true;
        }
    }

    return false;
}
