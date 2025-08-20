#include "source_manager.h"

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "util/panic.h"
#include "util/vec.h"
#include "util/xmalloc.h"

#include "files/location.h"
#include "files/filepath.h"
#include "files/file_manager.h"
#include "files/line_map.h"

// Instantiate the vector implementation with all of the function definition.

vector_of_impl(SourceFile*, SourceFile, source_file)

/* SourceFile */

SourceFile* source_file_create(SourceFileId id, Location start, Location include,
        FileBuffer* buffer)
{
    assert(location_is_file(start));

    SourceFile* file = xmalloc(sizeof(SourceFile));
    *file = (SourceFile) 
    {
        .id = id,
        .start_location = start,
        .included_location = include,
        .file_buffer = buffer,
        .line_map = line_map_create(buffer, start)
    };

    return file;
}

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file)
{
    line_map_delete(&file->line_map);
    free(file);
}

SourceFileId source_file_get_id(const SourceFile* sf)
{
    return sf->id;
}

FileBuffer* source_file_get_buffer(const SourceFile* sf)
{
    return sf->file_buffer;
}

Filepath* source_file_get_name(const SourceFile* sf)
{
    return &sf->file_buffer->path;
}

bool source_file_is_include(const SourceFile* sf)
{
    panic("TODO: figure out how we will represent includes");

    return false;
}

// Get the include location of a source file
Location source_file_get_include_location(const SourceFile* sf)
{
    assert(source_file_is_include(sf));

    return sf->included_location;
}

/* SourceManager */

SourceManager source_manager(void)
{
    SourceManager sm = (SourceManager)
    {
        .fm = file_manager_create(),
        .next_source_location = 1, // Start at location 1 so we're not invalid
        .next_id = 0,
        .sources = source_file_vector_create(16)
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{
    file_manager_free(&sm->fm);
    source_file_vector_free(&sm->sources, source_file_free);
}

static SourceFile* source_manager_assign_file(SourceManager* sm, FileBuffer* fb, 
        Location include)
{
    // Create the source file
    SourceFile* sf = source_file_create(sm->next_id, sm->next_source_location, include, fb);

    // Increment our file id's and determine the sm's new highest location
    sm->next_id++;
    sm->next_source_location += (Location) file_buffer_get_length(fb);

    assert(location_is_file(sm->next_source_location) && "Too many locations");

    // Add the source file to our list of files
    source_file_vector_push(&sm->sources, sf);

    return sf;
}

SourceFile* source_manager_create_filepath(SourceManager* sm, Filepath path)
{
    FileBuffer* fb = file_manager_try_get(&sm->fm, path);
    
    // Could not find or read the file so give up
    if (!fb)
    {
        return NULL;
    }

    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_create_builtin_buffer(SourceManager* sm, Buffer buffer)
{
    FileBuffer* fb = file_manager_buffer_from(&sm->fm, buffer, 
            FILE_BUFFER_BUILTIN);

    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_create_command_line_buffer(SourceManager* sm, Buffer buffer)
{
    FileBuffer* fb = file_manager_buffer_from(&sm->fm, buffer, 
            FILE_BUFFER_COMMAND_LINE);

    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_create_anonomous_buffer(SourceManager* sm, Buffer buffer)
{
    FileBuffer* fb = file_manager_buffer_from(&sm->fm, buffer, 
            FILE_BUFFER_ANONOMOUS);

    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_from_id(SourceManager* sm, SourceFileId id)
{
    return source_file_vector_get(&sm->sources, (size_t) id);
}

// Look up the source file from a given location
SourceFile* source_manager_from_location(SourceManager* sm, Location loc)
{
    assert(location_is_file(loc));

    if (loc > sm->next_source_location)
    {
        panic("Location given higher than maximum");

        return NULL;
    }

    // TODO: convert to a binary search from O(n) search
    for (size_t i = 0; i < source_file_vector_size(&sm->sources); i++) 
    {
        LocationRange range = source_file_vector_get(&sm->sources, i)->line_map.range;
        if (location_range_contains(&range, loc))
        {
            return source_file_vector_get(&sm->sources, i);
        }
    }

    assert(false && "Unreachable");

    return NULL;
}

