#include "source_manager.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "util/vec.h"
#include "util/xmalloc.h"

#include "files/location.h"
#include "files/filepath.h"
#include "files/file_manager.h"
#include "files/line_table.h"
#include "files/line_map.h"

// Instantiate the vector implementation with all of the function definition.

vector_of_impl(SourceFile*, SourceFile, source_file)

VirtLocation virtual_location_create(const SourceFile* sf, const Filepath* path,
        uint32_t line, uint32_t column, Location include_location)
{
    VirtLocation vloc = (VirtLocation) {
        .sf = sf,
        .path = path,
        .line = line,
        .column = column,
        .include_location = include_location
    };

    return vloc;
}

const SourceFile* virtual_location_file(const VirtLocation* location)
{
    return location->sf;
}

const Filepath* virtual_location_path(const VirtLocation* location)
{
    return location->path;
}

uint32_t virtual_location_line(const VirtLocation* location)
{
    return location->line;
}

uint32_t virtual_location_column(const VirtLocation* location)
{
    return location->column;
}

Location virtual_location_include(const VirtLocation* location)
{
    return location->include_location;
}

bool virtual_location_is_include(const VirtLocation* location)
{
    return location->include_location != LOCATION_INVALID;
}

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
        .end_location = start + file_buffer_get_length(buffer),
        .included_location = include,
        .file_buffer = buffer,
        .line_map = line_map_create(buffer, start),
        .line_overrides = line_override_vector_create(0)
    };

    return file;
}

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file)
{
    line_map_delete(&file->line_map);
    line_override_vector_free(&file->line_overrides, NULL);
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

Location source_file_start_location(const SourceFile* sf)
{
    return sf->start_location;
}

Location source_file_end_location(const SourceFile* sf)
{
    return sf->end_location;
}

bool source_file_is_include(const SourceFile* sf)
{
    return sf->included_location != LOCATION_INVALID;
}

// Get the include location of a source file
Location source_file_get_include_location(const SourceFile* sf)
{
    return sf->included_location;
}

bool source_file_contains(const SourceFile* sf, Location location)
{
    LocationRange range = { sf->start_location, sf->end_location };
    return location_range_contains(&range, location);
}

const LineMap* source_file_line_map(const SourceFile* sf)
{
    return &sf->line_map;
}

LineOverrideVector* source_file_overrides(const SourceFile* sf)
{
    return (LineOverrideVector*) &sf->line_overrides;
}

/* SourceManager */

SourceManager source_manager(void)
{
    SourceManager sm = (SourceManager)
    {
        .fm = file_manager_create(),
        .next_source_location = 1, // Start at location 1 so we're not invalid
        .next_id = 0,
        .sources = source_file_vector_create(16),
        .override_filenames = line_override_filenames_create()
    };

    return sm;
}

void source_manager_delete(SourceManager* sm)
{
    file_manager_free(&sm->fm);
    source_file_vector_free(&sm->sources, source_file_free);
    line_override_filenames_free(&sm->override_filenames);
}

static SourceFile* source_manager_assign_file(SourceManager* sm, FileBuffer* fb, 
        Location include)
{
    // Create the source file
    SourceFile* sf = source_file_create(sm->next_id, sm->next_source_location,
            include, fb);

    // TODO: check overflow here...

    // Increment our file id's and determine the sm's new highest location
    sm->next_id++;
    sm->next_source_location += (Location) file_buffer_get_length(fb);

    assert(location_is_file(sm->next_source_location) && "Too many locations");

    // Add the source file to our list of files
    source_file_vector_push(&sm->sources, sf);

    return sf;
}

SourceFile* source_manager_create_filepath(SourceManager* sm, Filepath path,
        Location include)
{
    FileBuffer* fb = file_manager_try_get(&sm->fm, path);

    // Could not find or read the file so give up
    if (!fb)
    {
        return NULL;
    }

    return source_manager_assign_file(sm, fb, include);
}

SourceFile* source_manager_create_builtin_buffer(SourceManager* sm,
        Buffer buffer)
{
    FileBuffer* fb = file_manager_add_builtin(&sm->fm, buffer);
    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_create_command_line_buffer(SourceManager* sm,
        Buffer buffer)
{
    FileBuffer* fb = file_manager_add_command_line(&sm->fm, buffer);
    return source_manager_assign_file(sm, fb, 0);
}

SourceFile* source_manager_create_anonomous_buffer(SourceManager* sm,
        Buffer buffer, Location include)
{
    FileBuffer* fb = file_manager_add_anonymous(&sm->fm, buffer);
    return source_manager_assign_file(sm, fb, include);
}

void source_manager_set_main_file(SourceManager* sm, SourceFile* sf)
{
    sm->main_file = sf;
}

SourceFile* source_manager_get_main_file(SourceManager* sm)
{
    return sm->main_file;
}

SourceFile* source_manager_from_id(SourceManager* sm, SourceFileId id)
{
    return source_file_vector_get(&sm->sources, (size_t) id);
}

int source_file_compare(const Location* key, const SourceFile** member)
{
    // FIXME: this is not the best
    const SourceFile* file = *member;
    const LocationRange* range = &file->line_map.range;
    return location_range_compare(key, range);
}

// Look up the source file from a given location. We first try to optomise to
// see if the location if in the main file as we assume the possibility of most
// errors being in the main file.
// FIXME: this is sill possibly a little too slow. Can we speed this up at all?
// TODO: add caching to this maybe?
SourceFile* source_manager_from_location(SourceManager* sm, Location loc)
{
    assert(location_is_file(loc) && "not a file location");
    assert(loc <= sm->next_source_location && "location too high!");

    SourceFile* main = sm->main_file;
    if (main != NULL)
    {
        if (!source_file_compare(&loc, (const SourceFile**) &main))
        {
            return main;
        }
    }

    // Use bsearch to hopefully find the source file we are after quite quickly
    SourceFile** file = bsearch(&loc, source_file_vector_front(&sm->sources),
            source_file_vector_size(&sm->sources), sizeof(SourceFile*),
            (int (*)(const void*, const void*)) source_file_compare);

    assert(file != NULL && "failed to find file");
    return *file;
}

void source_manager_do_line(SourceManager* sm, const Filepath* path,
            uint32_t number, Location start_loc)
{
    // First, get the id of the filepath's line overrride name
    LineOverrideFileId id = line_override_filenames_get(&sm->override_filenames,
            path);

    // We will need to get the sourcefile and get the location range this is for
    // then we will need to add the entry and handle some other crap...
    SourceFile* sf = source_manager_from_location(sm, start_loc);

    // Then we will want to handle 2 cases.
    // 1. We have no current line directives -> just add one in from start
    // 2. We are adding a new line directive -> get last one and set final loc
    //                                          and then add a new one in
    Location ending_loc = source_file_end_location(sf);
    LineOverrideVector* overrides = source_file_overrides(sf);
    if (!line_override_vector_empty(overrides))
    {
        // If not empty, fix the ending location up.
        LineOverride* last = line_override_vector_back(overrides);
        // FIXME: line_override_set_ending_loc(last, start_loc);
        last->ending_loc = start_loc;
    }

    // We add a new override in on both cases. But first bump the start_loc by
    // 1 since start_loc currently referes to the end of the line. Then we want
    // to resolve the real line number for this override
    start_loc++; // Since we only want to start on the next line.
    const LineMap* lm = source_file_line_map(sf);
    ResolvedLocation rloc = line_map_resolve_location(lm, start_loc);

    line_override_vector_push(overrides, line_override_create(start_loc,
            ending_loc, number, id, resolved_location_line(&rloc)));
}

// Recalcualte the line which we should use given the following.
void source_manager_recalculate_location(SourceManager* sm, Location loc,
        SourceFile* sf, uint32_t rline, Filepath** path, Location* line)
{
    assert(!line_override_vector_empty(source_file_overrides(sf))
            && "recalculating line num but no #line directives??");

    // First we see to find the override for the location. To do this, we find
    // the override that contains the location. i.e. we are bigger that the 
    // start of the ovveride, but smaller than the start of the next one.
    LineOverrideVector* overrides = source_file_overrides(sf);
    LineOverride* override = bsearch(&loc,
            line_override_vector_front(overrides),
            line_override_vector_size(overrides), sizeof(LineOverride),
            (int (*)(const void*, const void*)) line_override_compare);

    // Since we should only get this override if we're below it
    assert(rline >= line_override_real_line(override) && "wrong override?");

    // Then we need to get the difference between the rline and the real line so
    // that we can know how many to add to the overrides line
    uint32_t line_diff = rline - line_override_real_line(override);

    *line = line_override_line_no(override) + line_diff;
    *path = line_override_filenames_from_id(&sm->override_filenames,
            line_override_file_id(override));
}

VirtLocation source_manager_virtual_location(SourceManager* sm, Location loc)
{
    // First get the source file from the location
    SourceFile* sf = source_manager_from_location(sm, loc);

    // Then we will need to use the linemap to get the resolved location to get
    // the line's location range this location is part of, and to get the raw
    // line, and column for this one.
    const LineMap* lm = source_file_line_map(sf);
    ResolvedLocation rloc = line_map_resolve_location(lm, loc);

    uint32_t rline = resolved_location_line(&rloc);
    uint32_t rcol = resolved_location_column(&rloc);

    // Now we have resolved the rough line. If the line table then has some 
    // entries we will need to go and figure out the virtual line instead of
    // just using the actual.
    Filepath* path = source_file_get_name(sf);
    uint32_t line = rline;
    if (!line_override_vector_empty(source_file_overrides(sf)))
    {
        source_manager_recalculate_location(sm, loc, sf, rline, &path, &line);
    }

    // Then we will ned to use the resolved location to get the raw filebuffer
    // so that we are able to calculate the 'real' column that we are given. 
    // i.e. by handling multiwidth characters and such. (for now just tabs)
    // TODO: handle this...
    uint32_t col = rcol;

    Location include = source_file_get_include_location(sf);
    return virtual_location_create(sf, path, line, col, include);
}

