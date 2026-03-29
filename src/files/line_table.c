#include "line_table.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "util/xmalloc.h"
#include "util/hash.h"
#include "util/hash_map.h"
#include "util/vec.h"

#include "files/location.h"
#include "files/filepath.h"

vector_of_impl(LineOverrideFilename*, LineOverrideFilename,
        line_override_filename)
vector_of_impl(LineOverride, LineOverride, line_override)

// A line ovverride filenaem is a private structure that we are going to use so
// that we are able to actually store our id's within the line override map.
struct LineOverrideFilename {
    Filepath path; // the path the override contains
    LineOverrideFileId id; // the id of this filename
};

static LineOverrideFilename* line_override_filename_create(Filepath path, 
        LineOverrideFileId id)
{
    LineOverrideFilename* lof = xmalloc(sizeof(LineOverrideFilename));
    *lof = (LineOverrideFilename)
    {
        .path = path,
        .id = id
    };

    return lof;
}

static void line_override_filename_free(void* key, void* data)
{
    (void) key;
    LineOverrideFilename* lof = data;
    free(data);
}

static Filepath* line_override_filename_path(LineOverrideFilename* lof)
{
    return &lof->path;
}

static uint32_t line_override_filename_hash(const void* lof)
{
    const Filepath* fp = lof;
    return cstring_get_hash(fp->path, fp->len);
}

static bool line_ovveride_filename_cmp(const void* lof1, const void* lof2)
{
    Filepath* fp1 = &((LineOverrideFilename*) lof1)->path;
    Filepath* fp2 = &((LineOverrideFilename*) lof1)->path;

    if (fp1->len != fp2->len)
    {
        return false;
    }

    return (strcmp(fp1->path, fp2->path) == 0);
}

bool line_override_file_id_is_valid(LineOverrideFileId id)
{
    return (id != 0);
}

// Create a strcture to store all of the line override filenames
LineOverrideFilenames line_override_filenames_create(void)
{
    LineOverrideFilenames lof = (LineOverrideFilenames)
    {
        .next_id = 0,
        .filenames = hash_map_create(line_override_filename_hash,
                line_ovveride_filename_cmp, line_override_filename_free),
        .names = line_override_filename_vector_create(0)
    };

    return lof;
}

// Delete a store of line override filenames
// FIXME: probably want to delete the filenames in the vector not in the 
// FIXME: hashtable might lead to better performance in the long run.
void line_override_filenames_free(LineOverrideFilenames* lofs)
{
    hash_map_delete(&lofs->filenames);
    line_override_filename_vector_free(&lofs->names, NULL);
}

// Get the id asociated with a filepath inserting it if not already in the table
// otherwise simply getting the id asociated with it.
LineOverrideFileId line_override_filenames_get(LineOverrideFilenames* lofs, 
        const Filepath* path)
{
    LineOverrideFilename* lof = hash_map_get(&lofs->filenames, (void*) path);
    if (lof != NULL)
    {
        return lof->id;
    }

    lof = line_override_filename_create(*path, lofs->next_id);
    line_override_filename_vector_push(&lofs->names, lof);

    lofs->next_id++;
    LineOverrideFilename* inserted = hash_map_insert(&lofs->filenames,
            &lof->path, lof);

    assert(inserted == lof);
    return inserted->id;
}

Filepath* line_override_filenames_from_id(LineOverrideFilenames* lofs,
        LineOverrideFileId id)
{
    LineOverrideFilename* lof = line_override_filename_vector_get(&lofs->names, 
            id);
    return line_override_filename_path(lof);
}

LineOverride line_override_create(Location loc, Location end, uint32_t line_no, 
        LineOverrideFileId file, uint32_t real_line)
{
    return (LineOverride) { loc, end, line_no, file, real_line };
}

Location line_override_location(const LineOverride* override)
{
    return override->location;
}

uint32_t line_override_line_no(const LineOverride* override)
{
    return override->line_no;
}

LineOverrideFileId line_override_file_id(const LineOverride* override)
{
    return override->id;
}

uint32_t line_override_real_line(const LineOverride* override)
{
    return override->real_line;
}

int line_override_compare(const Location* loc, const LineOverride* override)
{
    LocationRange range = location_range_create(override->location,
            override->ending_loc);
    return location_range_compare(loc, &range);
}
