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

// A line ovverride filenaem is a private structure that we are going to use so
// that we are able to actually store our id's within the line override map.
typedef struct LineOverrideFilename {
    Filepath path; // the path the override contains
    LineOverrideFileId id; // the id of this filename
} LineOverrideFilename;

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

static uint32_t line_override_filename_hash(const void* lof)
{
    const Filepath* fp = lof;

    return cstring_get_hash(fp->path, fp->len);
}

static bool line_ovveride_filename_cmp(const void* lof1, const void* lof2)
{
    const Filepath* fp1 = lof1;
    const Filepath* fp2 = lof2;

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
        .next_id = 1,
        .filenames = hash_map_create(line_override_filename_hash,
                line_ovveride_filename_cmp, line_override_filename_free)
    };

    return lof;
}

// Delete a store of line override filenames
void line_override_filenames_free(LineOverrideFilenames* lofs)
{
    hash_map_delete(&lofs->filenames);
}

// Get the id asociated with a filepath inserting it if not already in the table
// otherwise simply getting the id asociated with it.
LineOverrideFileId line_override_filenames_get(LineOverrideFilenames* lofs, 
        Filepath path)
{
    LineOverrideFilename* lof = hash_map_get(&lofs->filenames, &path);

    if (lof != NULL)
    {
        return lof->id;
    }

    lof = line_override_filename_create(path, lofs->next_id);

    lofs->next_id++;

    LineOverrideFilename* inserted = hash_map_insert(&lofs->filenames, &lof->path, lof);

    assert(inserted == lof);

    return inserted->id;
}

LineOverride line_override_create(Location loc, uint32_t line_no, 
        LineOverrideFileId file)
{
    return (LineOverride) { loc, line_no, file };
}

vector_of_impl(LineOverride, LineOverride, line_override)

LineOverrideTable line_override_table_create(void)
{
    LineOverrideTable lot = (LineOverrideTable)
    {
        .overrides = line_override_vector_create(1)
    };

    return lot;
}

void line_override_table_delete(LineOverrideTable* table)
{
    line_override_vector_free(&table->overrides, NULL);
}

