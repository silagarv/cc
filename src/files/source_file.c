#include "source_file.h"

#include <stddef.h>
#include <assert.h>
#include <stdio.h>

#include "util/xmalloc.h"

#include "files/filepath.h"

static void source_file_open_and_read(SourceFile* file)
{
    FILE* f = fopen(file->name.path, "rb");
    
    // If opening failed just return
    if (!f)
    {
        return;
    }

    // Get the file size
    fseek(f, 0, SEEK_END);
    const size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    // Now create a buffer and set sourcefile up
    file->contents_size = size;
    file->contents = xmalloc(sizeof(char) * (size + 1));
    file->end_contents = file->contents + size;

    // Finally read into the buffer
    const size_t read = fread(file->contents, sizeof(char), size, f);

    // TODO: better error handling that we will sort out later but for now
    // this is good enough to get the entire file...
    assert(read == size);

    // Close the file
    fclose(f);
}

SourceFile* source_file_try_create(Filepath* path)
{
    assert(filepath_is_absolute(path));

    // Create the file and we will just have all fields blank except name
    SourceFile* file = xmalloc(sizeof(SourceFile));
    *file = (SourceFile) { .name = *path };

    // We got a directory which is definitely not a valid filename so we can
    // just return the file
    if (filepath_is_directory(path))
    {
        return file;
    }

    // Otherwise we will try to open and read it's contents
    source_file_open_and_read(file);

    // Now we can just return
    return file;
}

SourceFile* source_file_from_buffer(Filepath* name, Buffer buffer)
{
    SourceFile* file = xmalloc(sizeof(SourceFile));
    *file = (SourceFile) { .name = *name };

    // We need to extract the contents of the buffer and put then into the source
    // file now
    file->contents = buffer.buffer;
    file->end_contents = buffer.buffer + buffer.len;
    file->contents_size = buffer.len;

    return file;
}

