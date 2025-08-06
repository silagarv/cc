#include "file_buffer.h"

#include <assert.h>

#include "util/buffer.h"
#include "util/xmalloc.h"

// Create a file buffer from a given path (or at least attempt to). Path should
// be the canonical path to the file itself
FileBuffer* file_buffer_try_get(Filepath path);

// Create a file buffer from a buffer with a given name
FileBuffer* file_buffer_from_buffer(Filepath name, Buffer buffer)
{
    FileBuffer* file = xmalloc(sizeof(FileBuffer));
    *file = (FileBuffer)
    {
        .path = name,
        .buffer_start = buffer_get_ptr(&buffer),
        .buffer_end = buffer_get_ptr(&buffer) + buffer_get_len(&buffer)
    };

    assert(*file->buffer_end == '\0');

    return file;
}

// Get the name give to the FileBuffer object
const Filepath* file_buffer_get_name(const FileBuffer* buffer)
{
    return &buffer->path;
}

// Get the start and end of a FileBuffer object
const char* file_buffer_get_start(const FileBuffer* buffer)
{
    return buffer->buffer_start;
}

const char* file_buffer_get_end(const FileBuffer* buffer)
{
    return buffer->buffer_end;
}

// Get the length of a filebuffer object
size_t file_buffer_get_length(const FileBuffer* buffer)
{
    return buffer->buffer_end - buffer->buffer_start;
}
