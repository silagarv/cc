#include "file_buffer.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "util/buffer.h"
#include "util/xmalloc.h"

static void read_file_contents(FILE* fp, char** buffer, size_t* length)
{
    fseek(fp, 0, SEEK_END);
    size_t file_length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    *buffer = xmalloc(sizeof(char) * (file_length + 1));
    *length = fread(*buffer, sizeof(char), file_length, fp);
    (*buffer)[*length] = '\0';

    assert(file_length == *length);
}

// Create a file buffer from a given path (or at least attempt to). Path should
// be the canonical path to the file itself
FileBuffer* file_buffer_try_get(Filepath path)
{
    FILE* fp = fopen(path.path, "rb");
    if (!fp)
    {
        return NULL;
    }

    // Get our buffer and it's length
    char* buffer = NULL;
    size_t length = 0;

    read_file_contents(fp, &buffer, &length);

    fclose(fp);

    FileBuffer* file_buffer = xmalloc(sizeof(FileBuffer));
    *file_buffer = (FileBuffer)
    {
        .path = path,
        .buffer_start = buffer,
        .buffer_end = buffer + length
    };

    return file_buffer;
}

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

// Free a file buffer structure
void file_buffer_free(FileBuffer* buffer)
{
    free(buffer->buffer_start);
    free(buffer);
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
    return (buffer->buffer_end - buffer->buffer_start + 1);
}
