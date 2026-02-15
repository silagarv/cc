#include "file_manager.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <limits.h>

#include "files/filepath.h"
#include "util/hash_map.h"
#include "util/vec.h"
#include "util/xmalloc.h"
#include "util/buffer.h"
#include "util/hash.h"

#define STDIN_READ_SIZE (1024)

vector_of_impl(FileBuffer*, FileBuffer, file_buffer)

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

    // Check if we got a file or not
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
        .buffer_end = buffer + length,
        .type = FILE_BUFFER_FILE
    };

    return file_buffer;
}

FileBuffer* file_buffer_from_stdin(void)
{
    char* buffer = xmalloc(sizeof(char) * STDIN_READ_SIZE);
    size_t buffer_size = STDIN_READ_SIZE;
    size_t length = 0;
    while (true)
    {
        size_t read = fread(buffer + length, sizeof(char), buffer_size - length,
                stdin);
        length += read;

        if (length < buffer_size)
        {
            break;
        }

        assert(length == buffer_size);

        buffer_size *= 2;
        buffer = xrealloc(buffer, buffer_size);
    }

    // Unlikely but if we have finished reading from stdin and then we want to
    // null terminate but the buffer is full we need to resize it.
    // TODO: actually this might never be possible?
    if (length == buffer_size)
    {
        buffer_size *= 2;
        buffer = xrealloc(buffer, buffer_size);
    }

    // All filebuffers must be null terminated
    buffer[length] = '\0';

    Filepath path = filepath_from_cstring("<stdin>");
    FileBuffer* file_buffer = xmalloc(sizeof(FileBuffer));
    *file_buffer = (FileBuffer)
    {
        .path = path,
        .buffer_start = buffer,
        .buffer_end = buffer + length,
        .type = FILE_BUFFER_STDIN
    };

    return file_buffer;
}

// Create a file buffer from a buffer with a given name
FileBuffer* file_buffer_from_buffer(Filepath name, Buffer buffer,
        FileBufferType type)
{
    assert(type != FILE_BUFFER_FILE);

    FileBuffer* file = xmalloc(sizeof(FileBuffer));
    *file = (FileBuffer)
    {
        .path = name,
        .buffer_start = buffer_get_ptr(&buffer),
        .buffer_end = buffer_get_ptr(&buffer) + buffer_get_len(&buffer),
        .type = type
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

/* FileManager */
// The function used in the filemap in order to be able to hash the keys
static uint32_t fp_hash(const void* fp)
{
    const Filepath* path = fp;
    return cstring_get_hash(path->path, path->len);
}

// the function used in the filemap to compare the two filepaths
static bool fp_key_cmp(const void* fp1, const void* fp2)
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
static void fp_free(void* key, void* data)
{
    // Since key is not a malloced pointer there is nothing to free here
    // also noting that data contains key as well so we don't want to double
    // free
    (void) key;

    // Here the data is a FileBuffer* so we can just go and free that
    file_buffer_free(data);
}

// Create a file manager structure
FileManager file_manager_create(void)
{
    FileManager fm = (FileManager)
    {
        .files = hash_map_create(fp_hash, fp_key_cmp, fp_free),
        .ananomous_files = file_buffer_vector_create(16),
        .builtin = NULL,
        .command_line = NULL
    };

    return fm;
}

// Free all of the memory in a file manager structure
void file_manager_free(FileManager* fm)
{
    hash_map_delete(&fm->files);
    file_buffer_vector_free(&fm->ananomous_files, file_buffer_free);
    
    if (fm->builtin)
    {
        file_buffer_free(fm->builtin);
    }

    if (fm->command_line)
    {
        file_buffer_free(fm->command_line);
    }
}

// Attempt to add a file to the filemanager with the given path. Will first try
// to find the file within the filemanager and if not found attempt to create
// the file. This will only ever work on real files and will never return
// a builtin, commandline, or anonomous file. Note that if under a slightly
// different name, then we might not be able to resolve to the same file and
// could possibly end up storing a duplicate copy. This is fine and helps us
// to track accurate names.
FileBuffer* file_manager_try_get(FileManager* fm, Filepath path)
{
    FileBuffer* fb = hash_map_get(&fm->files, &path);

    // If it was already a found file just get it from the map
    if (fb) {
        return fb;
    }

    // Otherwise we will try to find it, read it if possible and then insert 
    // into the hashmap if it was a real file
    fb = file_buffer_try_get(path);
    
    // Attempt see if we are asking for the file from stdin. If so attempt to
    // read and return it but only if we didn't get the file yet.
    if (!fb && filepath_is(&path, "-")) {
        fb = file_buffer_from_stdin();
    }

    // Failed to get the file based on the path of the file and the file wasn't
    // attempting to be read from stdin either. So just return NULL and indicate
    // that we couldn't find the file.
    if (!fb)
    {
        return NULL;
    }

    // insert the file into the map
    assert(!hash_map_contains(&fm->files, &fb->path));
    hash_map_insert(&fm->files, &fb->path, fb);

    return fb;
}

FileBuffer* file_manager_add_anonymous(FileManager* fm, Buffer buffer)
{
    Filepath name = filepath_from_cstring("<anonymous>");
    
    // Create the new filebuffer and push it to our vector of anonymous buffers
    FileBuffer* new_buff = file_buffer_from_buffer(name, buffer,
            FILE_BUFFER_ANONOMOUS);
    file_buffer_vector_push(&fm->ananomous_files, new_buff);

    return new_buff;
}

FileBuffer* file_manager_add_builtin(FileManager* fm, Buffer buffer)
{
    assert(fm->builtin == NULL);

    Filepath name = filepath_from_cstring("<command-line>");
    
    // Create the new filebuffer and add it as our builtin buffer
    FileBuffer* new_buff = file_buffer_from_buffer(name, buffer,
            FILE_BUFFER_BUILTIN);
    fm->builtin = new_buff;

    return new_buff;
}

FileBuffer* file_manager_add_command_line(FileManager* fm, Buffer buffer)
{
    assert(fm->command_line == NULL);

    Filepath name = filepath_from_cstring("<command-line>");
    
    // Create the new filebuffer and add it as our command line buffer
    FileBuffer* new_buff = file_buffer_from_buffer(name, buffer,
            FILE_BUFFER_COMMAND_LINE);
    fm->command_line = new_buff;

    return new_buff;
}
