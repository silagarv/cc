#ifndef FILE_BUFFER_H
#define FILE_BUFFER_H

#include "util/buffer.h"

#include "files/filepath.h"
#include <stddef.h>

// FileBuffer object to help us keep track of our nice buffer and to help us 
// cache the results of file lookups to help keep them quick
typedef struct FileBuffer {
    Filepath path; // the given path to file should be canonical
    char* buffer_start; // start of the buffer
    char* buffer_end; // end of the buffer (always '\0')
} FileBuffer;

// Create a file buffer from a given path (or at least attempt to). Path should
// be the canonical path to the file itself. If the file is unable to be opened
// or read then a NULL pointer is returned indicating it failed.
FileBuffer* file_buffer_try_get(Filepath path);

// Create a file buffer from a buffer with a given name
FileBuffer* file_buffer_from_buffer(Filepath name, Buffer buffer);

// Free a file buffer structure
void file_buffer_free(FileBuffer* buffer);

// Get the name give to the FileBuffer object
const Filepath* file_buffer_get_name(const FileBuffer* buffer);

// Get the start and end of a FileBuffer object
const char* file_buffer_get_start(const FileBuffer* buffer);
const char* file_buffer_get_end(const FileBuffer* buffer);

// Get the length of a filebuffer object
size_t file_buffer_get_length(const FileBuffer* buffer);

#endif /* FILE_BUFFER_H */
