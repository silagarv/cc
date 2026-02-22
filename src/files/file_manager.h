#ifndef FILE_BUFFER_H
#define FILE_BUFFER_H

#include <stdbool.h>

#include "util/buffer.h"
#include "util/vec.h"
#include "util/hash_map.h"

#include "files/filepath.h"

// The type of file buffer we are. File's represent normal files on disk,
// builtin represents the builtin buffer for definitions given before any file
// is run. The command line represents commandline options given by the user
// and the anonomous represents any buffer used for anything else as given below
typedef enum FileBufferType {
    FILE_BUFFER_FILE,
    FILE_BUFFER_STDIN,
    FILE_BUFFER_BUILTIN,
    FILE_BUFFER_COMMAND_LINE,
    FILE_BUFFER_ANONOMOUS
} FileBufferType;

// FileBuffer object to help us keep track of our nice buffer and to help us 
// cache the results of file lookups to help keep them quick
typedef struct FileBuffer {
    Filepath path; // the given path to file should be canonical
    char* buffer_start; // start of the buffer
    char* buffer_end; // end of the buffer (always '\0')
    FileBufferType type; // the type of filebuffer we are
} FileBuffer;

vector_of_decl(FileBuffer*, FileBuffer, file_buffer);

// A file manager structure used to store all of our filebuffers, retrieve them,
// get canonicalise the paths and help us to find new files.
typedef struct FileManager {
    // The map which stores the actual files used during compilation which are
    // on the actual filesystem of the computer. Kept seperate from the buffers
    // below since a real file could be called for example "<builtin>" which
    // would cause issues if for some reason we wanted to actually use that file
    HashMap files;

    // The map which stores the ananomous buffers used by the file manager. All
    // of these files will have similar names e.g. <ananomous-n> where n is the
    // count of the ananomous file. This should be used for compiler generated
    // buffers for example _Pragma buffers or stringification of possibly token
    // concatenation
    FileBufferVector ananomous_files;

    // The "<builtin>" buffer used for builtin in macros and other defines, kept
    // hidden from the user.
    FileBuffer* builtin;

    // The "<command-line>" buffer used for command line defines and includes
    // specified by the user and run before the main file compilation. Kept 
    // seperate from the otherfiles for above reasons
    FileBuffer* command_line;
} FileManager;

/* FileBuffer */

// Create a file buffer from a given path (or at least attempt to). Path should
// be relative to the invocation directory but can also be absolute if 
// required.If the file is unable to be opened or read then a NULL pointer is 
// returned indicating it failed.
FileBuffer* file_buffer_try_get(Filepath path);

// Attempt to get a filebuffer by reading from standard input until all the 
// input from it is read. If this fails, then NULL is returned
FileBuffer* file_buffer_from_stdin(void);

// Create a file buffer from a buffer with a given name. Should only be used to
// create compiler generated buffers.
FileBuffer* file_buffer_from_buffer(Filepath name, Buffer buffer,
        FileBufferType type);

// Free a file buffer structure
void file_buffer_free(FileBuffer* buffer);

// Get the name give to the FileBuffer object
const Filepath* file_buffer_get_name(const FileBuffer* buffer);

// Get the start and end of a FileBuffer object
const char* file_buffer_get_start(const FileBuffer* buffer);
const char* file_buffer_get_end(const FileBuffer* buffer);

// Get the length of a filebuffer object
size_t file_buffer_get_length(const FileBuffer* buffer);

/* FileManager */

// Create a file manager structure
FileManager file_manager_create(void);

// Free all of the memory in a file manager structure
void file_manager_free(FileManager* fm);

// Attempt to add a file to the filemanager with the given path. Will first try
// to find the file within the filemanager and if not found attempt to create
// the file. This will only ever work on real files and will never return
// a builtin, commandline, or anonomous file. Note that if under a slightly
// different name, then we might not be able to resolve to the same file and
// could possibly end up storing a duplicate copy. This is fine and helps us
// to track accurate names.
FileBuffer* file_manager_try_get(FileManager* fm, Filepath path);

// Functions to add new 'files' to the file manager in order to have them be of
// use to us for lexing and that kind of thing.
FileBuffer* file_manager_add_anonymous(FileManager* fm, Buffer buffer);
FileBuffer* file_manager_add_builtin(FileManager* fm, Buffer buffer);
FileBuffer* file_manager_add_command_line(FileManager* fm, Buffer buffer);

#endif /* FILE_BUFFER_H */
