#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "files/file_buffer.h"
#include "files/location.h"

typedef uint32_t SourceFileId;

// A structure representing a source that we are going to use in compilation
// this structure is made to be nice and lean and contains pointers to data that
// we are wanting to store elsewhere mainly
typedef struct SourceFile {
    SourceFileId id; // the id of the source file we got
    Location included_location; // where the file was included from (if needed)
    FileBuffer* file_buffer; // the underlying file that we have
} SourceFile;

// This will be used to create a user made buffer either through commmand line
// defines and includes (or builtin defines and that kind of thing) or through
// token concatenation and pasting
SourceFile* source_file_create(SourceFileId id, FileBuffer* buffer, Location include);

// Free a source file structure not including the filebuffer
void source_file_free(SourceFile* file);

#endif /* SOURCE_FILE_H */
