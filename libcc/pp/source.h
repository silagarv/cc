#ifndef SOURCE_H
#define SOURCE_H

#include <stddef.h>
#include <stdio.h>

// represet a source

// basic char reading actions

// basically just use this to build a line and read from it

#include "adt/buffer.h"
#include "adt/string_map.h"

#include "pp/location.h"
#include "pp/input_manager.h"

typedef struct Source {
    // For reading the next line from the source
    FILE* fp;
    Buffer* buffer;
    size_t buffer_pos;

    // the location's we are at in the source
    // also doubles for storing the real / current name
    LineLocation loc;
    LineLocation real_loc;

    // For storing all of our filenames
    StringMap* name_map;

    // For inclusions
    size_t include_depth;
    SearchpathEntry* searchpath;
    struct Source* prev;
} Source;

Source* source_push(Source* prev, FILE* fp, char* filename, 
        SearchpathEntry* searchpath);
void source_delete(Source* source);

// read a char from source and increment the current position
// note this action is potentially undoable as buffer may be overritten after
int source_read_char(Source* source);

void source_set_line(Source* source, size_t line_no);
void source_set_file(Source* source, char* filename);

#endif /* SOURCE_H */
