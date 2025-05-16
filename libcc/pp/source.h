

#include <stddef.h>
#include <stdio.h>

// represet a source

// basic char reading actions

// basically just use this to build a line and read from it

#include "adt/buffer.h"

#include "core/location.h"

#include "pp/input_manager.h"

typedef struct Source {
    char* filename;

    // For reading the next line from the source
    FILE* fp;

    Buffer* buffer;

    LineLocation loc;
    LineLocation real_loc;

    // For inclusions
    size_t include_depth;
    void* searchpath;
    struct Source* prev;
} Source;
