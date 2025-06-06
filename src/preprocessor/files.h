#ifndef FILES_H
#define FILES_H

// NOTE: this is isolated since we maybe in future might want to port to windows
// NOTE: so we isolate incase we use anything Linux specific

#include <stdbool.h>
#include <stdio.h>

#include "util/static_string.h"

bool is_directory(StaticString* maybe_dir);
bool is_file(StaticString* maybe_file);

FILE* open_file(StaticString* filename);

#endif /* FILES_H */
