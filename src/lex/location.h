#ifndef LOCATION_H
#define LOCATION_H

#include <stdint.h>

#include "files/filepath.h"

typedef uint32_t Location;

typedef struct ResolvedLineLocation {
    Filepath* path;
    uint32_t line;
} ResolvedLineLocation;

typedef struct ResolvedLocation {
    Filepath* name;
    uint32_t line;
    uint32_t col;
} ResolvedLocation;

#endif /* LOCATION_H */
