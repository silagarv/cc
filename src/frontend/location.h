#ifndef LOCATION_H
#define LOCATION_H

#include <stdint.h>

// A `real` location i.e. not just some number that we can trace...
struct Location {
    const char* filename;
    uint32_t line;
    uint32_t col;
};
typedef struct Location Location;

#endif /* LOCATION_H */
