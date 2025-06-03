#ifndef LOCATION_H
#define LOCATION_H

#include <stdint.h>

// A `real` location mapped from somewhere in the source
struct ResolvedLocation {
    const char* filename;
    uint32_t line;
    uint32_t col;
};
typedef struct ResolvedLocation ResolvedLocation;

typedef uint32_t Location;






#endif /* LOCATION_H */
