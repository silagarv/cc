#include "target.h"

#include <stdbool.h>
#include <stddef.h>

Target target_create_x86_64_linux(void)
{
    Target target = (Target)
    {
        // Sizes
        1,
        1,
        2,
        4,
        8,
        8,
        4,
        8,
        16,
        8,
        8,

        // Alignement
        1,
        1,
        2,
        4,
        8,
        8,
        4,
        8,
        16,
        8,
        8,

        // Is char signed
        true,

        // Byte order
        TBO_LITTLE_ENDIAN
    };

    return target;
}

