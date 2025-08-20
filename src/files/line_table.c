#include "line_table.h"

#include <stdint.h>
#include <stdlib.h>

#include "util/vec.h"

#include "files/location.h"
#include "files/filepath.h"

vector_of_impl(LineOverride, LineOverride, line_override)

bool line_override_file_id_is_valid(LineOverrideFileId id)
{
    return (id != 0);
}

LineOverride line_override_create(Location loc, uint32_t line_no, 
        LineOverrideFileId file)
{
    return (LineOverride) { loc, line_no, file };
}


