#include "location_map.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

#include "util/panic.h"

#include "files/filepath.h"

#include "lex/location.h"
#include "lex/source_line.h"
#include "util/xmalloc.h"

// Add a line to a line run and return the base location given for that 
// particular line.
Location line_run_add_line(LineRun* run, SourceLine line)
{
    if (run->finalised)
    {
        panic("cannot add line to finalised LineRun");
        return 0;
    }

    // Since we start with 0 allocated in case of a new LineRun then we will
    // have to allocate a since line info
    if (run->used == run->allocated)
    {
        run->allocated = (run->allocated == 0) ? 1 : run->allocated * 2;
        run->lines = xrealloc(run->lines, sizeof(LineInfo) * run->allocated);
    }

    // Calculate the new line number
    const uint32_t new_line_num = (run->used == 0) ? (run->starting_line) : 
            ((run->lines[run->used - 1].current_line) +
            (run->lines[run->used - 1].line.num_phyical_lines));

    // Add the line to the line run
    run->lines[run->used++] = (LineInfo)
    {
        .start_location = run->highest_location,
        .end_location = run->highest_location + line.string.len,

        .line = line,

        .current_name = &run->filename,
        .current_line = new_line_num
    };

    // Don't forget to add the length to the run
    run->highest_location += line.string.len;
    // TODO: add the highest location to the map as well when that is impl'd

    return run->lines[run->used - 1].start_location;
}

void line_run_finalise(LineRun* run)
{
    run->finalised = true;
}

ResolvedLocation line_run_resolve_location(LineRun* run, Location loc)
{
    const Location min_loc = run->start_location;
    const Location max_loc = run->highest_location;

    if (loc < min_loc)
    {
        panic("location given is less than what is in the run");
        return (ResolvedLocation) {0};
    }

    if (loc >= max_loc)
    {
        panic("location given is greater than what is in the run");
        return (ResolvedLocation) {0};
    }

    // TODO: maybe optimise to a binary search if needed???
    // but even for sqlite file this seems very very fast. Yay!
    for (size_t i = 0; i < run->used; i++)
    {
        LineInfo info = run->lines[i];

        if (loc >= info.start_location && loc < info.end_location)
        {
            uint32_t col = loc - info.start_location + 1;

            ResolvedLocation resolved = (ResolvedLocation)
            {
                .name = info.current_name,
                .line = info.current_line,
                .col = col
            };

            return resolved;
        }
    }

    panic("something unknown went wrong");

    return (ResolvedLocation) {0};
}

// Free a line run and all of the memory asociated with it
void line_run_free(LineRun* run)
{
    for (size_t i = 0; i < run->used; i++)
    {
        free(run->lines[i].line.string.ptr);
    }
    free(run->lines);
}
