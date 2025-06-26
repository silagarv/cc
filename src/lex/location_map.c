#include "location_map.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

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

        .current_line = new_line_num
    };

    // Don't forget to add the length to the run
    run->highest_location += line.string.len;
    run->map->highest_location = run->highest_location;

    return run->lines[run->used - 1].start_location;
}

void line_run_finalise(LineRun* run)
{
    run->finalised = true;
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

ResolvedLineLocation line_run_resolve_line_location(LineRun* run, Location loc)
{
    // This is lazy. We fully resolve it then throw out information to get just
    // the line it was on.
    ResolvedLocation resolved = line_run_resolve_location(run, loc);

    return (ResolvedLineLocation) {.path = resolved.name, .line = resolved.line};
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
        LineInfo* info = &run->lines[i];

        if (loc >= info->start_location && loc < info->end_location)
        {
            const uint32_t col = loc - info->start_location + 1;

            ResolvedLocation resolved = (ResolvedLocation)
            {
                .name = &run->filename,
                .line = info->current_line,
                .col = col
            };

            return resolved;
        }
    }

    panic("Unknown error in line run resolve location");

    return (ResolvedLocation) {0};
}

LineMap line_map_create(void)
{
    LineMap map = (LineMap)
    {
        .runs = NULL,
        .used = 0,
        .allocated = 0,

        .include_depth = 0,

        .highest_location = 0
    };

    return map;
}

void line_map_free(LineMap* map)
{
    for (size_t i = 0; i < map->used; i++)
    {
        line_run_free(&map->runs[i]);
    }
    free(map->runs);
}

static void line_map_maybe_realloc(LineMap* map)
{
    if (map->used == map->allocated)
    {
        map->allocated = (map->allocated == 0) ? 1 : (map->allocated * 2);
        map->runs = xrealloc(map->runs, sizeof(LineRun) * map->allocated);
    }
}

LineRun* line_map_start(LineMap* map, Filepath* path)
{
    line_map_maybe_realloc(map);

    LineRun* run = &map->runs[map->used++];
    *run = (LineRun)
    {
        .reason = LINE_RUN_ENTER,

        .filename = *path,
        .starting_line = 1,
        
        .start_location = map->highest_location,
        .highest_location = map->highest_location,

        .finalised = false,

        .lines = NULL,
        .used = 0,
        .allocated = 0,

        .renamed = false,
        .has_parent = false,

        .rename = 0,
        .parent = 0,

        .map = map
    };

    return run;

    return NULL;
}

LineRun* line_map_enter(LineMap* map, Filepath* path, Location loc)
{
    line_map_maybe_realloc(map);
    
    // make the previous map fail upon insertion to help stop errors
    LineRun* highest = &map->runs[map->used - 1];
    line_run_finalise(highest);

    LineRun* run = &map->runs[map->used++];
    *run = (LineRun)
    {
        .reason = LINE_RUN_ENTER,

        .filename = *path,
        .starting_line = 1,
        
        .start_location = map->highest_location,
        .highest_location = map->highest_location,

        .finalised = false,

        .lines = NULL,
        .used = 0,
        .allocated = 0,
        
        .renamed = false,
        .has_parent = true,

        .rename = 0,
        .parent = loc,

        .map = map
    };

    map->include_depth++;

    return run;
}

LineRun* line_map_leave(LineMap* map)
{
    line_map_maybe_realloc(map);

    // make the previous map fail upon insertion to help stop errors
    LineRun* highest = &map->runs[map->used - 1];
    line_run_finalise(highest);

    // Get the previous run
    assert(highest->has_parent && "Must have a parent to leave a line_map");
    LineRun* from = line_run_lookup(map, highest->parent);

    LineRun* run = &map->runs[map->used++];
    *run = (LineRun)
    {
        .reason = LINE_RUN_LEAVE,

        .filename = from->filename,
        
        // get the previous line its current one and calcualte what the line
        // would have been if we wern't rudely interupted
        .starting_line = (from->lines[from->used - 1].current_line + 
                from->lines[from->used - 1].line.num_phyical_lines),
        
        .start_location = map->highest_location,
        .highest_location = map->highest_location,

        .finalised = false,

        .lines = NULL,
        .used = 0,
        .allocated = 0,
        
        .renamed = from->renamed,
        .has_parent = from->has_parent,

        .rename = 0,
        .parent = from->parent,

        .map = map
    };

    assert(map->include_depth && "Map include depth must be >0 to sub from it");
    map->include_depth--;

    return run;
}

LineRun* line_map_line(LineMap* map, Filepath* new_name, uint32_t new_line, Location loc);

LineRun* line_run_lookup(LineMap* map, Location loc)
{
    if (loc >= map->highest_location)
    {
        panic("location given is too high for linemap");

        return NULL;
    }

    // TODO: like above could / should this be optimised to a binary search???
    for (size_t i = 0; i < map->used; i++)
    {
        LineRun* run = &map->runs[i];

        if (loc >= run->start_location && loc < run->highest_location)
        {
            return run;
        }
    }

    panic("bad linemap lookup; failed for unknown reason");

    return NULL;
}

ResolvedLocation line_map_resolve_location(LineMap* map, Location loc)
{
    LineRun* run = line_run_lookup(map, loc);

    return line_run_resolve_location(run, loc);
}
