#ifndef LOCATION_MAP_H
#define LOCATION_MAP_H

#include <stdint.h>
#include <stdbool.h>

#include "files/filepath.h"

#include "lex/location.h"
#include "lex/source_line.h"

// TODO: could we change the LineInfo to store a pointer to a sourceline and 
// then store the line else where to prevent relexing or reopening of files?
// TODO: we could also possibly intern filenames or at least try to minimize
// duplicated as the structure is very bloated by them...

// This is inspired by a combinations of gcc's CPPLIB linemaps and Clangs
// SourceLocation system. Thank you!

// A struct to represent the info for a specific line this includes the starting
// and ending locations a pointer to the current name (from a line run) and the
// calculated current line number.
//
// From this struct the column number can be calculated. The line at index 0
// corrosponds to the starting location. Thus from knowing the starting index we
// can the calculate the column from the position.
// 
// This struct should be immutable and and only written to once (upon 
// construction). Additionally, it should be part of a line run for easy access
// to all of the details of it specifically for     binary searching to get the
// location nice and quickly
typedef struct LineInfo {
    Location start_location; // the location we start at
    Location end_location; // the location we end at

    SourceLine line; // The statically allocated line we got

    Filepath* current_name; // The current filename for this line (not owned)
    uint32_t current_line; // The current line number caclulated by the map
} LineInfo;

// A enum to tell us the reason a line run was created. 
//
// LINE_RUN_ENTER: any time a new file is entered to be read from for any reason
// this includes a #include'd file, a file from the command line, and even the
// base file
//
// LINE_RUN_LEAVE: any time a file is left would mostly be done for #include'd
// files
// 
// LINE_RUN_LINE: any time a #line directive is present within a file so that
// we can track where it came from
typedef enum LineRunReason {
    LINE_RUN_ENTER, // Entering a file either via #include or CLI or base...
    LINE_RUN_LEAVE, // Leave a file for whatever reason e.g. include ending
    LINE_RUN_LINE // using a #line directive to modify the line number 
} LineRunReason;

// A structure to represent a 'run' of lines in source. This stores a group of
// lines together in order to make searching for a line faster.
//
// To find if a location is within the line run we check:
//     start_location <= LOCATION < highest_location
// if the above is true then it is somewhere in the linerun. Once we determined
// if a location is within a run we can then find the specific LineInfo it is
// from. This is done by a similar process.
//
// if the line run is finalised then we cannot add anymore lines to it
//
//
// TODO: The finer details of the 'renamer' field are still being worked out
typedef struct LineRun {
    LineRunReason reason; // The reason this line run was created

    Filepath filename; // The statically allocated filename
    uint32_t starting_line; // The line number we give lines[0].current_line

    Location start_location; // The location this starts at
    Location highest_location; // the current highest location given out

    bool finalised; // If true no more lines can be added

    LineInfo* lines; // The lines actually stored in this run
    size_t used; // number of lines used
    size_t allocated; // number of lines we allocated for

    struct LineRun* renamer; // only non-NULL if the original LineRun got renamed
    struct LineRun* parent; // The parent (includer) line run

    struct LineMap* map; // The overall linemap for the linerun
} LineRun;

typedef struct LineMap {
    LineRun* runs;
    size_t used;
    size_t allocated;

    Location highest_location;

    size_t include_depth; // Current depth of the include stack
} LineMap;

// TODO: create some kind of macro map to track macro expansion

LineMap line_map_create(void);
void line_map_free(LineMap* map);

LineRun* line_map_enter(LineMap* map, Filepath* path, uint32_t start_line);
LineRun* line_map_leave(LineMap* map);
LineRun* line_map_line(LineMap* map, Filepath* new_name, uint32_t new_line);

// Add a line to a line run and return the base location given for that 
// particular line.
Location line_run_add_line(LineRun* run, SourceLine line);

// Finalise a line run making it not able to ever be added to again
void line_run_finalise(LineRun* run);

// Free a line run and all of the memory asociated with it
void line_run_free(LineRun* run);

// Resolve a location from a line run. It must contain said location, otherwise
// a panic will be run
ResolvedLocation line_run_resolve_location(LineRun* run, Location loc);

#endif /* LOCATION_MAP_H */
