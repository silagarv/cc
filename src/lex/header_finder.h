#ifndef HEADER_FINDER_H
#define HEADER_FINDER_H

#include <stdbool.h>

#include "util/arena.h"

#include "files/location.h"
#include "files/source_manager.h"

#include "files/filepath.h"

// A structure to represent a directory entry that we will use to find the
// given header. This is stored within the header finder structure and should
// not be used directly
typedef struct DirectoryEntry {
    Filepath directory_path; // the directories path
    bool is_system; // are we a system directory
    struct DirectoryEntry* next; // The next entry in this chain
} DirectoryEntry;

// This structure is used to find a given header based on a variety of factors
// so that we are actually able to process includes within the source file as
// we are given them.
typedef struct HeaderFinder {
    Arena allocator; // The allocator for this header finder, to enable the easy
                     // creation and freeing of header entries

    // The directory entries we have in order.
    DirectoryEntry* quote; // specifier by -iquote on the command line
    DirectoryEntry* bigi; // specifier by -I
    DirectoryEntry* system; // specific by -isystem
    DirectoryEntry* dirafter; // specifier by -idirafter
} HeaderFinder;

HeaderFinder header_finder_create(void);
void header_finder_delete(HeaderFinder* hf);

// Add a directory path to the header finder
void header_finder_add_directory(HeaderFinder* hf, const Filepath* path);

// The function that does the heavy lifting of the header finder. Brings 
// together quite a few things like the handling of the includes and other such
// stuff and the handling of our filepaths.
bool header_finder_try_find_include(HeaderFinder* hf, SourceManager* sm,
        const Filepath* current_path, DirectoryEntry* current_directory,
        const Filepath* path, bool angled, Location include_loc,
        SourceFile** include);

#endif /* HEADER_FINDER_H */
