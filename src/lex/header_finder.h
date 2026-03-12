#ifndef HEADER_FINDER_H
#define HEADER_FINDER_H

#include <stdbool.h>

#include "util/arena.h"

#include "files/location.h"
#include "files/source_manager.h"

#include "files/filepath.h"

// The type of directory entry that we want to add to the HeaderFinder
typedef enum DirectoryEntryType {
    DIRECTORY_ENTRY_QUOTE, // From -iquote on the command line
    DIRECTORY_ENTRY_ANGLED, // Specified by -I on the command line
    DIRECTORY_ENTRY_SYSTEM, // Specified by -isystem on the command line
    DIRECTORY_ENTRY_AFTER // Specified by -idirafter on command line
} DirectoryEntryType;

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

    // The directory entries we have in order that they should be searched for
    DirectoryEntry* quote; // specifier by -iquote on the command line
    DirectoryEntry* angled; // specifier by -I
    DirectoryEntry* system; // specific by -isystem
    DirectoryEntry* after; // specifier by -idirafter
} HeaderFinder;

DirectoryEntry* directory_entry_create(Arena allocator, const char* path,
        bool system);
void directory_entry_set_next(DirectoryEntry* entry, DirectoryEntry* next);

Filepath* directory_entry_filepath(const DirectoryEntry* entry);
bool directory_entry_is_system(const DirectoryEntry* entry);
DirectoryEntry* directory_entry_next(const DirectoryEntry* entry);

// FIXME: will need to give the header finder some kind of struct which contains
// FIXME: all of the options we want to add for header search paths...
HeaderFinder header_finder_create(void);
void header_finder_delete(HeaderFinder* hf);

// Get the directory entries for the header finder specified by the type we want
DirectoryEntry* header_finder_iter(HeaderFinder* hf, DirectoryEntryType start);

// The function that does the heavy lifting of the header finder. Brings 
// together quite a few things like the handling of the includes and other such
// stuff and the handling of our filepaths.
// TODO: will need to add a second output of the current directory that the 
// TODO: header file was actually found at.
bool header_finder_try_find_include(HeaderFinder* hf, SourceManager* sm,
        const Filepath* current_path, DirectoryEntry* current_directory,
        const Filepath* path, bool angled, Location include_loc,
        SourceFile** include);

#endif /* HEADER_FINDER_H */
