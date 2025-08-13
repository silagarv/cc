#ifndef HEADER_FINDER_H
#define HEADER_FINDER_H

#include <stdbool.h>

#include "files/filepath.h"

// A structure to represent a directory entry that we will use to find the
// given header. This is stored within the header finder structure and should
// not be used directly
typedef struct DirectoryEntry {
    Filepath directory_path; // the directories path
    bool is_system; // are we a system directory
} DirectoryEntry;

// This structure is used to find a given header based on a variety of factors
// so that we are actually able to process includes within the source file as
// we are given them.
typedef struct HeaderFinder {
    DirectoryEntry* entries;
} HeaderFinder;

HeaderFinder header_finder_create(void);

void header_finder_delete(HeaderFinder* hf);

// Add a directory path to the header finder
void header_finder_add_directory(HeaderFinder* hf, Filepath path);

#endif /* HEADER_FINDER_H */
