#ifndef FILEPATH_H
#define FILEPATH_H

#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#define FILEPATH_LEN (FILENAME_MAX)

typedef struct Filepath {
    char path[FILEPATH_LEN];
    size_t len;
} Filepath;

#define FILEPATH_STATIC_INIT(array) \
        ((Filepath) {.path = array, .len = sizeof(array) - 1})

// Filepath routines that do not require real paths

bool filepath_is(const Filepath* path, const char* is);

Filepath filepath_from_cstring(const char* path);

const char* filepath_get_cstr(const Filepath* path);

bool filepath_is_absolute(const Filepath* path);
bool filepath_is_directory(const Filepath* path);

// Filepath routines which attempt to use real paths
bool filepath_get_real(const Filepath* input, Filepath* output);

#endif /* FILEPATH_H */
