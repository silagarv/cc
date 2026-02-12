#ifndef FILEPATH_H
#define FILEPATH_H

#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct Filepath {
    char path[FILENAME_MAX];
    size_t len;
} Filepath;

#define FILEPATH_STATIC_INIT(array) \
        ((Filepath) {.path = array, .len = sizeof(array) - 1})

bool filepath_is(const Filepath* path, const char* is);

Filepath filepath_from_cstring(const char* path);

const char* filepath_get_cstr(const Filepath* path);

bool filepath_is_absolute(const Filepath* path);
bool filepath_is_directory(const Filepath* path);

#endif /* FILEPATH_H */
