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

Filepath filepath_from_cstring(const char* path);

bool filepath_is_absolute(const Filepath* path);
bool filepath_is_directory(const Filepath* path);

#endif /* FILEPATH_H */
