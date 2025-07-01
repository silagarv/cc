#ifndef FILEPATH_H
#define FILEPATH_H

#include <stddef.h>
#include <stdbool.h>

#include <linux/limits.h>

typedef struct Filepath {
    char path[PATH_MAX];
    size_t len;
} Filepath;

#define FILEPATH_STATIC_INIT(array) ((Filepath) {.path = array, .len = sizeof(array) - 1})

Filepath filepath_from_cstring(const char* path);

bool filepath_is_absolute(const Filepath* path);

// Filepath* filepath_from_string(const char* str);

#endif /* FILEPATH_H */
