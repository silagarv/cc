#ifndef FILEPATH_H
#define FILEPATH_H

#include <stddef.h>
#include <stdbool.h>

#include <linux/limits.h>

#define FILEPATH_MAX_LEN PATH_MAX

typedef struct Filepath {
    char path[FILEPATH_MAX_LEN];
    size_t len;
} Filepath;

#define FILEPATH_STATIC_INIT(array) \
        ((Filepath) {.path = array, .len = sizeof(array) - 1})

bool filepath_get_current_path(Filepath* path);

Filepath filepath_from_cstring(const char* path);

bool filepath_is_absolute(const Filepath* path);
bool filepath_is_directory(const Filepath* path);

#endif /* FILEPATH_H */
