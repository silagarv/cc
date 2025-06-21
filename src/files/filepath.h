#ifndef FILEPATH_H
#define FILEPATH_H

#include <stddef.h>
#include <linux/limits.h>

typedef struct Filepath {
    char path[PATH_MAX];
    size_t len;
} Filepath;

// Filepath* filepath_from_string(const char* str);

#endif /* FILEPATH_H */
