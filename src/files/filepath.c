#include "filepath.h"

#include <stddef.h>
#include <string.h>

#include <linux/limits.h>

Filepath filepath_from_cstring(const char* path)
{
    const size_t len = strlen(path);

    Filepath fp;
    memcpy(fp.path, path, len);
    fp.len = len;

    return fp;
}

bool filepath_is_absolute(const Filepath* path)
{
    return (path->path[0] == '/');
}

bool filepath_is_directory(const Filepath* path)
{
    const char* last_slash = strrchr(path->path, '/');
    const size_t offset = last_slash - path->path;

    if (offset == path->len - 1)
    {
        return true;
    }
    
    return false;
}

