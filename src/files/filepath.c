#include "filepath.h"

#include <stddef.h>
#include <string.h>

bool filepath_is(const Filepath* path, const char* is)
{
    size_t len = strlen(is);
    if (path->len != len)
    {
        return false;
    }

    return !strncmp(path->path, is, len);
}

Filepath filepath_from_cstring(const char* path)
{
    const size_t len = strlen(path);

    Filepath fp;
    memcpy(fp.path, path, len + 1);
    fp.len = len;

    return fp;
}

const char* filepath_get_cstr(const Filepath* path)
{
    return path->path;
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

