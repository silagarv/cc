#include "files.h"

#include "util/panic.h"

#include <sys/stat.h>

bool is_directory(StaticString* maybe_dir)
{
    struct stat stat_buff;
    if (stat(maybe_dir->ptr, &stat_buff) != 0)
    {
        return false;
    }

    if (!S_ISDIR(stat_buff.st_mode))
    {
        return false;
    }

    return true;
}

bool is_file(StaticString* maybe_file)
{
    struct stat stat_buff;
    if (stat(maybe_file->ptr, &stat_buff) != 0)
    {
        return false;
    }

    if (!S_ISREG(stat_buff.st_mode))
    {
        return false;
    }

    return true;
}

FILE* open_file(StaticString* filename)
{
    if (!is_file(filename))
    {
        return NULL;
    }

    FILE* file = fopen(filename->ptr, "r");

    if (!file)
    {
        panic("we though it was a file but fopen fails");
    }

    return file;
}
