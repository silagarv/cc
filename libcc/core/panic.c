#include "panic.h"

#include <stdlib.h>
#include <stdio.h>

void panic(const char* msg)
{
    fputs("internal compiler error: ", stderr);
    fputs(msg, stderr);
    fputc('\n', stderr);
    fflush(stderr);
    abort();
}
