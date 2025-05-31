#include "panic.h"

#include <stdlib.h>
#include <stdio.h>

void panic(const char* msg)
{
    fputs("internal compiler error: ", stderr);
    fputs(msg, stderr);
    fputs("\n\n", stderr);
    fflush(stderr);
    abort();
}
