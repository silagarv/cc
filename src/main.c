// #include "driver/driver.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "util/xmalloc.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    char test_pgm[] = 
        "int printf(const char*, ...);\n"
        "\n"
        "int main(int argc, char** argv)\n"
        "{\n"
        "   printf(\"Hello, World!\");\n"
        "\n"
        "   return 69;\n"
        "}\n";

    size_t len = sizeof(test_pgm);
    char* alloced = xmalloc(sizeof(char) * len);
    memcpy(alloced, test_pgm, len);

    SourceStream ss = source_stream(alloced, len);

    while (!source_stream_at_eof(&ss))
    {
        SourceLine line = source_stream_read_line(&ss);
        printf("%.*s", (int) line.string.len, line.string.ptr);
        // printf("Number lines: %u\n", line.num_phyical_lines);
        free(line.string.ptr);
    }

    source_stream_close(&ss);
}
