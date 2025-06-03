#include <stdlib.h>//shutup clangd

#include <stdio.h>

#include "preprocessor/buffered_source.h"
#include "preprocessor/line.h"

int main(int argc, char** argv)
{
    BufferedSource* source = buffered_source_new(fopen("src/main.c", "r"), 
            "src/main.c", NULL);
    
    Line line;
    while (line_read_from_buffered_source(source, &line))
    {
        printf("%s:%u:\n", line.source_real_name, line.real_line_no);
        printf("%s", line_get_ptr(&line));
        line_free(&line);
    }

    buffered_source_free(source);
}
