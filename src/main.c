#include <stdlib.h>//shutup clangd

#include <stdio.h>

#include "diagnostic/diagnostic.h"

#include "preprocessor/buffered_source.h"
#include "preprocessor/line.h"

#include "util/static_string.h"
#include "preprocessor/token.h"

int main(int argc, char** argv)
{
    diagnostics_init();

    // TODO: add argument parsing here

    // if (argc == 1)
    // {
    //     fatal_error("no input files");
    //     exit(1);
    // }

    BufferedSource* source = NULL;
    BufferedSource* source_new = buffered_source_from_file(fopen("test.c", "r"), 
            "test.c");
    source = buffered_source_push(source, source_new);
    
    Line line;
    while (line_read_from_buffered_source(source, &line))
    {
        printf("%s:%u:\n", line.source_real_name, line.real_line_no);
        printf("%s", line_get_ptr(&line));
        line_free(&line);
    }

    buffered_source_free(source);
}
