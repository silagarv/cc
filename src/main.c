#include "parse/line.h"
#include "parse/input.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/arena.h"

#include "adt/vector.h"

int main(int argc, char** argv)
{
    Input* input = input_new(fopen(argv[1], "r"), argv[1], NULL);

    if (!input)
    {
        exit(EXIT_FAILURE);
    }

    // Get all of the lines...
    Line* make_line;
    while ((make_line = input_get_next_line(input)) != NULL)
    {
        // printf("--> %s:%u\n--> %s:%u\n%s\n", line->loc.filename, line->loc.line_no, 
        //         line->real_loc.filename, line->real_loc.line_no, line->line_buffer);
    }

    // Find a line
    Line* line = input_find_real_line(input, 46);
    if (line)
    {
        printf("--> %s:%u\n--> %s:%u\n%s\n", line->loc.filename, line->loc.line_no, 
                line->real_loc.filename, line->real_loc.line_no, line->line_buffer);
    }

    input_delete(input);

    exit(EXIT_SUCCESS);
}
