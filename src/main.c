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
    Line* line = input->current_line;
    do
    {
        printf("--> %s:%u\n--> %s:%u\n%s\n", line->loc.filename, line->loc.line_no, 
                input->location.filename, input->location.line_no, line->line_buffer);
        input_set_line(input, 10);       
    } while ((line = input_get_next_line(input)) != NULL);

    // Find a line
    // Line* line = input_find_real_line(input, 1);
    // if (line)
    // {
    //     printf("--> %s:%u\n--> %s:%u\n%s\n", line->loc.filename, line->loc.line_no, 
    //             line->real_loc.filename, line->real_loc.line_no, line->line_buffer);
    // }

    input_delete(input);

    exit(EXIT_SUCCESS);
}
