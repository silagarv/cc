#include "parse/line.h"
#include "parse/input.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/arena.h"

#include "adt/vector.h"

int main(int argc, char** argv)
{
    InputManager* input_manager = input_manager_new();
    input_manager_finish_setup(input_manager);

    input_manager_print_include_paths(input_manager);

    Input* in = input_new(fopen(argv[1], "r"), argv[1], NULL);



    input_delete(in);

    input_manager_delete(input_manager);

    exit(EXIT_SUCCESS);
}
