
#include <stdlib.h>//shutup clangd

#include <stdio.h>

#include "frontend/source.h"
#include "frontend/line_map.h"
#include "frontend/location_map.h"
#include "frontend/location_resolver.h"

#include "diagnostic/diagnostic.h"



int main(int argc, char** argv)
{
    diagnostics_init();

    if (argc == 1)
    {
        fatal_error("no input file");
        exit(1);
    }

    LocationResolver* resolver = location_resolver_new();
    Source* src = source_new(argv[1], fopen(argv[1], "r"));

    Line* line;
    while ((line = source_read_line(src)) != NULL)
    {
        location_resolver_add_line(resolver, line);
    }

    location_resolver_free(resolver);
    source_free(src);
}
