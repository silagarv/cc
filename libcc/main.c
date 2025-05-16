#include "adt/vector.h"

#include "adt/string_map.h"

#include "pp/line.h"
#include "pp/source.h"
#include <stdio.h>

int main(int argc, char** argv)
{
    FILE* fp = fopen(argv[1], "r");

    Source* current = NULL;

    current = source_push(current, fp, argv[1], NULL);

    Line line;
    while (line_get_next_from_source(current, &line))
    {
        // printf("%u: %s", line.loc.line_no, line.line.buffer);
        line_delete(&line);
    }

    // int c;
    // while ((c = source_read_char(current)) != EOF)
    // {
    //     printf("%s:%u: %c\n", current->loc.filename, current->loc.line_no, c);
    // }
    
    source_delete(current);

    return 0;
}
