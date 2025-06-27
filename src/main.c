// #include "driver/driver.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/token_lexer.h"
#include "lex/token.h"
#include "lex/location_map.h"

#include "util/buffer.h"
#include "util/xmalloc.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// char test_pgm2[] = {
//         #embed "/usr/include/stdio.h"
//         // #embed "../sqlite-autoconf-3490100/sqlite3.c"
// };

int main(int argc, char** argv)
{
    char test_pgm[] = 
        // "#include <stdio.h>\n"
        // "\n"
        "int puts(const char*);\n"
        "\n"
        "int main(int argc, char** argv)\n"
        "{\n"
        "    puts(\"Hello, World!\");\n"
        "\n"
        "    return 69;\n"
        "}\n";

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    Buffer buff = buffer_from_cstr(test_pgm);
    SourceStream ss = source_stream_from_buffer(&buff);
    
    LineMap map = line_map_create();
    LineRun* run = line_map_start(&map, &path);

    while (!source_stream_at_eof(&ss))
    {
        SourceLine line = source_stream_read_line(&ss);
        Location loc = line_run_add_line(run, line);
        
        LineInfo* highest = &run->lines[run->used - 1];

        ResolvedLineLocation lloc = line_run_resolve_line_location(run, loc);

        // printf("<%s:%u>\n", lloc.path->path, lloc.line);
        // printf("<%u:%u>\n", highest->start_location, highest->end_location);
        // printf("%s", highest->line.string.ptr);
    }

    source_stream_close(&ss);

    line_map_free(&map);
}
    
