// #include "driver/driver.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/token_lexer.h"
#include "lex/token.h"
#include "util/buffer.h"
#include "util/xmalloc.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "lex/location_map.h"

// char test_pgm[] = {
//         #embed "../sqlite-autoconf-3490100/sqlite3.c"
// };

int main(int argc, char** argv)
{
    char test_pgm[] = 
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

    LineRun fake_run;
    fake_run.reason = LINE_RUN_ENTER,
    fake_run.filename = path;
    fake_run.starting_line = 1;
    fake_run.start_location = 0;
    fake_run.highest_location = 0;
    fake_run.finalised = false;

    fake_run.lines = NULL;
    fake_run.used = 0;
    fake_run.allocated = 0;

    fake_run.renamer = NULL;
    fake_run.parent = NULL;
    fake_run.map = NULL;

    while (!source_stream_at_eof(&ss))
    {
        SourceLine line = source_stream_read_line(&ss);
        
        Location loc = line_run_add_line(&fake_run, line);
        LineInfo* highest = &fake_run.lines[fake_run.used - 1];

        printf("<%s:%u>\n", highest->current_name->path, highest->current_line);
        printf("<%u:%u>\n", highest->start_location, highest->end_location);
        printf("%s\n", highest->line.string.ptr);
    }

    // ResolvedLocation loc = line_run_resolve_location(&fake_run, 1234143);
    // printf("%s:%u:%u\n", loc.name->path, loc.line, loc.col);

    line_run_free(&fake_run);

    source_stream_close(&ss);
}
