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

int main(int argc, char** argv)
{
    char test_pgm[] = 
        "int puts(const char*);\n"
        "\n"
        "int main(int argc, char** argv)\n"
        "{\n"
        "   puts(\"Hello, World!\");\n"
        "\n"
        "   return 69;\n"
        "}\n";

    Filepath path = FILEPATH_STATIC_INIT("<builtin>");

    Buffer buff = buffer_from_cstr(test_pgm);
    SourceStream ss = source_stream_from_buffer(&buff);

    LineInfo prev_info = (LineInfo)
    {
        .start_location = 0,
        .end_location = 0,

        .line = {0},

        .current_name = &path,
        .current_line = 0,
    };

    while (!source_stream_at_eof(&ss))
    {
        SourceLine line = source_stream_read_line(&ss);
        
        LineInfo info = (LineInfo)
        {
            .start_location = prev_info.end_location,
            .end_location = prev_info.end_location + line.string.len,

            .line =  line,

            .current_name = prev_info.current_name,
            .current_line = prev_info.current_line + line.num_phyical_lines
        };

        printf("<%s:%u>\n", info.current_name->path, info.current_line);
        printf("<%u:%u>\n", info.start_location, info.end_location);
        printf("%s\n", line.string.ptr);

        prev_info = info;

        // free(line.string.ptr);
    }

    source_stream_close(&ss);
}
