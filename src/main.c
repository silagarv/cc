// #include "driver/driver.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/token_lexer.h"
#include "lex/token.h"
#include "lex/location_map.h"

#include "util/panic.h"
#include "util/buffer.h"
#include "util/xmalloc.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// char test_pgm[] = {
//         #embed "/usr/include/stdio.h"
        // #embed "../sqlite-autoconf-3490100/sqlite3.c"
// };

int main(int argc, char** argv)
{
    char test_pgm[] = 
        // "#include <stdio.h>\n"
        // "// Declaration of puts \n"
        // "\nint puts(const char* str);\n"
        // "\n"
        // "/* Test multiline comment\n"
        // " * the comment that keeps giving */\n"
        // "\n"
        "int main()\n"
        "{\n"
        "    int x = 34;\n"
        "    int y = x + 35;\n"
        "    return y;\n"
        "    auto int a = 0;\n"
        "}\n";

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    Buffer buff = buffer_from_cstr(test_pgm);
    SourceStream ss = source_stream_from_buffer(&buff);
    
    LineMap map = line_map_create();
    LineRun* run = line_map_start(&map, &path);
    
    TokenLexer lexer = token_lexer_create(ss, run);

    Token tok;
    tok.type = TOKEN_UNKNOWN;
    while (true)
    {
        tok = token_lexer_get_next(&lexer);

        if (tok.type == TOKEN_UNKNOWN)
        {
            panic("unknown token");
        }

        if (tok.type == TOKEN_EOF)
        {
            break;
        }

        // ResolvedLocation loc = line_map_resolve_location(&map, tok.loc);
        // printf("%s:%u:%u\n", loc.name->path, loc.line, loc.col);
        printf("%s\n", token_type_get_name(tok.type));
        // printf("%.*s\n\n", (int) tok.opt_value.len, tok.opt_value.start);
        // printf("%s\n\n", (tok.start_of_line) ? "start of line" : "not start of line");
    }

    token_lexer_close(&lexer);
    
    // line_map_leave(&map);

    // LineRun* run = line_map_start(&map, &path);

    // while (!source_stream_at_eof(&ss))
    // {
    //     SourceLine line = source_stream_read_line(&ss);
    //     Location loc = line_run_add_line(run, line);
        
    //     LineInfo* highest = &run->lines[run->used - 1];

    //     ResolvedLineLocation lloc = line_run_resolve_line_location(run, loc);

    //     printf("<%s:%u>\n", lloc.path->path, lloc.line);
    //     printf("<%u:%u>\n", highest->start_location, highest->end_location);
    //     printf("%s", highest->line.string.ptr);
    // }

    // source_stream_close(&ss);

    line_map_free(&map);
}
    
