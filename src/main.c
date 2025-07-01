#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/xmalloc.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/token_lexer.h"
#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/parser.h"

// char test_pgm[] = {
//         // #embed "/usr/include/stdio.h"
//         // #embed "sqlite3.c.testing.i"
//         // #embed "../sqlite-autoconf-3490100/sqlite3.c"
// };

int main(int argc, char** argv)
{
    diag_init();
    
    char test_pgm[] =
        // "test[32][46];\n"
        // "function_call();\n"
        // "nest.the->mf.structs[32](*abc, a, 2, 9, 10 == 11 ? 1 : 0);\n";
        "switch (123 + 10) abc: break;\n"
        "goto abc; {{{{{{{goto _;}}}}}}}\n"
        "for (a != 0; a != 10; a++) { a + 30; }\n";
        // "int main();\n"
        // "\n"
        // "int main()\n"
        // "{\n"
        // "    return 69;\n"
        // "};\n";

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    Buffer buff = buffer_from_cstr(test_pgm);
    SourceStream ss = source_stream_from_buffer(&buff);
    
    LineMap map = line_map_create();
    LineRun* run = line_map_start(&map, &path);
    TokenLexer lexer = token_lexer_create(ss, run);

    TokenList tokens = (TokenList)
    {
        .tokens = xmalloc(sizeof(Token)),
        .used = 0,
        .allocated = 1
    };

    while (true)
    {
        if (tokens.used == tokens.allocated)
        {
            tokens.allocated *= 2;
            tokens.tokens = xrealloc(tokens.tokens, sizeof(Token) * tokens.allocated);
        }

        tokens.tokens[tokens.used++] = token_lexer_get_next(&lexer);

        Token* tok = &tokens.tokens[tokens.used - 1];

        if (tok->type == TOKEN_EOF)
        {
            break;
        }
    }

    TokenStream stream = token_list_to_stream(&tokens);
    parse_translation_unit(&stream, &map);
    free(tokens.tokens);

    token_lexer_close(&lexer);
    line_map_free(&map);
}
