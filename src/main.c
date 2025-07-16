#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/token_lexer.h"
#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/parser.h"

#include "util/arena.h"

// char test_pgm[] = {
//         // #embed "/usr/include/stdio.h"
//         // #embed "sqlite3.c.testing.i"
//         // #embed "../../external/sqlite3/sqlite3.i"
//         #embed "../../external/c-testsuite/tests/single-exec/00004.c"
// };

#include "parse/type.h"

#include <assert.h>

int main(int argc, char** argv)
{
    diag_init();

    char test_pgm[] =
        "const int;\n";    

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

    // TokenStream stream = token_list_to_stream(&tokens);
    // parse_translation_unit(&stream, &map);
    free(tokens.tokens);

    // token_lexer_close(&lexer);
    // line_map_free(&map);
}
