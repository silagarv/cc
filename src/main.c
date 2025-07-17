#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/parser.h"
#include "lex/token.h"

#include "util/arena.h"

#include "lex/lexer.h"

char test_pgm[] = {
        // #embed "/usr/include/stdio.h"
        // #embed "../../external/sqlite3/sqlite3.c"
        #embed "test.c"
//         #embed "../../external/c-testsuite/tests/single-exec/00004.c"
    ,'\0'
};

#include <assert.h>

int main(int argc, char** argv)
{
    diag_init();

    // char test_pgm[] =
    //     "L\"m\?eow\\\n\""
    //     "L'a\n"
    //     "   \t const int;\\\n"
    //     "meow poo poo\n"
    //     "\\\n"
    //     "inc\\\n"
    //     "lude stdio h\n"
    //     "a\\\n"
    //     "\\\n"
    //     "\\\n"
    //     "b\n"
    //     "meow meow 123.123.12.34.1aE+12\n"
    //     "a?\?/\n"
    //     "b\n"
    //     "meow\n"
    //     ".\\\n.\\\n.\n"
    //     "%:include";

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    Lexer l = lexer(test_pgm, test_pgm + sizeof(test_pgm) - 1, 0);

    Token tok;
    while (lexer_get_next(&l, &tok))
    {
        printf("%s\n", token_get_string(&tok));
        printf("%s\n", token_type_get_name(tok.type));

        if (token_has_opt_value(&tok))
        {
            free(tok.opt_value.ptr);
        }
    }
    

    // LineMap map = line_map_create();
    // LineRun* run = line_map_start(&map, &path);
    // TokenLexer lexer = token_lexer_create(ss, run);

    // TokenList tokens = (TokenList)
    // {
    //     .tokens = xmalloc(sizeof(Token)),
    //     .used = 0,
    //     .allocated = 1
    // };

    // while (true)
    // {
    //     if (tokens.used == tokens.allocated)
    //     {
    //         tokens.allocated *= 2;
    //         tokens.tokens = xrealloc(tokens.tokens, sizeof(Token) * tokens.allocated);
    //     }

    //     tokens.tokens[tokens.used++] = token_lexer_get_next(&lexer);

    //     Token* tok = &tokens.tokens[tokens.used - 1];

    //     if (tok->type == TOKEN_EOF)
    //     {
    //         break;
    //     }
    // }

    // // TokenStream stream = token_list_to_stream(&tokens);
    // // parse_translation_unit(&stream, &map);
    // free(tokens.tokens);

    // // token_lexer_close(&lexer);
    // // line_map_free(&map);
}
