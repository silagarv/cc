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
        "32 * 25 / *&*&*&*&*32 + 14 * x; 10 + 15 * &20 - sizeof 19;\n";
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

        // ResolvedLocation loc = line_map_resolve_location(&map, tok->loc);
        // printf("%s:%u:%u\n", loc.name->path, loc.line, loc.col);
        // printf("%s\n", token_type_get_name(tok->type));
        // if (token_has_opt_value(tok))
        // {
        //     printf("%.*s\n\n", (int) tok->opt_value.len, token_get_string(tok));
        // }
        // else
        // {
        //     printf("%s\n\n", token_get_name(tok));
        // }
    }

    TokenStream stream = token_list_to_stream(&tokens);
    parse_translation_unit(&stream, &map);

    


    free(tokens.tokens);

    // Token tok;
    // tok.type = TOKEN_UNKNOWN;
    // while (true)
    // {
    //     tok = token_lexer_get_next(&lexer);

    //     if (tok.type == TOKEN_UNKNOWN)
    //     {
    //         panic("unknown token");
    //     }

    //     if (tok.type == TOKEN_EOF)
    //     {
    //         break;
    //     }

    //     ResolvedLocation loc = line_map_resolve_location(&map, tok.loc);
    //     printf("%s:%u:%u\n", loc.name->path, loc.line, loc.col);
    //     printf("%s\n", token_type_get_name(tok.type));
    //     if (token_has_opt_value(&tok))
    //     {
    //         printf("%.*s\n\n", (int) tok.opt_value.len, token_get_string(&tok));
    //     }
    //     else
    //     {
    //         printf("%s\n\n", token_get_name(&tok));
    //     }
    //     // printf("%s\n\n", (tok.start_of_line) ? "start of line" : "not start of line");
    // }

    token_lexer_close(&lexer);
    line_map_free(&map);
}
