#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "files/source_file.h"
#include "lex/location.h"
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
        // #embed "src/parse/parser.c"
//         #embed "../../external/c-testsuite/tests/single-exec/00004.c"
    ,'\0'
};

#include "driver/target.h"

#include "lex/preprocessor.h"

#include <assert.h>
#include "files/filepath.h"

int main(int argc, char** argv)
{
    diag_init();

    // char test_pgm[] =
    //     "L\"m\?eow\"\\\n"
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

    const Target t = target_create_x86_64_linux();

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    SourceFile file;
    file.contents = test_pgm;
    file.end_contents = test_pgm + sizeof(test_pgm) - 1;
    file.name = path;
    file.contents_size = sizeof(test_pgm) - 1;

    LineMap map;
    line_map(&map, &file, 0);

    Lexer l = lexer(test_pgm, test_pgm + sizeof(test_pgm) - 1, 0);

    // Token tok;
    // while (lexer_get_next(&l, &tok))
    // {
    //     ResolvedLocation loc = line_map_resolve_location(&map, tok.loc);
    //     ResolvedLocation loc_end = line_map_resolve_location(&map, tok.end);

    //     printf("%s:%u:%u\n", loc.name->path, loc.line, loc.col);
    //     printf("%s:%u:%u\n", loc_end.name->path, loc_end.line, loc_end.col);
        
    //     printf("%s\n", token_get_string(&tok));
    //     printf("%s\n\n", token_type_get_name(tok.type));
    // }

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
        
        Token* token =  &tokens.tokens[tokens.used];

        lexer_get_next(&l, token);

        tokens.used++;

        if (token->type == TOKEN_EOF)
        {
            break;
        }
    }

    TokenStream stream = token_list_to_stream(&tokens);
    parse_translation_unit(&stream, &map);

    for (size_t i = 0; i < tokens.used; i++)
    {
        token_free_data(&tokens.tokens[i]);
    }
    free(tokens.tokens);
            
    line_map_delete(&map);

    // Filepath cwd;
    // filepath_get_current_path(&cwd);

    // printf("%s\n", cwd.path);
                    
                    // // token_lexer_close(&lexer);
    // // line_map_free(&map);
}
