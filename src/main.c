#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "files/file_buffer.h"
#include "files/source_file.h"
#include "files/location.h"
#include "files/source_manager.h"
#include "util/hash_map.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "lex/token.h"
#include "files/line_map.h"

#include "parse/parser.h"
#include "lex/token.h"

#include "util/arena.h"

#include "lex/lexer.h"

char test_pgm[] = {
        // #embed "/usr/include/stdio.h"
        // #embed "../../external/sqlite3/sqlite3.i"
        #embed "test.c"
        // #embed "../sqlite-autoconf-3490100/sqlite3.c"
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

    const Target t = target_create_x86_64_linux();

    Filepath path = FILEPATH_STATIC_INIT("test_pgm.c");

    Buffer buffer = buffer_from_format("%.*s", (int) sizeof(test_pgm), test_pgm);
    SourceFile* file = source_file_create(0, file_buffer_from_buffer(path, buffer), 0);

    LineMap map;
    line_map(&map, file, 0);

    Lexer l = lexer(test_pgm, test_pgm + sizeof(test_pgm) - 1, 0);

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
    // parse_translation_unit(&stream, &map);

    for (size_t i = 0; i < tokens.used; i++)
    {
        token_free_data(&tokens.tokens[i]);
    }
    free(tokens.tokens);
            
    line_map_delete(&map);

    file_buffer_free(file->file_buffer);
    source_file_free(file);
}
