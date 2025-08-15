#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "files/file_manager.h"
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

#include "driver/target.h"

#include "lex/preprocessor.h"

#include <assert.h>
#include "files/filepath.h"

int main(int argc, char** argv)
{
    diag_init();

    Filepath path = FILEPATH_STATIC_INIT("test.c");
    FileBuffer* fb = file_buffer_try_get(path);
    if (!fb)
    {
        panic("cannot find file!");
    }

    SourceFile* file = source_file_create(0, 1, 0, fb);

    LineMap map = line_map_create(fb, 0);

    Lexer l = lexer(fb->buffer_start, fb->buffer_end, 0);

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
