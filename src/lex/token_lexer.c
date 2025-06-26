#include "token_lexer.h"

#include <stddef.h>
#include <stdbool.h>

#include "lex/source_line.h"
#include "lex/source_stream.h"

typedef struct TokenLexer {
    SourceStream stream;

    SourceLine current_line;

    char* current_line_ptr;
    size_t current_line_len;
    size_t current_line_pos;

    bool has_line;
    bool is_start_of_line;
    bool preprocessing_directive;
    bool can_lex_header;
} TokenLexer;
