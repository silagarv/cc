#ifndef LEXER_H
#define LEXER_H

#include "util/buffer.h"

#include "lex/location.h"
#include "lex/token.h"

typedef struct Lexer {
    const char* buffer_start;
    const char* buffer_end;
    
    char* current_ptr;

    Location start_loc;

    bool start_of_line;
    bool lexing_directive;
    bool can_lex_header;
} Lexer;

/* create a lexer. No heap allocations needed, so can just fade away into the
 * stack once we are done with it. (Since we do not take ownership of the 
 * buffer given).
 */
Lexer lexer(const char* buffer_start, const char* buffer_end, Location start_loc);
Lexer lexer_from_buffer(Buffer buffer, Location start_loc);

bool lexer_get_next(Lexer* lexer, Token* tok);

#endif /* LEXER_H */
