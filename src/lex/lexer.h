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


// Create a lexer on the stack, no heap allocations needed for it at all
Lexer lexer(const char* buffer_start, const char* buffer_end, Location start_loc);
Lexer lexer_from_buffer(Buffer buffer, Location start_loc);

// Get the next token from the lexer advancing it's position
bool lexer_get_next(Lexer* lexer, Token* tok);

// Get the spelling of token and add it to the buffer given
void token_get_spelling(const Token* token, Buffer buffer);

// Stringify the given token adding it to the buffer
void token_stringify(const Token* token, Buffer buffer);

#endif /* LEXER_H */
