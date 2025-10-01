#ifndef LEXER_H
#define LEXER_H

#include "files/location.h"
#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

typedef struct Lexer {
    IdentifierTable* identifiers;

    const char* buffer_start;
    const char* buffer_end;
    
    char* current_ptr;

    Location start_loc;

    bool start_of_line;
    bool lexing_directive;
    bool can_lex_header;
} Lexer;

// Create a lexer on the stack, no heap allocations needed for it at all
Lexer lexer(IdentifierTable* identifiers, const char* buffer_start,
        const char* buffer_end, Location start_loc);

Lexer lexer_create(IdentifierTable* identifiers, SourceFile* source);

// Get the next token from the lexer advancing it's position
bool lexer_get_next(Lexer* lexer, Token* tok);

// Get the token type of the next token in the stream without advancing the
// lexers position.
TokenType lexer_get_next_next_type(Lexer* lexer);

#endif /* LEXER_H */
