#ifndef LEXER_H
#define LEXER_H

#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/location.h"
#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

typedef struct Lexer {
    DiagnosticManager* dm;
    LangOptions* lang;
    Arena* literal_arena;
    IdentifierTable* identifiers;

    const char* buffer_start;
    const char* buffer_end;

    Location start_loc;

    char* current_ptr;

    // True if we have warned about a line comment in this lexer
    bool err_line_comment;

    // Some lexer flags for helping us do some preprocessor stuff
    bool start_of_line;
    bool lexing_directive;
    bool can_lex_header;
} Lexer;

// Create a lexer on the stack, no heap allocations needed for it at all
void lexer_create(Lexer* lexer, DiagnosticManager* dm, LangOptions* opts,
        Arena* literal_arena, IdentifierTable* identifiers, SourceFile* source);

// Get the next token from the lexer advancing it's position
bool lexer_get_next(Lexer* lexer, Token* tok);

// Get the next token from the lexer, but do not advance it's position
bool lexer_peek(Lexer* lexer, Token* tok);

// Get the token type of the next token in the stream without advancing the
// lexers position.
TokenType lexer_get_next_next_type(Lexer* lexer);

#endif /* LEXER_H */
