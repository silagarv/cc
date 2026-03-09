#ifndef LEXER_H
#define LEXER_H

#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/location.h"
#include "files/source_manager.h"

#include "lex/token.h"
#include "lex/identifier_table.h"
#include "util/buffer.h"

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

// Create a lexer on the stack, no heap allocations needed for it at all.
void lexer_create(Lexer* lexer, DiagnosticManager* dm, LangOptions* opts,
        Arena* literal_arena, IdentifierTable* identifiers, SourceFile* source);
void lexer_set_directive(Lexer* lexer);
void lexer_set_header(Lexer* lexer);

// Get the next token from the lexer advancing it's position. Note that this
// cannot be undone. However, this should never be necessary as the token 
// should always be saved be the preprocessor.
bool lexer_get_next(Lexer* lexer, Token* tok);

// Lex the next token but do not advance the lexer at all. This function will
// save the entire state of the lexer and also ensure that it does not emit any
// diagnositcs while we are lexing the code. Note that technichally this has
// 'observable' effects since lexing a token may require and identifier table
// lookup or some form of memory allocation. However, this is typically minimal
// and this function should be used sparingly anyways.
bool lexer_peek_next(Lexer* lexer, Token* token);

// Specialised function getting the rest of the line and putting it in a buffer
// that we can used. This function is mainly used for warning diagnostics to get
// the raw string of text. This function reads directly from the current 
// position to the first newline it sees but does not eat the newline. So to
// get out of preprocessor mode `lexer_get_next` should be called. Note that 
// this function also consumes any leading whitespace and ignores it.
void lexer_read_diagnostic_string(Lexer* lexer, Buffer* buffer);

// Other lexer functions to help us with preprocessing primarily. These allow
// for us to get the spelling for tokens which is particularly useful for things
// like preprocessing.
// TODO: will need to create a lexer from a sourcefile and give it an offset to
// TODO: start at...

#endif /* LEXER_H */
