#ifndef TOKEN_LEXER_H
#define TOKEN_LEXER_H

#include <stddef.h>

#include "lex/source_line.h"
#include "lex/source_stream.h"
#include "lex/location.h"
#include "lex/location_map.h"

// A struct to provide the necessary equipment to be able to lex tokens nice and
// efficiently. Specifically this will be used to lex tokens form a source
// accounting for anything that might affect that. This is particularly true if 
// the line is a preprocessing line.
//
// TODO: current this will simply just lex a single simple source file. We WILL
// want to change this later but for now we would just like to get something
// just working...
typedef struct TokenLexer {
    SourceStream stream; // The stream we are getting all of the lines from
    LineRun* line_run; // the line map we are using to give out locations

    SourceLine line; // a pointer to the line given

    Location bol; // Location given for the start of the line

    char* line_ptr; // line->string.ptr
    size_t line_len; // line->string.len
    size_t line_pos; // current index into line_ptr to read

    bool has_line; // do we have a line at the moment
    bool start_of_line; // at beginning of the line (excluding whitespace)
    // bool preprocessing_directive; // is this line a preprocessing directive
    // bool should_lex_header; // should we try to lex a header name
} TokenLexer;

TokenLexer token_lexer_create(SourceStream stream, LineRun* run);
void token_lexer_close(TokenLexer* lexer);

// Get the next token from a source lexer in order to build the run of
// tokens for that line. Note that we must have more to lex, otherwise we will
// panic
Token token_lexer_get_next(TokenLexer* lexer);

#endif /* TOKEN_LEXER_H */
