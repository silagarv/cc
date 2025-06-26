#ifndef TOKEN_LEXER_H
#define TOKEN_LEXER_H

#include "lex/source_stream.h"
#include "lex/token.h"

TokenList source_stream_lex_tokens(SourceStream stream);

#endif /* TOKEN_LEXER_H */
