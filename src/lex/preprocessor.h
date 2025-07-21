#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "util/buffer.h"

#include "lex/lexer.h"

typedef struct Preprocessor {

    Lexer lexer;

} Preprocessor;

Buffer destringize_pragma_literal(const Token* token);

#endif /* PREPROCESSOR_H */
