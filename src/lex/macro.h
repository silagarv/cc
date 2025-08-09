#ifndef MACRO_H
#define MACRO_H

#include "util/buffer.h"

#include "lex/token.h"

typedef struct Macro {
    void* data;
} Macro;

// Destringize a pragma literal token
Buffer destringize_pragma_literal(const Token* token);



#endif /* MACRO_H */
