#ifndef MACRO_H
#define MACRO_H

#include <stdbool.h>
#include <stddef.h>

#include "util/buffer.h"

#include "lex/token.h"

typedef struct Macro {
    IdentifierNode* name;

    bool is_function;

    size_t num_args;

    TokenList replacement_list;
} Macro;

// Destringize a pragma literal token as we will consider this to be macro like
// in terms of the preprocessor
Buffer destringize_pragma_literal(const Token* token);


#endif /* MACRO_H */
