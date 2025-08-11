#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "util/buffer.h"
#include "util/hash_map.h"

#include "lex/lexer.h"

typedef struct Preprocessor {

    // The lexer -> but todo, create a lexer stack or something
    Lexer lexer;

    // A macro map where the key's in the map are the macro names and the data
    // is the macro struct themselves
    HashMap macro_map;

} Preprocessor;

Preprocessor preprocessor_create(void);

#endif /* PREPROCESSOR_H */
