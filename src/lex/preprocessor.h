#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "util/buffer.h"
#include "util/hash_map.h"

#include "files/source_manager.h"

#include "lex/lexer.h"
#include "lex/header_finder.h"

typedef struct Preprocessor {
    // Source manager for us to be able to manage all of the source that we
    // are going to use in the preprocessor. This is not owned by the 
    // preprocessor in the event that we want to acess files after preprocessing
    // e.g. printing diagnostics during parsing for instance. 
    SourceManager* sm;

    // The header finder used to search for headers within the source
    HeaderFinder hf;

    // The lexer -> but todo, create a lexer stack or something
    Lexer lexer;

    // A macro map where the key's in the map are the macro names and the data
    // is the macro struct themselves
    HashMap macros;
} Preprocessor;

Preprocessor preprocessor_create(SourceManager* sm);

#endif /* PREPROCESSOR_H */
