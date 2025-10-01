#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "files/filepath.h"
#include "lex/identifier_table.h"
#include "lex/token.h"
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

    IdentifierTable* identifiers;

    // The header finder used to search for headers within the source
    HeaderFinder hf;

    // The lexer -> but todo, create a lexer stack or something
    Lexer lexer;
} Preprocessor;

Preprocessor preprocessor_create(SourceManager* sm, 
        IdentifierTable* identifiers,SourceFile* starting_file);

bool preprocessor_advance_token(Preprocessor* pp, Token* token);
TokenType preprocessor_peek_next_token_type(Preprocessor* pp);

#endif /* PREPROCESSOR_H */
