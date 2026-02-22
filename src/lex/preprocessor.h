#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <stddef.h>

#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "lex/token.h"
#include "lex/lexer.h"
#include "lex/header_finder.h"

// Implement at a typedef so when changes are made we don't have to recompile
typedef struct InputStack InputStack;

typedef struct Preprocessor {
    DiagnosticManager* dm;

    // The language we are trying to preprocess
    LangOptions* lang;

    // Source manager for us to be able to manage all of the source that we
    // are going to use in the preprocessor. This is not owned by the 
    // preprocessor in the event that we want to acess files after preprocessing
    // e.g. printing diagnostics during parsing for instance. 
    SourceManager* sm;

    // The header finder used to search for headers within the source
    HeaderFinder hf;

    // The table we are storing all of our identifiers in.
    IdentifierTable* identifiers;

    // The arena that holds all of the data for our literal nodes. This includes
    // strings, characters, and numbers etc...
    Arena literal_arena;

    // The max depth of inputs that we are allowed to do. set to 200
    unsigned int max_depth;
    
    // The stack of lexers we are using for this preprocessor
    InputStack* inputs;

    // The Arena that we are going to use for allocating all of our preprocessor
    // information onto particularaly for our macro tables and those kinds of
    // things.
    Arena pp_allocator;

    // The token cache that we are going to use for macro tokens and the like.
    // Also used to inject tokens into the token stream when needed.
    TokenList cache;
} Preprocessor;

bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids);
void preprocessor_delete(Preprocessor* pp);

bool preprocessor_advance_token(Preprocessor* pp, Token* token);
bool preprocessor_peek_token(Preprocessor* pp, Token* token);
// TODO: would it be beneficial to be able to peek arbitrarily far ahead?
// bool preprocessor_peek_n_token(Preprocessor* pp, Token* token, size_t n);

// Insert a token directly into the lexers token stream. Should only be used to
// help error recovery within the parser and not to just push tokens in whenever
// we feel like it.
void preprocessor_insert_token(Preprocessor* pp, Token token);

#endif /* PREPROCESSOR_H */
