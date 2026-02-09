#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

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
typedef struct LexerStack LexerStack;

typedef struct Preprocessor {
    DiagnosticManager* dm;

    // The language we are trying to preprocess
    LangOptions* lang;

    // Source manager for us to be able to manage all of the source that we
    // are going to use in the preprocessor. This is not owned by the 
    // preprocessor in the event that we want to acess files after preprocessing
    // e.g. printing diagnostics during parsing for instance. 
    SourceManager* sm;

    // The table we are storing all of our identifiers in.
    IdentifierTable* identifiers;

    // The header finder used to search for headers within the source
    HeaderFinder hf;

    // The arena that holds all of the data for our literal nodes. This includes
    // strings, characters, and numbers etc...
    Arena literal_arena;

    // The lexer -> but todo, create a lexer stack or something
    Lexer lexer;

    // The stack of lexers we are using for this preprocessor
    LexerStack* lexers;

    // The Arena that we are going to use for allocating all of our preprocessor
    // information onto particularaly for our macro tables and those kinds of
    // things.
    Arena pp_allocator;
} Preprocessor;

bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids);
void preprocessor_delete(Preprocessor* pp);

bool preprocessor_advance_token(Preprocessor* pp, Token* token);
bool preprocessor_peek_token(Preprocessor* pp, Token* token);

TokenType preprocessor_peek_next_token_type(Preprocessor* pp);

// TODO: to help error recovery
void preprocessor_insert_token(Preprocessor* pp, Token token);

#endif /* PREPROCESSOR_H */
