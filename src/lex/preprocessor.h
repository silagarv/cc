#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <stddef.h>

#include "util/buffer.h"
#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "lex/header_finder.h"
#include "lex/token.h"
#include "lex/include_stack.h"
#include "lex/macro_map.h"
#include "lex/expand.h" 

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

    // The arena that holds all of the data for our literal nodes. This includes
    // strings, characters, and numbers etc... This is given as a pointer to
    // each of our lexers, and should only be destroyed at the end of 
    // preprocessing.
    Arena literal_arena;

    // The header finder used to search for headers within the source
    HeaderFinder hf;

    // The stack of includes that we are using for this preprocessor.
    IncludeVector inputs;

    // The max depth of inputs that we are allowed to do. set to 200
    unsigned int max_depth;

    // Also store some important preprocessing identifiers. These are mainly
    // used to assist when we are expanding our macros just to make our lives a
    // little bit easier then.
    Identifier* id___VA_ARGS__;
    Identifier* id___LINE__;
    Identifier* id___FILE__;
    Identifier* id___DATE__;
    Identifier* id___TIME__;
    Identifier* id___INCLUDE_LEVEL__;
    Identifier* id___COUNTER__;
    Identifier* id__Pragma;

    // The thing that will store all of our preprocessor macros and definitions
    // and handle the defining and undefinting of these. This also helps the
    // preprocessor by storing the current state of our macros.
    MacroMap macros;

    // The macro expander used in the preprocessor. The macro expander 
    // implements all of the functionality required in order to expand macros.
    // Note that it hides alot of the complexity away from the preprocessor and
    // only tells the preprocessor if it is expanding a macro right now or not.
    MacroExpander expander;

    // The Arena that we are going to use for allocating all of our preprocessor
    // information onto particularaly for our macro tables and those kinds of
    // things.
    Arena pp_allocator;

    // The token cache that we are going to use for macro tokens and the like.
    // Also used to inject tokens into the token stream when needed.
    TokenList cache;
} Preprocessor;

// Functions for creating or destorying the current preprocessor and all of it's
// context.
bool preprocessor_create(Preprocessor* pp, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids);
void preprocessor_delete(Preprocessor* pp);

// Functions that should only be used by the preprocessor for preprocessing
// purposes like the handling of directives and such. Should not be used by 
// the parser for any purpose at all.
DiagnosticManager* preprocessor_diagnostics(Preprocessor* pp);
SourceManager* preprocessor_source_manager(Preprocessor* pp);
Arena* preprocessor_allocator(Preprocessor* pp);
IncludeVector* preprocessor_inputs(Preprocessor* pp);
unsigned int preprocessor_include_depth(const Preprocessor* pp);
MacroMap* preprocessor_macro_map(Preprocessor* pp);

void preprocessor_enter_directive(Preprocessor* pp);
void preprocessor_read_diagnostic_string(Preprocessor* pp, Buffer* buffer);

// Get the next token from the preprocessor that is not macro expanded or 
// anything. This should be used for trying to parse macro definitions and that
// general kind of thing.
bool preprocessor_next_raw_token(Preprocessor* pp, Token* token);

// Routines for the parser to use to help manage it's current token in the 
// preprocessor. It can either advance the token stream to the next token, or
// peek the next token from the preprocessor.
bool preprocessor_advance_token(Preprocessor* pp, Token* token);
bool preprocessor_peek_token(Preprocessor* pp, Token* token);
// TODO: would it be beneficial to be able to peek arbitrarily far ahead?
// bool preprocessor_peek_n_token(Preprocessor* pp, Token* token, size_t n);

// Insert a token directly into the lexers token stream. Should only be used to
// help error recovery within the parser and not to just push tokens in whenever
// we feel like it. Note that this pushes said token right into the start of the
// stream, regardless of whatever tokens are already in front of it.
void preprocessor_insert_token(Preprocessor* pp, Token token);

#endif /* PREPROCESSOR_H */
