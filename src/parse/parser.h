#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/location.h"
#include "files/line_map.h"

#include "files/source_manager.h"
#include "lex/identifier_table.h"
#include "lex/preprocessor.h"
#include "lex/token.h"

#include "parse/ast.h"
#include "parse/symbol.h"
#include "parse/semantic.h"

// #include "parse/scope.h"

typedef struct Parser {
    // How we will give diagnostics.
    DiagnosticManager* dm;

    // The language we are attempting to parse
    LangOptions* lang;

    // The preprocessor that is used to parse this file.
    Preprocessor pp;

    // Will need to add context to it as well for parsing loops conditionals
    // and other things
    Token prev_token;
    Token token;
    Token peek_token;

    // Count of the current parens, brackets, and braces for ensuring matched
    // braces at the end of the function.
    size_t paren_count;
    size_t bracket_count;
    size_t brace_count;

    // Special case of parser storing some scopes that we would like to 
    // initialise before we need to parse anything. These are the only scopes
    // that should be stored here. Mainly so we can install implicit top-level
    // decls in the semantic checker before we even parse
    Scope externs;
    Scope top_level;

    // Stores the ast. This includes all of the statements, expressions,
    // declaration, and anything else that we might need. This is held in one
    // large arena so that we can effeciently free and allocate it.
    Ast* ast;

    // The semantic checker taking care of all of our declarations and needed
    // things for that.
    SemanticChecker sc;
} Parser;

bool parser_create_for_translation_unit(Parser* parser, DiagnosticManager* dm,
        LangOptions* opts, SourceManager* sm, Filepath main_file,
        IdentifierTable* ids, Ast* ast);
void parser_delete(Parser* parser);

void parse_translation_unit(Parser* parser);

#endif /* PARSER_H */
