#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "driver/diagnostic.h"
#include "files/location.h"
#include "files/line_map.h"

#include "lex/preprocessor.h"
#include "lex/token.h"

#include "parse/ast.h"
#include "parse/symbol.h"
#include "parse/semantic.h"

// #include "parse/scope.h"

typedef struct Parser {
    // How we will give diagnostics.
    DiagnosticManager* dm;

    // The preprocessor that is used to parse this file.
    Preprocessor* pp;

    // Will need to add context to it as well for parsing loops conditionals
    // and other things
    Token token;
    Token peek_token;

    // Count of the current parens, brackets, and braces for ensuring matched
    // braces at the end of the function.
    size_t paren_count;
    size_t bracket_count;
    size_t brace_count;

    // Stores the ast. This includes all of the statements, expressions,
    // declaration, and anything else that we might need. This is held in one
    // large arena so that we can effeciently free and allocate it.
    Ast ast;

    // The semantic checker taking care of all of our declarations and needed
    // things for that.
    SemanticChecker sc;

    // TODO: track brackets???
} Parser;

void parse_translation_unit(DiagnosticManager* dm, Preprocessor* pp);

#endif /* PARSER_H */
