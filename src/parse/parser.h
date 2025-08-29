#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "files/location.h"
#include "files/line_map.h"

#include "lex/token.h"

#include "parse/ast.h"

// #include "parse/scope.h"

typedef struct Parser {
    // Current tokens and where we are in the list
    TokenStream* stream;
    LineMap* map;

    // Will need to add context to it as well for parsing loops conditionals
    // and other things

    /* our anchor set for recovering from parsing */
    size_t recover_set[TOKEN_LAST];

    // Stores the current context of the parser for statements and expressions.
    AstContext current_context;

    // TODO: track brackets???
} Parser;

void parse_translation_unit(TokenStream* stream, LineMap* map);

#endif /* PARSER_H */
