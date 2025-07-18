#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "lex/token.h"
// #include "lex/location.h"
#include "lex/location_map.h"

// #include "parse/scope.h"

typedef struct Parser {
    // Current tokens and where we are in the list
    TokenStream* stream;
    LineMap* map;

    // Will need to add context to it as well for parsing loops conditionals
    // and other things

    /* our anchor set for recovering from parsing */
    size_t recover_set[TOKEN_LAST];
} Parser;

void parse_translation_unit(TokenStream* stream, LineMap* map);

#endif /* PARSER_H */
