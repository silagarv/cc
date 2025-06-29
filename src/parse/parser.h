#ifndef PARSER_H
#define PARSER_H

#include <stddef.h>
#include <stdbool.h>

#include "lex/token.h"

#include "parse/scope.h"

struct Parser {
    // Current tokens and where we are in the list
    TokenStream stream;

    Scope* scopes;
    size_t scopes_count;
    size_t scopes_allocated;
};
typedef struct Parser Parser;

void parse_translation_unit(TokenStream stream);

#endif /* PARSER_H */
