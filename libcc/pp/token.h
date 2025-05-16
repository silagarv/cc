#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdbool.h>

#include "core/location.h"

typedef enum TokenType {
    TOKEN_EOF,
} TokenType;

// compact token type...
typedef struct Token {
    TokenType type;
    char* start;
    size_t len;
    LocationID loc;
} Token;

bool is_token_keyword(Token* tok);

bool is_token_identifier(Token* tok);

bool is_token_constant(Token* tok);

bool is_token_string_literal(Token* tok);

bool is_token_punctuator(Token* tok);

bool is_token_defineable(Token* tok);

// TODO: maybe more functions about tokens pasting

#endif /* TOKEN_H */
