#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>

#include "adt/vector.h"

#include "parse/location.h"

typedef enum TokenType {
    TOKEN_EOF,
} TokenType;

typedef struct Token {
    TokenType type;

    char* str;
    size_t strlen;
    
    LocationID loc;
} Token;

typedef struct TokenStream {
    // vector(Token) tokens;
    Vector tokens;
    size_t current_token;
} TokenStream;

#endif /* TOKEN_H */
