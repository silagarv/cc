#ifndef LEXER_H
#define LEXER_H

#include "adt/vector.h"

#include "parse/location.h"
#include "parse/input.h"
#include "parse/token.h"

typedef struct Lexer {
    InputManager* inputs; // the input manager we are using
    LocationMap* locations; // a pointer to the location map in the TU

    vector(Token) tokens; // a list of all our tokens
} Lexer;



#endif /* LEXER_H */
