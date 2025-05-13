#ifndef LEXER_H
#define LEXER_H

#include "parse/location.h"
#include "parse/input.h"

typedef struct Lexer {
    InputManager* inputs;
    LocationMap* locations;

    
} Lexer;


#endif /* LEXER_H */
