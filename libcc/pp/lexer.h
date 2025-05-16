#ifndef LEXER_H
#define LEXER_H

#include "pp/location.h"
#include "pp/line.h"
#include "pp/source.h"
#include "pp/input_manager.h"
#include "pp/token.h"

// contains our sources and an input manager for getting the sources
typedef struct Lexer {
    // A place for us to store all of our persistent memory into here
    InputManager* manager;
    LineMap* lines;
    LocationMap* locations;
    Vector* sources;

    // The current source and it's parents
    Source* source_stack;

    // Current lexing state and information
    Line* current_line;
    /* some other bools and things would go here */

    // Store all of the current tokens
    Vector* tokens;
} Lexer;

#endif /* LEXER_H */
