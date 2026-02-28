#ifndef DIRECTIVES_H
#define DIRECTIVES_H

#include "preprocessor.h"

// Function which returns true if this token is the start of a directive
bool preprocessor_directive_start(Preprocessor* pp, Token* token);

// Function which parses and handles any processing required for directives.
void preprocessor_parse_directive(Preprocessor* pp, Token* token);

#endif /* DIRECTIVES_H */
