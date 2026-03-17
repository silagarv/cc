#ifndef PP_EXPRESSION_H
#define PP_EXPRESSION_H

#include <stdint.h>

#include "lex/token.h"
#include "lex/preprocessor.h"

// FIXME: include a location in the value???
// A simple structure to represent a preprocessing value and if the 
// preprocessing value is valid to use.
typedef struct PPValue {
    int64_t value; // The integer value of this expression
    bool success; // Did we fail to parse and expression or did we succeed
} PPValue;

int64_t pp_value_get_value(const PPValue* value);
bool pp_value_get_success(const PPValue* value);

// Function to parse a preprocessing expression.
bool preprocessor_parse_expression(Preprocessor* pp, Token* token,
        PPValue* value);

#endif /* PP_EXPRESSION_H */
