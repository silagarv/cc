#ifndef LITERAL_PARSER_H
#define LITERAL_PARSER_H

#include <stdint.h>
#include <stdbool.h>

#include "lex/token.h"

// A collected of all of the integer value types
typedef enum IntegerValueType {
    INTEGER_VALUE_ERROR,
    INTEGER_VALUE_INTEGER,
    INTEGER_VALUE_LONG,
    INTEGER_VALUE_UNSIGNED_LONG,
    INTEGER_VALUE_LONG_LONG,
    INTEGER_VALUE_UNSIGNED_LONG_LONG
} IntegerValueType;

// Currently we only support integer values of up to 64 bits...
typedef struct IntegerValue {
    IntegerValueType type;
    uint64_t value;

    bool error;
    bool overflow;
} IntegerValue;

// I believe both wide and normal characters have 'int' type
typedef struct CharValue {
    uint64_t value; // The value of the character itself
    bool error; // Did we get an error
} CharValue;

bool parse_char_literal(CharValue* value, const Token* token);

bool parse_integer_value(IntegerValue* value, const Token* token);


#endif /* LITERAL_PARSER_H */
