#ifndef LITERAL_PARSER_H
#define LITERAL_PARSER_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "driver/diagnostic.h"
#include "lex/token.h"

// A collected of all of the integer value types
typedef enum IntegerValueType {
    INTEGER_VALUE_ERROR,
    INTEGER_VALUE_INTEGER,
    INTEGER_VALUE_UNSIGNED_INTEGER,
    INTEGER_VALUE_LONG,
    INTEGER_VALUE_UNSIGNED_LONG,
    INTEGER_VALUE_LONG_LONG,
    INTEGER_VALUE_UNSIGNED_LONG_LONG
} IntegerValueType;

// These are all of the valid integer value suffixes, 
typedef enum IntegerValueSuffix {
    INTEGER_VALUE_SUFFIX_INVALID,
    INTEGER_VALUE_SUFFIX_NONE,
    INTEGER_VALUE_SUFFIX_L,
    INTEGER_VALUE_SUFFIX_LL,
    INTEGER_VALUE_SUFFIX_U,
    INTEGER_VALUE_SUFFIX_UL,
    INTEGER_VALUE_SUFFIX_ULL
} IntegerValueSuffix;

// Currently we only support integer values of up to 64 bits...
typedef struct IntegerValue {
    IntegerValueType type;
    IntegerValueSuffix suffix;
    uint64_t value;
} IntegerValue;

// Enum to represent different floating point value types
typedef enum FloatingValueType {
    FLOATING_VALUE_ERROR,
    FLOATING_VALUE_FLOAT,
    FLOATING_VALUE_DOUBLE,
    FLOATING_VALUE_LONG_DOUBLE
} FloatingValueType;

// Structure to hold a floating point value
typedef struct FloatingValue {
    FloatingValueType type;
    long double value;

    bool error;
    bool overflow;
} FloatingValue;

typedef enum ValueType {
    VALUE_INTEGER_TYPE,
    VALUE_FLOATING_TYPE
} ValueType;

typedef struct LiteralValue {
    ValueType type;
    union {
        IntegerValue integer;
        FloatingValue floating;
    } value;
} LiteralValue;

// A structure to represent a character literal value in memory
typedef struct CharValue {   
    uint64_t value; // The value of the character itself
    bool error; // Did we get an error
} CharValue;

// Structures to represent string and wide string literal values
typedef struct StringLiteral {
    String string;

    bool wide; // is the string wide
    bool error;
} StringLiteral;

// bool parse_integer_literal(IntegerValue* value, const Token* token);
// bool parse_floating_literal(FloatingValue* value, const Token* token);

bool parse_preprocessing_number(LiteralValue* value, DiagnosticManager* dm,
        const Token* token);

bool parse_char_literal(CharValue* value, const Token* token);
bool parse_string_literal(StringLiteral* value, const Token* tokens, size_t num_tokens);

#endif /* LITERAL_PARSER_H */
