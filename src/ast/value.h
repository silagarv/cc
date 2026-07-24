#ifndef AST_VALUE_H
#define AST_VALUE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

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

typedef struct IntegerValue {
    IntegerValueType type;
    IntegerValueSuffix suffix;
    uint64_t value;
} IntegerValue;

IntegerValue integer_value_create(IntegerValueType type,
        IntegerValueSuffix suffix, uint64_t value);


// Enum to represent different floating point value types
typedef enum FloatingValueType {
    FLOATING_VALUE_ERROR,
    FLOATING_VALUE_FLOAT,
    FLOATING_VALUE_DOUBLE,
    FLOATING_VALUE_LONG_DOUBLE
} FloatingValueType;

typedef enum FloatingValueSuffix {
    FLOATING_VALUE_SUFFIX_ERROR,
    FLOATING_VALUE_SUFFIX_NONE,
    FLOATING_VALUE_SUFFIX_F,
    FLOATING_VALUE_SUFFIX_L
} FloatingValueSuffix;

// Structure to hold a floating point value
typedef struct FloatingValue {
    FloatingValueType type;
    FloatingValueSuffix suffix;
    long double value;
} FloatingValue;

typedef enum CharType {
    CHAR_TYPE_CHAR,
    CHAR_TYPE_WIDE,
    CHAR_TYPE_UTF8,
    CHAR_TYPE_UTF16,
    CHAR_TYPE_UTF32
} CharType;

typedef struct CharValue {   
    uint64_t value;
    bool is_wide;
} CharValue;

// Structures to represent string and wide string literal values
typedef struct StringLiteral {
    CharType type;
    char* string;
    size_t length;
    size_t char_size;
} StringLiteral;

// TODO: add in all of the functions and their implementation...

#endif /* AST_VALUE_H */
