#ifndef LITERAL_PARSER_H
#define LITERAL_PARSER_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "lex/token.h"
#include "parse/ast_allocator.h"

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

// A structure to represent a character literal value in memory. If it was a
// wide char the value should be taken from wide, otherwise it should be taken
// from normal.
typedef struct CharValue {   
    uint64_t value;
    bool is_wide; // Did we parse a wide char
    bool error; // Did we get an error
} CharValue;

// Structures to represent string and wide string literal values
typedef struct StringLiteral {
    char* string; // the buffer that holds the string data itself
    size_t length; // The length of the string including null terminator in the
                   // number of 'characters' that the string contains
    size_t char_size; // the size of one 'character' of the string in bytes
                      // e.g in a normal string it would be '1', but in a wide
                      // string it is '4'
} StringLiteral;

uint64_t integer_value_get_value(const IntegerValue* val);

uint64_t char_value_get_value(const CharValue* val);

long double floating_value_get_value(const FloatingValue* val);

ValueType literal_value_get_type(const LiteralValue* value);

char* string_literal_buffer(const StringLiteral* string);
size_t string_literal_length(const StringLiteral* string);
size_t string_literal_char_size(const StringLiteral* string);

bool parse_preprocessing_number(LiteralValue* value, DiagnosticManager* dm,
        const Token* token);
bool parse_char_literal(CharValue* value, DiagnosticManager* dm,
        const Token* token, bool wide);
// bool parse_string_literal(AstAllocator* allocator, StringLiteral* value,
//         DiagnosticManager* dm, TokenVector tokens, LocationVector locs,
//         bool wide);
bool parse_string_literal(AstAllocator* allocator, StringLiteral* value,
        DiagnosticManager* dm, LangOptions* lang, const TokenList* tokens,
        bool unevaluated);

#endif /* LITERAL_PARSER_H */
