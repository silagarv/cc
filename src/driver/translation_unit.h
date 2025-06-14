#ifndef TRANSLATION_UNIT_H
#define TRANSLATION_UNIT_H

#include <stdbool.h>

#include "util/static_string.h"

#include "driver/options.h"

// #include "preprocessor/token.h"

// Represents a c compilation unit...
struct TranslationUnit {
    // Starting file name
    StaticString* start_file_name;

    // All of our options we are going to use in compilation
    Options opts;

    // Need to store all of out ast, symbol table, etc... in here

    struct TranslationUnit* next;
};
typedef struct TranslationUnit TranslationUnit;

TranslationUnit* translation_unit_initialise(Options* options, 
        StaticString* main_file);
void translation_unit_free(TranslationUnit* unit);

#endif /* TRANSLATION_UNIT_H */
