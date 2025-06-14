#ifndef OPTIONS_H
#define OPTIONS_H

#include "util/static_string.h"

// enum LanguageOptions {
//     LANGUAGE_DEFAULT = 0,
//     LANGUAGE_C89, /* unused for now */
//     LANGUAGE_C99,
//     LANGUAGE_C11, /* unused for now */
//     LANGUAGE_C23  /* unused for now */
// };
// typedef enum LanguageOptions LanguageOptions;

struct Options {
    StaticString starting_file;

    StaticStringList quote_path;
    StaticStringList bracket_path;
    StaticStringList system_path;
    StaticStringList after_path;
};
typedef struct Options Options;

void options_initialise(Options* opts);
void options_free(Options* opts);

#endif /* OPTIONS_H */
