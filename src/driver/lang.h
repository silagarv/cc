#ifndef LANG_H
#define LANG_H

#include <stdbool.h>

// an enum which holds the different possible language standards.
typedef enum LangStandard {
    LANG_STANDARD_DEFAULT, // The default standard c99
    LANG_STANDARD_C89, // C89 (unsupported)
    LANG_STANDARD_C99, // C99 (supported)
    LANG_STANDARD_C11, // C11 (unsupported)
    LANG_STANDARD_C17, // C17 (unsupported)
    LANG_STANDARD_C23,  // C23 (unsupported)
} LangStandard;

// Structure to hold all of our possible language options.
typedef struct LangOptions {
    LangStandard standard; // The lang standard we are wanting to support
    bool trigraphs; // Do we accept trigraphs?
    bool strict; // Are we trying to be as strict as possible
    bool gnu; // Are we allowing gnu extensions (without warning)
} LangOptions;

bool lang_standard_c89(LangStandard standard);
bool lang_standard_c99(LangStandard standard);
bool lang_standard_c11(LangStandard standard);
bool lang_standard_c23(LangStandard standard);

bool lang_opts_c89(const LangOptions* opts);
bool lang_opts_c99(const LangOptions* opts);
bool lang_opts_c11(const LangOptions* opts);
bool lang_opts_c23(const LangOptions* opts);
bool lang_opts_strict(const LangOptions* opts);
bool lang_opts_gnu(const LangOptions* opts);

LangOptions lang_opts(LangStandard std, bool trigraphs, bool strict, bool gnu);

#endif /* LANG_H */
