#ifndef LANG_H
#define LANG_H

#include <stdbool.h>

// an enum which holds the different possible language standards.
typedef enum LangStandard {
    LANG_STANDARD_DEFAULT, // The default standard c99
    LANG_STANDARD_C89, // C89 (unsupported)
    LANG_STANDARD_C99, // C99 (supported)
    LANG_STANDARD_C11, // C11 (unsupported)
    LANG_STANDARD_C23,  // C23 (unsupported)
} LangStandard;

bool lang_standard_c89(LangStandard standard);
bool lang_standard_c99(LangStandard standard);
bool lang_standard_c11(LangStandard standard);
bool lang_standard_c23(LangStandard standard);

#endif /* LANG_H */
