#ifndef SOURCE_LINE_H
#define SOURCE_LINE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/str.h"

#include "lex/token.h"

// A SourceLine struct contains all the information of a line that has undergone
// stage 3 processing as according to the C99 standard.
//
// It is not really directly from the source as it could contain trigraph
// replacement, newline splicing and other things. So it may not actually
// directly relate back to the original source.
typedef struct SourceLine {
    String string;

    uint32_t num_phyical_lines;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
} SourceLine;

#endif /* SOURCE_LINE_H */
