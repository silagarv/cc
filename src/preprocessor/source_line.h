#ifndef SOURCE_LINE_H
#define SOURCE_LINE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/str.h"

struct SourceLine {
    // the line itself
    String string;

    // for calculating the next line, the number of physical lines it takes up
    uint32_t num_phyical_lines;

    // information about the line
    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
};
typedef struct SourceLine SourceLine;

#endif /* SOURCE_LINE_H */
