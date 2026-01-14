#ifndef OPTIONS_H
#define OPTIONS_H

#include "driver/lang.h"
#include "driver/diagnostic.h"

typedef struct CompilerOptions {
    // What standard are we using, defaults to c99
    LangStandard standard;

    // Should we treat warnings as errors.
    bool werror;

    // The file we should try to compile
    char* infile;
} CompilerOptions;

CompilerOptions parse_compiler_options(DiagnosticManager* dm, int argc,
        char** argv, bool* okay);

#endif /* OPTIONS_H */
