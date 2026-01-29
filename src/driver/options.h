#ifndef OPTIONS_H
#define OPTIONS_H

#include "driver/lang.h"
#include "driver/diagnostic.h"

typedef struct CompilerOptions {
    LangStandard standard;

    bool werror;

    char* infile;
    char* outfile;
} CompilerOptions;

bool parse_compiler_options(CompilerOptions* opts, DiagnosticManager* dm,
        int argc, char** argv);

#endif /* OPTIONS_H */
