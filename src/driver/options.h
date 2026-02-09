#ifndef OPTIONS_H
#define OPTIONS_H

#include "driver/lang.h"
#include "driver/diagnostic.h"

// An enum for all of the different levels of optimisation we can have
typedef enum OptimisationLevel {
    OPT_LEVEL_UNSPECIFIED,
    OPT_LEVEL_0,
    OPT_LEVEL_1,
    OPT_LEVEL_2,
    OPT_LEVEL_3,
    OPT_LEVEL_G,
    OPT_LEVEL_S,
    OPT_LEVEL_Z,
    OPT_LEVEL_FAST
} OptimisationLevel;

typedef struct CompilerOptions {
    // The language standard that we want to use
    LangStandard standard;
    bool strict;
    bool gnu;

    // The level of optimisations that we are going for
    OptimisationLevel opt_level;

    // Should we dump help, and should we dump version
    bool help;
    bool version;

    // Flags that are affected by -E, -fsyntax-only, -S, and -c
    bool preprocess_only;
    bool syntax_only;
    bool dump_assembly;
    bool compile_only;

    // Options controlling warnings
    bool werror;
    bool w;

    // Some prerprocessor options for when using -E
    bool print_headers;
    bool no_line_markers;

    // The initial outfile and infile
    char* infile;
    char* outfile;
} CompilerOptions;

bool parse_compiler_options(CompilerOptions* opts, DiagnosticManager* dm,
        int argc, char** argv);

#endif /* OPTIONS_H */
