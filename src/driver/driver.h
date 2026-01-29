#ifndef DRIVER_H
#define DRIVER_H

#include "driver/diagnostic.h"
#include "driver/options.h"

typedef struct CompilerDriver {
    DiagnosticManager dm;
    CompilerOptions options;
} CompilerDriver;

int compiler_driver_invoke(int argc, char** argv);

#endif /* DRIVER_H */
