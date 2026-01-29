#ifndef DRIVER_H
#define DRIVER_H

#include "driver/diagnostic.h"
#include "driver/options.h"
#include "driver/target.h"

typedef struct CompilerDriver {
    // The diagnostic manager for this driver
    DiagnosticManager dm;

    // The options we are using to compiler
    CompilerOptions options;

    
} CompilerDriver;

int compiler_driver_invoke(int argc, char** argv);

#endif /* DRIVER_H */
