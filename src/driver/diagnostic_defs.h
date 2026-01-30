#ifndef DIAGNOSTIC_DEFS_H
#define DIAGNOSTIC_DEFS_H

#include <stdbool.h>

typedef enum DiagnosticCode {
    DIAGNOSTIC_ERROR_GENERAL, // [Eother]
    DIAGNOSTIC_WARNING_GENERAL, // [Wother]
    DIAGNOSTIC_COUNT
} DiagnosticCode;

typedef struct DiagnosticState {
    DiagnosticCode code;
    const char* message;
    bool enabled;
    bool error;
} DiagnosticState;

#endif /* DIAGNOSTIC_DEFS_H */
