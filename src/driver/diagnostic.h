#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>
#include <stdbool.h>

// An enum containing all of the diagnostics that we have.
typedef enum DiagnosticType {
    DIAGNOSTIC_ERROR_START,
    
    DIAGNOSTIC_ERROR_END,

    DIAGNOSTIC_WARNING_START,

    DIAGNOSTIC_WARNING_END
} DiagnosticType;

bool diagnostic_is_warning(DiagnosticType type);

typedef struct DiagnosticManager {
    void* data;
} DiagnosticManager;

// TODO: should we implement a diagnostic queue maybe? so that we can have 
// a multithreaded compiler with different translation units being compiled
// simultaneously and then have diagnostics in flight from all of them? and then
// also get LTO for free since we have all of the translation units loaded into
// memory already

void diag_init(void);

void diag_fatal_error(const char* fmt, ...);
void diag_error(const char* fmt, ...);
void diag_warning(const char* fmt, ...);
void diag_note(const char* fmt, ...);
void diag_help(const char* fmt, ...);

void diag_verror(const char* fmt, va_list args);

#endif /* DIAGNOSTIC_H */
