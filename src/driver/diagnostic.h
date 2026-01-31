#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

#include "files/location.h"
#include "files/source_manager.h"

// An enum containing all of the diagnostics that we have.
typedef enum DiagnosticType {
    DIAGNOSTIC,
} DiagnosticType;

// An enum to keep track of the diagnostic state
typedef enum DiagnosticState {
    DIAGNOSTIC_STATE_OFF, // Off means will never trigger
    DIAGNOSTIC_STATE_ON, // On means will trigger a diagnostic for this
    DIAGNOSTIC_STATE_ERROR // Error means we will trigger a warning as an error
} DiagnosticState;

// The diagnostic option we would like to control
typedef struct DiagnosticOption {
    DiagnosticType type; // The option that this is for
    DiagnosticState state; // The state of this diagnostic
    const char* name; // the name of the option e.g. "shadow for -Wshadow"
    const char* diag; // The diagnostic text that this corrosponds to.
    bool error; // Treat this as an error always
} DiagnosticOption;

// Structure to store the diagnostic colours that we will use.
typedef struct DiagnosticColours {
    char* fatal;
    char* error;
    char* warning;
    char* note;
    char* help;
    char* white;
    char* highlight;
    char* reset_highlight;
    char* reset_all;
} DiagnosticColours;

typedef struct DiagnosticManager {
    DiagnosticColours colours;
    SourceManager* sm;

    size_t warning_count;
    size_t error_count;

    bool werror;
    bool disable_warnings;
} DiagnosticManager;

DiagnosticManager diagnostic_manager_init(SourceManager* sm);
void diagnostic_manager_set_sm(DiagnosticManager* dm, SourceManager* sm);
void diagnostic_manager_set_werror(DiagnosticManager* dm, bool value);
void diagnostic_manager_set_disable_warnings(DiagnosticManager* dm, bool value);

size_t diagnostic_manager_get_warning_count(const DiagnosticManager* dm);
size_t diagnostic_manager_get_error_count(const DiagnosticManager* dm);

void diagnostic_emit_count(DiagnosticManager* dm);

void diagnostic_error(DiagnosticManager* dm, const char* fmt, ...);
void diagnostic_warning(DiagnosticManager* dm, const char* fmt, ...);
void diagnostic_note(DiagnosticManager* dm, const char* fmt, ...);
void diagnostic_help(DiagnosticManager* dm, const char* fmt, ...);

void diagnostic_error_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...);
void diagnostic_warning_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...);
void diagnostic_note_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...);
void diagnostic_help_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...);

#endif /* DIAGNOSTIC_H */
