#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

#include "files/location.h"
#include "files/source_manager.h"

// An enum containing all of the diagnostics that we have.
typedef enum DiagnosticWarning {
    DIAG_WARNING_UNKNOWN = -1, // Represents and unknown warning -1 so it won't
                               // be counted by the 'count'
    DIAG_WARNING_OTHER, // legit any unknown warning!
    DIAG_WARNING_COUNT, // To represent the cound of all warnings
    DIAG_WARNING_ALL, // A pseudo warning which actually cosntrols a bunch
    DIAG_WARNING_EXTRA, // All of the -Wextra options
    DIAG_WARNING_PEDANTIC, // For our pedantic options
} DiagnosticWarning;

// An enum to keep track of the diagnostic state. Must be a bitfield since we
// can have a warning as an error but that doesn't mean that it will be 
// triggered
typedef enum DiagnosticState {
    DIAG_STATE_OFF = 0, // Off means will never trigger
    DIAG_STATE_ON = 1 << 0, // On means will trigger a diagnostic for this
    DIAG_STATE_ERROR = 1 << 2 // trigger a warning as an error
} DiagnosticState;

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

    DiagnosticState options[DIAG_WARNING_COUNT];
} DiagnosticManager;

DiagnosticWarning diagnostic_string_to_warning(const char* string);

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
