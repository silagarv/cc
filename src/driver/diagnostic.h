#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>

#include "driver/warning.h"

#include "files/location.h"
#include "files/source_manager.h"

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

    DiagnosticState options[DIAG_COUNT];
} DiagnosticManager;

DiagnosticManager diagnostic_manager_init(SourceManager* sm);
void diagnostic_manager_set_sm(DiagnosticManager* dm, SourceManager* sm);

void diagnostic_manager_set_werror(DiagnosticManager* dm, bool value);
void diagnostic_manager_set_disable_warnings(DiagnosticManager* dm, bool value);
void diagnostic_manager_set_option(DiagnosticManager* dm,
        DiagnosticWarning warning, bool no, bool error);

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
