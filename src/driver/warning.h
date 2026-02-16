#ifndef WARNING_H
#define WARNING_H

#include <stdbool.h>

// An enum containing all of the diagnostics that we have.
typedef enum DiagnosticWarning {
    Wnone = -1, // Represents and generic warning: -1 so it won't
                            // be counted by the 'count'
    
    // Our specific warning options start here
    WARNING_START,
    Wother = WARNING_START, // legit any unknown warning!
    Wexprimental,
    WARNING_COUNT, // To represent the cound of all warnings
    // End of specific warning options

    // Below are our warnings that are not specific to anything and are mainly
    // for compiler driver purposes.
    Werror, // pseudo warning that treats warnings as errors
    Wall, // A pseudo warning which actually cosntrols a bunch
    Wextra, // All of the -Wextra options
    Wpedantic, // For our pedantic options
    Wfatal_errors, // A switch to set fatal errors to be true
    Wunknown, // An unknown warning option
} DiagnosticWarning;

// An enum to keep track of the diagnostic state. Must be a bitfield since we
// can have a warning as an error but that doesn't mean that it will be 
// triggered
typedef enum DiagnosticState {
    DIAG_STATE_OFF = 0, // Off means will never trigger
    DIAG_STATE_ON = 1 << 0, // On means will trigger a diagnostic for this
    DIAG_STATE_ERROR = 1 << 1 // trigger a warning as an error
} DiagnosticState;

DiagnosticWarning diagnostic_string_to_warning(const char* string);
const char* diagnostic_warning_to_string(DiagnosticWarning warning);

bool diagnostic_state_off(DiagnosticState state);
bool diagnostic_state_on(DiagnosticState state);
bool diagnostic_state_error(DiagnosticState state);

#endif /* WARNING_H */
