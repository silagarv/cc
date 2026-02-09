#ifndef WARNING_H
#define WARNING_H

// An enum containing all of the diagnostics that we have.
typedef enum DiagnosticWarning {
    DIAG_NONE = -1, // Represents and generic warning: -1 so it won't
                            // be counted by the 'count'
    
    // Our specific warning options start here
    DIAG_OTHER, // legit any unknown warning!
    DIAG_COUNT, // To represent the cound of all warnings
    // End of specific warning options

    DIAG_ERROR, // pseudo warning that treats warnings as errors
    DIAG_ALL, // A pseudo warning which actually cosntrols a bunch
    DIAG_EXTRA, // All of the -Wextra options
    DIAG_PEDANTIC, // For our pedantic options
    DIAG_UNKNOWN, // An unknown warning option
} DiagnosticWarning;

// An enum to keep track of the diagnostic state. Must be a bitfield since we
// can have a warning as an error but that doesn't mean that it will be 
// triggered
typedef enum DiagnosticState {
    DIAG_STATE_OFF = 0, // Off means will never trigger
    DIAG_STATE_ON = 1 << 0, // On means will trigger a diagnostic for this
    DIAG_STATE_ERROR = 1 << 2 // trigger a warning as an error
} DiagnosticState;

DiagnosticWarning diagnostic_string_to_warning(const char* string);
const char* diagnostic_warning_to_string(DiagnosticWarning warning);

#endif /* WARNING_H */
