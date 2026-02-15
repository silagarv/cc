#include "warning.h"

#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

// The diagnostic option we would like to control
typedef struct DiagnosticOption {
    DiagnosticWarning type; // The option that this is for
    DiagnosticState state; // The state of this diagnostic
    const char* name; // the name of the option e.g. "shadow for -Wshadow"
} DiagnosticOption;

// Make this constant since this should never need to change
static const DiagnosticOption warnings[WARNING_COUNT] =
{
    [Wother] = (DiagnosticOption)
        {
            .type = Wother,
            .state = DIAG_STATE_ON, 
            .name = "other",
        },
};

// Also keep a count of the warnings
static const size_t warnings_count = WARNING_COUNT;

static bool string_is(const char* str1, const char* str2)
{
    return strcmp(str1, str2) == 0;
}

DiagnosticWarning diagnostic_string_to_warning(const char* string)
{
    // Check for any possible grouped warnings
    if (string_is(string, "") || string_is(string, "all"))
    {
        return Wall;
    }

    if (string_is(string, "extra"))
    {
        return Wextra;
    }

    if (string_is(string, "pedantic"))
    {
        return Wpedantic;
    }
    
    if (string_is(string, "error"))
    {
        return Werror;
    }

    if (string_is(string, "fatal-errors"))
    {
        return Wfatal_errors;
    }

    // Finally as a last resort go through all of the warnings that we could 
    // possible have
    for (size_t i = 0; i < warnings_count; i++)
    {
        if (string_is(string, warnings[i].name))
        {
            return warnings[i].type;
        }
    }

    return Wunknown;
}

const char* diagnostic_warning_to_string(DiagnosticWarning warning)
{
    // If it's between the range of the warnings that all have different option
    // names than return that name otherwise just return NULL.
    if (warning >= Wother && warning < WARNING_COUNT)
    {
        return warnings[warning].name;
    }

    return NULL;
}
