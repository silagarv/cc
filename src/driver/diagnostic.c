#include "diagnostic.h"

#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>

#include "util/panic.h"

enum DiagnosticKind 
{
    DIAGNOSTIC_FATAL,
    DIAGNOSTIC_ERROR,
    DIAGNOSTIC_WARNING,
    DIAGNOSTIC_NOTE,
    DIAGNOSTIC_HELP,
};
typedef enum DiagnosticKind DiagnosticKind;

struct DiagnosticColours 
{
    const char* fatal;
    const char* error;
    const char* warning;
    const char* note;
    const char* help;
    const char* white;
    const char* highlight;
    const char* reset_highlight;
    const char* reset_all;
};
typedef struct DiagnosticColours DiagnosticColours;

static const DiagnosticColours colour_off = 
{
    .fatal = "",
    .error = "",
    .warning = "",
    .note = "",
    .help = "",
    .white = "",
    .highlight = "",
    .reset_highlight = "",
    .reset_all = ""
};

static const DiagnosticColours colour_on = 
{
    .fatal = "\033[91m",
    .error = "\033[91m",
    .warning = "\033[93m",
    .note = "\033[94m",
    .help = "\033[96m",
    .white = "\033[97m",
    .highlight = "\033[1m",
    .reset_highlight = "\033[22m",
    .reset_all = "\033[0m"
};

static DiagnosticColours colours;

void diag_init(void)
{
    colours = colour_on;
}

static const char* kind_to_name(DiagnosticKind kind)
{
    switch (kind)
    {
        case DIAGNOSTIC_FATAL: return "fatal error";
        case DIAGNOSTIC_ERROR: return "error";
        case DIAGNOSTIC_WARNING: return "warning";
        case DIAGNOSTIC_NOTE: return "note";
        case DIAGNOSTIC_HELP: return "help";
        
        default:
            panic("invalid diagnostic kind");
            return NULL;
    }
}

static const char* kind_to_colour(DiagnosticKind kind)
{
    switch (kind)
    {
        case DIAGNOSTIC_FATAL: return colours.fatal;
        case DIAGNOSTIC_ERROR: return colours.error;
        case DIAGNOSTIC_WARNING: return colours.warning;
        case DIAGNOSTIC_NOTE: return colours.note;
        case DIAGNOSTIC_HELP: return colours.help;

        default:
            panic("invalid diagnostic kind");
            return NULL;
    }
}

static void finish(void)
{
    fprintf(stderr, "\n");
}

static void diag_internal(DiagnosticKind kind, const char* fmt, 
        va_list args)
{
    fprintf(stderr, "%s%s%s:%s%s ",
            colours.highlight,
            kind_to_colour(kind),
            kind_to_name(kind),
            colours.white,
            colours.reset_all
        );
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void diag_fatal_error(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diag_internal(DIAGNOSTIC_FATAL, fmt, args);
    va_end(args);

    finish();
}

void diag_error(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diag_internal(DIAGNOSTIC_ERROR, fmt, args);
    va_end(args);

    finish();
}

void diag_warning(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diag_internal(DIAGNOSTIC_WARNING, fmt, args);
    va_end(args);

    finish();
}

void diag_note(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diag_internal(DIAGNOSTIC_NOTE, fmt, args);
    va_end(args);

    finish();
}

void diag_help(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diag_internal(DIAGNOSTIC_HELP, fmt, args);
    va_end(args);

    finish();
}
