#include "diagnostic.h"

#include <stdarg.h>
#include <stdio.h>
#include <stddef.h>

#include "frontend/source.h"
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
    .note = "\033[96m",
    .help = "\033[92m",
    .white = "\033[97m",
    .highlight = "\033[1m",
    .reset_highlight = "\033[22m",
    .reset_all = "\033[0m"
};

static DiagnosticColours colours;

void diagnostics_init(void)
{
    colours = colour_on;
}
const char* diagnostic_kind_to_name(DiagnosticKind kind)
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

const char* diagnostic_kind_to_colour(DiagnosticKind kind)
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

void internal_compiler_error(char* msg)
{
    panic(msg);
}

static void diagnostic_internal(DiagnosticKind kind, char* fmt, va_list args)
{
    fprintf(stderr, "%s%s%s:%s%s ",
            colours.highlight,
            diagnostic_kind_to_colour(kind),
            diagnostic_kind_to_name(kind),
            colours.white,
            colours.reset_all
        );
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n\n");
}

void fatal_error(char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_FATAL, fmt, args);
    va_end(args);
}

void error(char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_ERROR, fmt, args);
    va_end(args);
}

void warning(char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_WARNING, fmt, args);
    va_end(args);
}

void note(char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_NOTE, fmt, args);
    va_end(args);
}

void help(char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_HELP, fmt, args);
    va_end(args);
}

void print_error_line(Line* line, char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    diagnostic_internal(DIAGNOSTIC_ERROR, fmt, args);
    va_end(args);

    fprintf(stderr, "%s\n", line_get_ptr(line));
    if (!line->ending_newline)
    {
        fprintf(stderr, "\n");
    }
}
