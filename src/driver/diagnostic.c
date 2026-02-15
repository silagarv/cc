#include "diagnostic.h"

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <unistd.h>
#include <assert.h>

#include "files/file_manager.h"
#include "util/panic.h"

#include "files/line_map.h"
#include "files/location.h"
#include "files/source_manager.h"

#define MINIMUM_LINNUM_LENGTH (4)

typedef enum DiagnosticKind {
    DIAGNOSTIC_FATAL,
    DIAGNOSTIC_ERROR,
    DIAGNOSTIC_WARNING,
    DIAGNOSTIC_NOTE,
    DIAGNOSTIC_HELP,
} DiagnosticKind;

struct DiagnosticColours {
    char* fatal;
    char* error;
    char* warning;
    char* note;
    char* help;
    char* white;
    char* highlight;
    char* reset_highlight;
    char* reset_all;
    char* caret;
};

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
    .reset_all = "",
    .caret = ""
};

static const DiagnosticColours colour_on = 
{
    .fatal = "\033[91m",
    .error = "\033[91m",
    .warning = "\033[93m",
    .note = "\033[94m",
    .help = "\033[92m",
    .white = "\033[97m",
    .highlight = "\033[1m",
    .reset_highlight = "\033[22m",
    .reset_all = "\033[0m",
    .caret = "\033[92m"
};

DiagnosticManager diagnostic_manager_init(SourceManager* sm)
{
    DiagnosticManager dm;
    dm.sm = sm;
    dm.colours = (DiagnosticColours*) (isatty(STDERR_FILENO) ? &colour_on 
            : &colour_off); // Cast away since we never change the values

    dm.warning_count = 0;
    dm.error_count = 0;

    dm.disable_warnings = false;
    dm.werror = false;

    return dm;
}

void diagnostic_manager_set_sm(DiagnosticManager* dm, SourceManager* sm)
{
    dm->sm = sm;
}

void diagnostic_manager_set_werror(DiagnosticManager* dm, bool value)
{
    dm->werror = value;
}

void diagnostic_manager_set_disable_warnings(DiagnosticManager* dm, bool value)
{
    dm->disable_warnings = value;
}

size_t diagnostic_manager_get_warning_count(const DiagnosticManager* dm)
{
    return dm->warning_count;
}

size_t diagnostic_manager_get_error_count(const DiagnosticManager* dm)
{
    return dm->error_count;
}

void diagnostic_emit_count(DiagnosticManager* dm)
{
    size_t w_count = diagnostic_manager_get_warning_count(dm);
    size_t e_count = diagnostic_manager_get_error_count(dm);

    if (e_count == 0 && w_count == 0)
    {
        return;
    }

    if (e_count == 0 && w_count > 0)
    {
        fprintf(stderr, "%zu warning%s generated.\n", w_count,
                w_count > 1 ? "s" : "");
        return;
    }

    if (e_count && w_count > 0)
    {
        fprintf(stderr, "%zu warning%s and ", w_count,
                w_count > 1 ? "s" : "");
    }

    fprintf(stderr, "%zu error%s generated.\n",
            e_count, e_count > 1 ? "s" : "");
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

static const char* kind_to_colour(DiagnosticManager* dm, DiagnosticKind kind)
{
    switch (kind)
    {
        case DIAGNOSTIC_FATAL: return dm->colours->fatal;
        case DIAGNOSTIC_ERROR: return dm->colours->error;
        case DIAGNOSTIC_WARNING: return dm->colours->warning;
        case DIAGNOSTIC_NOTE: return dm->colours->note;
        case DIAGNOSTIC_HELP: return dm->colours->help;

        default:
            panic("invalid diagnostic kind");
            return NULL;
    }
}

void diagnostic_internal(DiagnosticManager* dm, DiagnosticKind kind,
        const char* fmt, va_list ap)
{
    if (kind == DIAGNOSTIC_ERROR)
    {
        dm->error_count++;
    }
    else if (kind == DIAGNOSTIC_WARNING)
    {
        dm->warning_count++;
    }

    fprintf(stderr, "%s%scc: %s%s: %s%s", dm->colours->white,
            dm->colours->highlight,
            kind_to_colour(dm, kind),
            kind_to_name(kind),
            dm->colours->reset_all,
            dm->colours->white);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

void diagnostic_fatal_error(DiagnosticManager* dm, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal(dm, DIAGNOSTIC_FATAL, fmt, ap);
    va_end(ap);
}

void diagnostic_error(DiagnosticManager* dm, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal(dm, DIAGNOSTIC_ERROR, fmt, ap);
    va_end(ap);
}

void diagnostic_warning(DiagnosticManager* dm, const char* fmt, ...)
{
    // In clang disabling warnings takes priority over having them at all
    if (dm->disable_warnings)
    {
        return;
    }

    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal(dm, dm->werror ? DIAGNOSTIC_ERROR : DIAGNOSTIC_WARNING,
            fmt, ap);
    va_end(ap);
}

void diagnostic_note(DiagnosticManager* dm, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal(dm, DIAGNOSTIC_NOTE, fmt, ap);
    va_end(ap);
}

void diagnostic_help(DiagnosticManager* dm, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal(dm, DIAGNOSTIC_HELP, fmt, ap);
    va_end(ap);
}

// Get the number length that we should print for. It gets a minimum of
// MINIMUM_LINNUM_LENGTH, and the length (in digits) of the number
static int get_number_length(uint32_t number)
{
    // We know an upper limit on how many digits we can have since we can have
    // up to 4,000,000,000+ as our largest line number. So, we make an 
    // adequetely sized buffer that we can print into.
    char number_tmp[10];
    int len = snprintf(number_tmp, sizeof(number_tmp) / sizeof(number_tmp[0]),
            "%u", number);
    assert(len > 0);
    return len < MINIMUM_LINNUM_LENGTH ? MINIMUM_LINNUM_LENGTH : len;
}

// A very basic line printer for our diagnostic. Note: this may have to be 
// ripped out and rewritten once we have macros and a preprocessor 
void diagnostic_print_snippet(DiagnosticManager* dm, DiagnosticKind kind,
        SourceFile* file, Location loc, uint32_t line)
{
    // Now try to get the line to print
    Location start_line = line_map_get_line_start(&file->line_map, loc);
    assert(loc >= start_line);

    Location start_loc = source_file_get_start_location(file);
    assert(loc >= start_loc);
    Location start_offset = start_line - start_loc;

    // Get the file information
    const FileBuffer* fb = source_file_get_buffer(file);
    const char* raw = file_buffer_get_start(fb);
    const char* end = file_buffer_get_end(fb);

    // Print the part at the start which gives the line number we are on
    int line_number_len = get_number_length(line);
    fprintf(stderr, " %*u | ", line_number_len, line);

    // Print the line
    const char* line_start = raw + start_offset;
    char buff[BUFSIZ + 1];
    size_t buff_pos = 0;

    bool printed_newline = false;
    for (const char* current = line_start; ; current++)
    {
        char current_char = *current;
        
        // Add the char to the buffer to print
        buff[buff_pos++] = current_char;

        // If we have filled the buffer up then null terminate and dump to 
        // stderr so that it can be printed. Also remembering to reset the 
        // buffer
        if (buff_pos == BUFSIZ)
        {
            buff[BUFSIZ] = '\0';
            buff_pos = 0;
            fprintf(stderr, "%s", buff);
        }

        // Otherwise if we have a line terminator null terminate the buffer and
        // print it along with the line to stderr.
        if (current_char == '\r' || current_char == '\n' || current == end)
        {
            // Note: the newline character should already be in the buffer for
            // printing so just null terminate. If the buffer was full already
            // it would have also been flushed too :)
            buff[buff_pos++] = '\0';
            buff_pos = 0;
            fprintf(stderr, "%s", buff);

            if (current_char == '\r' || current_char == '\n')
            {
                printed_newline = true;
            }
            break;
        }
    }

    // Rare case: empty file basically or no newline at eof
    if (!printed_newline)
    {
        fprintf(stderr, "\n");
    }

    // Now we want to print the second part of the linnum thing. So we print
    // 2 + line_number_len spaces and then our '|'
    fprintf(stderr, " %*s | ", line_number_len, "");

    // Now print a caret afterwards so we have a better idea of where it is
    Location empty = loc - start_line;
    fprintf(stderr, "%*s", (int) empty, "");
    fprintf(stderr, "%s%s^\n%s", dm->colours->highlight, dm->colours->caret,
            dm->colours->reset_all);
}

void diagnostic_internal_at(DiagnosticManager* dm, DiagnosticKind kind,
        Location loc, const char* fmt, va_list ap)
{
    assert(dm->sm);
    assert(loc != LOCATION_INVALID);

    if (kind == DIAGNOSTIC_ERROR)
    {
        dm->error_count++;
    }
    else if (kind == DIAGNOSTIC_WARNING)
    {
        dm->warning_count++;
    }

    SourceFile* sf = source_manager_from_location(dm->sm, loc);
    ResolvedLocation resolved = line_map_resolve_location(&sf->line_map, loc);
    uint32_t line = resolved.line;
    uint32_t col = resolved.col;

    // Print the initial part
    fprintf(stderr, "%s%s%s:%u:%u: ", dm->colours->white,
            dm->colours->highlight, sf->file_buffer->path.path, line, col);
    fprintf(stderr, "%s%s:%s%s ",
            kind_to_colour(dm, kind),
            kind_to_name(kind),
            dm->colours->reset_all,
            dm->colours->white
        );
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "%s\n", dm->colours->reset_all);

    diagnostic_print_snippet(dm, kind, sf, loc, line);
}

void diagnostic_fatal_error_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal_at(dm, DIAGNOSTIC_FATAL, loc, fmt, ap);
    va_end(ap);
}

void diagnostic_error_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal_at(dm, DIAGNOSTIC_ERROR, loc, fmt, ap);
    va_end(ap);
}

void diagnostic_warning_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...)
{
    // In clang disabling warnings takes priority over having them at all
    if (dm->disable_warnings)
    {
        return;
    }

    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal_at(dm,
            dm->werror ? DIAGNOSTIC_ERROR : DIAGNOSTIC_WARNING, loc, fmt, ap);
    va_end(ap);
}

void diagnostic_note_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal_at(dm, DIAGNOSTIC_NOTE, loc, fmt, ap);
    va_end(ap);
}

void diagnostic_help_at(DiagnosticManager* dm, Location loc,
        const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    diagnostic_internal_at(dm, DIAGNOSTIC_HELP, loc, fmt, ap);
    va_end(ap);
}
