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

typedef enum DiagnosticKind {
    DIAGNOSTIC_FATAL,
    DIAGNOSTIC_ERROR,
    DIAGNOSTIC_WARNING,
    DIAGNOSTIC_NOTE,
    DIAGNOSTIC_HELP,
} DiagnosticKind;

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
    .note = "\033[90m",
    .help = "\033[92m",
    .white = "\033[97m",
    .highlight = "\033[1m",
    .reset_highlight = "\033[22m",
    .reset_all = "\033[0m"
};

DiagnosticManager diagnostic_manager_init(SourceManager* sm)
{
    DiagnosticManager dm;
    dm.sm = sm;
    dm.colours = isatty(STDERR_FILENO) ? colour_on : colour_off;

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
        case DIAGNOSTIC_FATAL: return dm->colours.fatal;
        case DIAGNOSTIC_ERROR: return dm->colours.error;
        case DIAGNOSTIC_WARNING: return dm->colours.warning;
        case DIAGNOSTIC_NOTE: return dm->colours.note;
        case DIAGNOSTIC_HELP: return dm->colours.help;

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
    
    fprintf(stderr, "%scc: %s%s:%s%s ",
            dm->colours.highlight,
            kind_to_colour(dm, kind),
            kind_to_name(kind),
            dm->colours.white,
            dm->colours.reset_all
        );
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
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

// A very basic line printer for our diagnostic. Note: this may have to be 
// ripped out and rewritten once we have macros and a preprocessor 
void diagnostic_print_line(DiagnosticManager* dm, SourceFile* file,
        Location loc)
{
    // Now try to get the line to print
    Location start_line = line_map_get_line_start(&file->line_map, loc);
    assert(loc >= start_line);

    Location start_loc = source_file_get_start_location(file);
    assert(loc >= start_loc);
    Location start_offset = start_line - start_loc;

    const FileBuffer* fb = source_file_get_buffer(file);
    const char* raw = file_buffer_get_start(fb);
    const char* end = file_buffer_get_end(fb);

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
        printf("\n");
    }

    // Now print a caret afterwards so we have a better idea of where it is
    Location empty = loc - start_line;
    for (Location i = 0; i < empty; i++)
    {
        printf("%.*s", (int) empty, " ");
    }
    printf("^\n");
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
    fprintf(stderr, "%s%s:%u:%u: ", dm->colours.highlight,
            sf->file_buffer->path.path, line, col);
    fprintf(stderr, "%s%s:%s ",
            kind_to_colour(dm, kind),
            kind_to_name(kind),
            dm->colours.reset_all
        );
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "%s\n", dm->colours.reset_all);

    diagnostic_print_line(dm, sf, loc);
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
