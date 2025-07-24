#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>

// TODO: should we implement a diagnostic queue maybe? so that we can have 
// a multithreaded compiler with different translation units being compiled
// simultaneously and then have diagnostics in flight from all of them? and then
// also get LTO for free since we have all of the translation units loaded into
// memory already

void diag_init(void);

void diag_fatal_error(const char* fmt, ...);
void diag_error(const char* fmt, ...);
void diag_warning(const char* fmt, ...);
void diag_note(const char* fmt, ...);
void diag_help(const char* fmt, ...);

void diag_verror(const char* fmt, va_list args);

#endif /* DIAGNOSTIC_H */
