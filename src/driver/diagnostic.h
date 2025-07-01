#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include <stdarg.h>

void diag_init(void);

void diag_fatal_error(const char* fmt, ...);
void diag_error(const char* fmt, ...);
void diag_warning(const char* fmt, ...);
void diag_note(const char* fmt, ...);
void diag_help(const char* fmt, ...);

void diag_verror(const char* fmt, va_list args);

#endif /* DIAGNOSTIC_H */
