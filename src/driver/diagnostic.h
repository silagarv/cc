#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

void diag_init(void);

void diag_fatal_error(const char* fmt, ...);
void diag_error(const char* fmt, ...);
void diag_warning(const char* fmt, ...);
void diag_note(const char* fmt, ...);
void diag_help(const char* fmt, ...);

#endif /* DIAGNOSTIC_H */
