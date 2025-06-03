#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

void diagnostics_init(void);

void internal_compiler_error(char* fmt);

void fatal_error(char* fmt, ...);
void error(char* fmt, ...);
void warning(char* fmt, ...);
void note(char* fmt, ...);
void help(char* fmt, ...);

#endif /* DIAGNOSTIC_H */
