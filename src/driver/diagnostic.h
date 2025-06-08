#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "preprocessor/location.h"
#include "preprocessor/line.h"

void diagnostics_init(void);

void fatal_error(const char* fmt, ...);
void error(const char* fmt, ...);
void warning(const char* fmt, ...);
void note(const char* fmt, ...);
void help(const char* fmt, ...);

// void fatal_error_location(Location loc, const char* fmt, ...);

#endif /* DIAGNOSTIC_H */
