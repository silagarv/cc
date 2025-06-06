#ifndef COMMAND_LINE_H
#define COMMAND_LINE_H

#include <stdbool.h>

#include "driver/options.h"

bool command_line_parse(Options* options, int argc, char** argv);

#endif /* COMMAND_LINE_H */
