#ifndef COMMAND_LINE_H
#define COMMAND_LINE_H

#include "driver/options.h"

int command_line_parse(Options* options, int argc, char** argv);

#endif /* COMMAND_LINE_H */
