#include "command_line.h"

#include <string.h>

#include "util/static_string.h"

#include "driver/options.h"
#include "driver/diagnostic.h"

bool is_argument(char* maybe_arg)
{
    // Check we didn't get "-" for stdin to be used in the future
    if (maybe_arg[0] == '-' && maybe_arg[1] != '\0')
    {
        return true;
    }

    return false;
}

bool is_argument_type(char* maybe_arg, char prefix)
{
    if (maybe_arg[0] == '-' && maybe_arg[1] == prefix)
    {
        return true;
    }

    return false;
}

bool command_line_parse(Options* options, int argc, char** argv)
{
    if (argc == 1)
    {
        // TODO: improve this in future to print options or tell user how to
        // TODO: print options at the bare minimum
        fatal_error("no input files provided; terminating compilation");

        return false;
    }

    static_string_init_copy(&options->file, argv[1]);

    bool triggered_warning = false;
    for (int arg = 2; arg < argc; arg++)
    {
        warning("only one input file supported; ignoring file '%s'", argv[arg]);
        triggered_warning = true;
    }

    if (triggered_warning)
    {
        help("multiple files will be supported in the future");
    }

    return true;
}
