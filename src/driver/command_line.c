#include "command_line.h"

#include "util/static_string.h"

#include "driver/options.h"
#include "driver/diagnostic.h"

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
