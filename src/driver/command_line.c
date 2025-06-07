#include "command_line.h"

#include <string.h>

#include "util/static_string.h"

#include "driver/options.h"
#include "driver/diagnostic.h"

bool is_argument(const char* maybe_arg)
{
    // Check we didn't get "-" for stdin to be used in the future
    if (maybe_arg[0] == '-' && maybe_arg[1] != '\0')
    {
        return true;
    }

    return false;
}

bool is_argument_equal(const char* maybe_arg, const char* desired)
{
    if (!is_argument(maybe_arg))
    {
        return false;
    }

    return (strcmp(maybe_arg + 1, desired) == 0);
}

static bool parse_preprocessor_args(Options* options, int* argc, char** argv)
{
    return false;
}

int command_line_parse(Options* options, int argc, char** argv)
{
    if (argc == 1)
    {
        // TODO: improve this in future to print options or tell user how to
        // TODO: print options at the bare minimum
        fatal_error("no input files provided; terminating compilation");

        return 1;
    }

    static_string_init_copy(&options->file, argv[1]);

    bool triggered_warning = false;
    for (int arg = 2; arg < argc; arg++)
    {
        if (is_argument_equal(argv[arg], "fno-line-notes"))
        {
            note("yat");
        }

        warning("only one input file supported; ignoring file '%s'", argv[arg]);
        triggered_warning = true;
    }

    if (triggered_warning)
    {
        help("multiple files will be supported in the future");
    }

    return 0;
}
