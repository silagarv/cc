#include "options.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"

static bool starts_with(const char* opt, const char* with)
{
    size_t len_with = strlen(with);
    if (strncmp(opt, with, len_with) == 0)
    {
        return true;
    }

    return false;
}

static CompilerOptions compiler_options_create(void)
{
    CompilerOptions opts =
    {
        .standard = LANG_STANDARD_DEFAULT,
        .werror = false,
        .infile = NULL
    };

    return opts;
} 

CompilerOptions parse_compiler_options(DiagnosticManager* dm, int argc,
        char** argv, bool* okay)
{
    *okay = true;

    CompilerOptions opts = compiler_options_create();

    // Parse all of our options
    for (int idx = 1; idx < argc; idx++)
    {
        char* arg = argv[idx];
        if (strcmp(arg, "-Werror") == 0)
        {
            opts.werror = true;
        }
        else if (strcmp(arg, "-Wno-error") == 0)
        {
            opts.werror = false;
        }
        else // We have a file
        {
            if (opts.infile == NULL)
            {
                opts.infile = arg;
            }
            else
            {
                diagnostic_error(dm, "multiple input filenames provided "
                        "(first two filenames are '%s' and '%s')", opts.infile,
                        arg);
                *okay = false;
                goto done;
            }
        }
    }

done:
    // Check if we got an infile or not
    if (opts.infile == NULL)
    {
        diagnostic_error(dm, "no input file");
        *okay = false;
    }

    // Finish the options setting up some values
    if (opts.standard == LANG_STANDARD_DEFAULT)
    {
        opts.standard = LANG_STANDARD_C99;
    }

    return opts;
}
