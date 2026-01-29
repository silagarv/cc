#include "options.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"

typedef struct CommandLineState {
    const int argc;
    char** const argv;
    int i;

    bool options_okay;
    bool exit_early;
} CommandLineState;

bool state_has_next_argument(const CommandLineState* state)
{
    return state->i != state->argc;
}

void state_eat_argument(CommandLineState* state)
{
    state->i++;
}

char* state_get_argument(const CommandLineState* state)
{
    return state->argv[state->i];
}

static bool is_compiler_argument(const char* arg)
{
    assert(arg != NULL);

    // Exclude '-' from being a compiler argument since it means read from stdin
    return arg[0] == '-' && arg[1] != '\0';
}

static bool argument_is(const char* value, const char* arg)
{
    return strcmp(arg, value) == 0;
}

static CompilerOptions compiler_options_create(void)
{
    CompilerOptions opts =
    {
        .standard = LANG_STANDARD_DEFAULT,
        .werror = false,
        .infile = NULL,
        .outfile = NULL,
    };

    return opts;
}

static void compiler_options_finish(CompilerOptions* opts,
        DiagnosticManager* dm, CommandLineState* state)
{
    // Make sure we have an input file
    if (opts->infile == NULL)
    {
        diagnostic_error(dm, "no input files");
        state->options_okay = false;
    }

    // Set the default language if required
    if (opts->standard == LANG_STANDARD_DEFAULT)
    {
        opts->standard = LANG_STANDARD_C99;
    }
}

static void handle_infile(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    // First check that we didn't have a file already.
    if (opts->infile != NULL)
    {
        diagnostic_error(dm, "multiple input filenames provided (first two "
                "filenames are '%s' and '%s')", opts->infile, argument);
        state->options_okay = false;
        state->exit_early = true;
        return;
    }
    else
    {
        opts->infile = argument;
    }

    // Unconditionally eat the argument
    state_eat_argument(state);
}

static bool handle_outfile(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    assert(is_compiler_argument(argument));

    if (!argument_is("-o", argument))
    {
        return false;
    }

    // Eat this argument so we can get to the filename
    state_eat_argument(state);

    // Check for a filename
    if (!state_has_next_argument(state))
    {
        diagnostic_error(dm, "missing filename after '-o'");
        return true;
    }
    
    // Get and set the output filename
    opts->outfile = state_get_argument(state);

    state_eat_argument(state);
    return true;
}

static void parse_compiler_option(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state)
{
    char* current_arg = state_get_argument(state);
    
    // First see if this is potentially a file for us to use.
    if (!is_compiler_argument(current_arg))
    {
        handle_infile(opts, dm, state, current_arg);
        return;
    }

    // Otherwise we need to handle an actual argument here
    if (handle_outfile(opts, dm, state, current_arg))
    {
        return;
    }

    diagnostic_error(dm, "unknown argument '%s'", current_arg);

    // We didn't find anything so eat the argument and error.
    state_eat_argument(state);
}

bool parse_compiler_options(CompilerOptions* opts, DiagnosticManager* dm,
        int argc, char** argv)
{
    // First create our options so that they are empty.
    *opts = compiler_options_create();

    // Now create our command line state and keep parsing arguments until we 
    // have none left to go
    CommandLineState state = {argc, argv, 1, true, false};
    while (state_has_next_argument(&state))
    {
        parse_compiler_option(opts, dm, &state);

        // If we got some sort of fatal error whilst parsing our command line
        // arguments we simply cannot continue and just kill ourselves here.
        if (state.exit_early)
        {
            return false;
        }
    }

    // Finish our options and return if we are okay to proceed
    compiler_options_finish(opts, dm, &state);

    return state.options_okay;
}
