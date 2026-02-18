#include "options.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"
#include "driver/warning.h"

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

static bool string_is(const char* value, const char* string)
{
    return strcmp(value, string) == 0;
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

static bool argument_starts_with(const char* value, const char* arg)
{
    size_t value_length = strlen(value);
    return strncmp(value, arg, value_length) == 0;
}

static bool f_argument(const char* f, const char* arg)
{
    if (!argument_starts_with("-f", arg))
    {
        return false;
    }

    // Ignore the -f part;
    arg += strlen("-f");

    // Return if we are that
    return argument_is(f, arg);
}

static bool fyes_no_argument(const char* fyes_no, const char* arg, bool* truth)
{
    if (!argument_starts_with("-f", arg))
    {
        return false;
    }

    // Ignore the -f part;
    arg += strlen("-f");

    bool no = argument_starts_with("no-", arg);
    if (no == true)
    {
        arg += strlen("no-");
    }

    // Now we need to see if we are the fyes_no part
    if (!argument_is(fyes_no, arg))
    {
        return false;
    }

    // Set the truth to be no if no and yes if yes
    *truth = !no;
    
    return true;
}

static CompilerOptions compiler_options_create(void)
{
    CompilerOptions opts =
    {
        .standard = LANG_STANDARD_DEFAULT,
        .syntax_only = false,
        .werror = false,
        .infile = NULL,
        .outfile = NULL,
    };

    return opts;
}

static void compiler_options_finish(CompilerOptions* opts,
        DiagnosticManager* dm, CommandLineState* state)
{
    // Set the default language if it was not done already
    if (opts->standard == LANG_STANDARD_DEFAULT)
    {
        opts->standard = LANG_STANDARD_C99;
    }

    // Check the optimisation level and give a sensible default level if the 
    // user did not specify any level.
    if (opts->opt_level == OPT_LEVEL_UNSPECIFIED)
    {
        opts->opt_level = OPT_LEVEL_0;
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
        state->options_okay = false;
        return true;
    }
    
    // Get and set the output filename
    opts->outfile = state_get_argument(state);

    state_eat_argument(state);
    return true;
}

static bool handle_standard(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    assert(is_compiler_argument(argument));

    static const struct {
        const char* value;
        LangStandard standard;
    } standards[] =
    {
        // C89 Standards
        { "c89",                LANG_STANDARD_C89},
        { "c90",                LANG_STANDARD_C89},
        { "iso9899:1990",       LANG_STANDARD_C89},

        // C99 Standards
        { "c99",                LANG_STANDARD_C99},
        { "iso9899:1999",       LANG_STANDARD_C99},

        // C11 Standards
        { "c11",                LANG_STANDARD_C11},
        { "iso9899:2011",       LANG_STANDARD_C11},

        // C17 Standards
        { "c17",                LANG_STANDARD_C17},
        { "iso9899:2017",       LANG_STANDARD_C17},
        { "c18",                LANG_STANDARD_C17},
        { "iso9899:2018",      LANG_STANDARD_C17},

        // C23 Standards
        { "c23",                LANG_STANDARD_C23}
    };
    static size_t num_standards = sizeof(standards) / sizeof(standards[0]);

    // Make sure that the arugment has an '=' sign
    if (!argument_starts_with("-std=", argument))
    {
        return false;
    }

    // Eat the argument since we don't need it anymore
    state_eat_argument(state);

    // Get the remaining part of the argument.
    size_t prefix_len = strlen("-std=");
    char* remaining = argument + prefix_len;

    // Find the standard that this corrosponds to
    for (size_t i = 0; i < num_standards; i++)
    {
        if (string_is(standards[i].value, remaining))
        {
            opts->standard = standards[i].standard;
            if (standards[i].standard != LANG_STANDARD_C99)
            {
                diagnostic_warning(dm, Wexperimental, "support of language "
                        "standards other than C99 is experimental");
            }
            return true;
        }
    }

    // Report error and set options state
    diagnostic_error(dm, "invalid value '%s' in 'std='", remaining); 
    state->options_okay = false;
    return true;
}

static bool handle_syntax_only(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!f_argument("syntax-only", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->syntax_only = true;

    return true;
}

static bool handle_s(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-S", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->dump_assembly = true;
    
    return true;
}

static bool handle_c(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-c", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->compile_only = true;
    
    return true;
}

static bool handle_e(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-E", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->preprocess_only = true;
    
    return true;
}

static bool handle_h(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-H", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->print_headers = true;
    
    return true;
}

static bool handle_p(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-P", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->no_line_markers = true;
    
    return true;
}

static bool handle_help(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("--help", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->help = true;
    
    return true;
}

static bool handle_version(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("--version", argument)
            && !argument_is("-version", argument))
    {
        return false;
    }

    // eat the argumen
    state_eat_argument(state);

    opts->version = true;
    
    return true;
}

static bool handle_opt_level(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_starts_with("-O", argument))
    {
        return false;
    }

    // Make sure to eat the argument
    state_eat_argument(state);

    // Otherwise we now need to determine the optimisation level
    char* moved_argument = argument + strlen("-O");
    if (argument_is("", moved_argument))
    {
        // Special case of '-O' enable level 1 optimisations.
        opts->opt_level = OPT_LEVEL_1;
    }
    else if (argument_is("0", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_0;
    }
    else if (argument_is("1", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_1;
    }
    else if (argument_is("2", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_2;
    }
    else if (argument_is("3", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_3;
    }
    else if (argument_is("g", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_G;
    }
    else if (argument_is("s", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_S;
    }
    else if (argument_is("z", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_Z;
    }
    else if (argument_is("fast", moved_argument))
    {
        opts->opt_level = OPT_LEVEL_FAST;
    }
    else
    {
        diagnostic_error(dm, "invalid value '%s' in '%s'",
                moved_argument, argument);
        state->options_okay = false;
    }

    return true;
}

static bool handle_w(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-w", argument))
    {
        return false;
    }

    // Make sure to eat the argument
    state_eat_argument(state);

    opts->w = true;

    return true;
}

static bool handle_warning(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    // TODO: for now this just handles warning options as they come but in the
    // TODO: future we might want to save all of the handling for later?

    // Must start with -W but don't get any or the miscelanious options for
    // adding arguments to assembler, preprocessor, or linker
    if (!argument_starts_with("-W", argument)
        || argument_starts_with("-Wa,", argument)
        || argument_starts_with("-Wp,", argument)
        || argument_starts_with("-Wl,", argument))
    {
        return false;
    }

    // eat the argument since we don't need it anymore
    state_eat_argument(state);

    // Skip the -W prefix
    argument += strlen("-W");

    // Special case for -Werror since we want to be special in our handling of
    // that...
    bool no = false;
    bool error = false;

    // Check if we get a no prefix to disable a warning
    if (argument_starts_with("no-", argument))
    {
        no = true;
        argument += strlen("no-");
    }

    // Also check if we get an error= prefix to handle that
    if (argument_starts_with("error=", argument))
    {
        error = true;
        argument += strlen("error=");
    }

    // Now attempt to get the name of the warning option
    DiagnosticWarning warning = diagnostic_string_to_warning(argument);
    
    // For now just print a warning about not knowing about the warning option
    // if it wasn't known and do nothing about any of the other warnings.
    if (warning == Wunknown)
    {
        diagnostic_warning(dm, Wunknown_warning_opt, "unknown warning option "
                "'-W%s%s'", error ? "error=" : "", argument);
        return true;
    }
    
    diagnostic_manager_handle_warning_option(dm, warning, no, error);
    return true;
}

static bool handle_trigraphs(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    if (!argument_is("-trigraphs", argument))
    {
        return false;
    }

    // eat the argument since we don't need it anymore
    state_eat_argument(state);

    // Set the trigraphs flag to true
    opts->trigraphs = true;

    return true;
}

static bool handle_unknown(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state, char* argument)
{
    // We didn't find anything so eat the argument and error.
    diagnostic_error(dm, "unknown argument: '%s'", argument);
    state_eat_argument(state);
    state->options_okay = false;

    return true;
}

static void parse_compiler_option(CompilerOptions* opts, DiagnosticManager* dm,
        CommandLineState* state)
{
    char* current_arg = state_get_argument(state);
    
    // First see if this is potentially a file for us to use, and handle if so
    // otherwise go and try to handle any potential argument that we have
    if (!is_compiler_argument(current_arg))
    {
        handle_infile(opts, dm, state, current_arg);
        return;
    }

    if (handle_outfile(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_standard(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_syntax_only(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_s(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_c(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_e(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_h(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_p(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_help(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_version(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_opt_level(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_w(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_warning(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_trigraphs(opts, dm, state, current_arg))
    {
        return;
    }
    else if (handle_unknown(opts, dm, state, current_arg))
    {
        return;
    }
    else
    {
        panic("unhandled command line argument");
    }
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
