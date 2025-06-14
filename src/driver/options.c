#include "options.h"

#include "util/static_string.h"

void options_initialise(Options* opts)
{
    static_string_list_init(&opts->quote_path);
    static_string_list_init(&opts->bracket_path);
    static_string_list_init(&opts->system_path);
    static_string_list_init(&opts->after_path);
}

void options_free(Options* opts)
{
    static_string_free(&opts->starting_file);
    static_string_list_free(&opts->quote_path);
    static_string_list_free(&opts->bracket_path);
    static_string_list_free(&opts->system_path);
    static_string_list_free(&opts->after_path);
}
