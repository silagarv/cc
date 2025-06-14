#include "driver.h"

#include "driver/diagnostic.h"
#include "driver/options.h"
#include "driver/command_line.h"

#include "preprocessor/lexer.h"
#include "preprocessor/files.h"

int compiler_init(void)
{
    diagnostics_init();

    return 0; // In the future maybe we might have stuff that fails?
}

static int compile_translation_unit(Options* options)
{
    int ret_val = 0;

    Lexer lexer = {0};
    lexer_initialise(&lexer, options, NULL, NULL);

    if (lexer_push_start_file(&lexer, &options->starting_file))
    {
        ret_val = 1;

        goto bad_file;
    }

    TokenList tokens = {0};
    token_list_initialise(&tokens);

    if (lexer_tokenise(&lexer, &tokens))
    {
        ret_val = 1;

        goto bad_tokenisation;
    }

bad_tokenisation:
    token_list_free(&tokens);
bad_file:
    lexer_close(&lexer);

    return ret_val;
}

int compiler_main(int argc, char** argv)
{
    int ret_val = 0;

    if (compiler_init())
    {
        ret_val = 1;

        goto initisation_fail;
    }

    Options options = {0};
    options_initialise(&options);
    if (command_line_parse(&options, argc, argv))
    {
        ret_val = 1;

        goto bad_options;
    }

    if (compile_translation_unit(&options))
    {
        ret_val = 1;

        goto bad_translation_unit;
    }

bad_translation_unit:
bad_options:
    options_free(&options);
initisation_fail:
    return ret_val;
}
