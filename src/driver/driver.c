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

int compile_translation_unit(void)
{
    return 0;
}

int compiler_main(int argc, char** argv)
{
    int ret_val = 0;

    if (compiler_init())
    {
        // Currently dead code

        ret_val = 1;

        goto initisation_fail;
    }

    Options options = {0};
    if (command_line_parse(&options, argc, argv))
    {
        ret_val = 1;

        goto bad_options;
    }

    Lexer lexer = {0};
    TokenList tokens = {0};
    lexer_initialise(&lexer, NULL, NULL);

    if (lexer_push_start_file(&lexer, &options.file))
    {
        ret_val = 1;

        goto bad_file;
    }

    if (lexer_tokenise(&lexer, &tokens))
    {
        ret_val = 1;

        goto bad_tokenisation;
    }

bad_tokenisation:
bad_file:
    lexer_close(&lexer);
    static_string_free(&options.file);
bad_options:
    // Nothing extra for bad options
initisation_fail:
    return ret_val;
}
