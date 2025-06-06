#include "driver.h"

#include "driver/diagnostic.h"
#include "driver/options.h"
#include "driver/command_line.h"

#include "preprocessor/lexer.h"
#include "preprocessor/files.h"

int compiler_main(int argc, char** argv)
{
    diagnostics_init();

    int ret_val = 0;

    Options options = {0};
    if (!command_line_parse(&options, argc, argv))
    {
        ret_val = 1;

        goto bad_options;
    }

    Lexer lexer = {0};
    TokenList tokens = {0};
    lexer_initialise(&lexer, NULL, NULL);

    if (!lexer_push_start_file(&lexer, &options.file))
    {
        ret_val = 1;

        goto bad_file;
    }

    if (!lexer_tokenise(&lexer, &tokens))
    {
        ret_val = 1;

        goto bad_tokenisation;
    }

bad_tokenisation:
bad_file:
    lexer_close(&lexer);
    static_string_free(&options.file);
bad_options:
    return ret_val;
}
