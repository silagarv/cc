#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "files/filepath.h"
#include "files/source_manager.h"

#include "driver/diagnostic.h"
#include "driver/options.h"

#include "lex/preprocessor.h"

#include "parse/parser.h"

bool get_filepath(Filepath* path, const char* argv)
{
    if (argv == NULL)
    {
        return false;
    }

    size_t len = strlen(argv);
    if (len >= FILENAME_MAX)
    {
        return false;
    }

    strcpy(path->path, argv);
    path->len = len;

    return true;
}

int compiler_main(int argc, char** argv)
{
    DiagnosticManager dm = diagnostic_manager_init(NULL);

    bool okay;
    CompilerOptions opts = parse_compiler_options(&dm, argc, argv, &okay);
    if (!okay)
    {
        return EXIT_FAILURE;
    }

    // Create and set the sm, as the diagnostic managers sm
    SourceManager sm = source_manager();
    diagnostic_manager_set_sm(&dm, &sm);
    diagnostic_manager_set_werror(&dm, opts.werror);

    // Extract the filepath from the options
    Filepath path;
    get_filepath(&path, opts.infile);

    int status = 0;
    SourceFile* source = source_manager_create_filepath(&sm, path);
    if (!source)
    {
        diagnostic_error(&dm, "no such file '%s'", path.path);
        status = EXIT_FAILURE;
        goto no_file;
    }

    Preprocessor pp;
    preprocessor_create(&pp, &dm, &sm, source);
    parse_translation_unit(&dm, &pp);
    preprocessor_delete(&pp);
    diagnostic_emit_count(&dm);

no_file:
    source_manager_delete(&sm);

    return status;
}

int main(int argc, char** argv)
{
    return compiler_main(argc, argv);
}
