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
    SourceManager sm = source_manager();
    DiagnosticManager dm = diagnostic_manager_init(&sm);

    Filepath path;
    bool file = get_filepath(&path, argv[1]);
    if (!file)
    {
        diagnostic_error(&dm, "no input files");
        goto no_input;
    }

    SourceFile* source = source_manager_create_filepath(&sm, path);
    if (!source)
    {
        diagnostic_error(&dm, "no such file '%s'", path.path);
        goto no_input;
    }

    Preprocessor pp;
    preprocessor_create(&pp, &dm, &sm, source);
    parse_translation_unit(&dm, &pp);
    diagnostic_emit_count(&dm);

    preprocessor_delete(&pp);
no_input:
    source_manager_delete(&sm);

    return 0;
}

int main(int argc, char** argv)
{
    return compiler_main(argc, argv);
}
