#include "driver.h"

#include <stdlib.h>
#include <string.h>

#include "driver/diagnostic.h"
#include "driver/options.h"

#include "files/source_manager.h"

#include "lex/preprocessor.h"

#include "parse/parser.h"
#include "parse/ast.h"

#include "codegen/codegen.h"

static bool get_filepath(Filepath* path, const char* argv)
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

static bool compiler_driver_process_translation_unit(CompilerDriver* driver)
{
    bool okay = true;

    // Initialise our source manement
    SourceManager sm = source_manager();
    diagnostic_manager_set_sm(&driver->dm, &sm);
    diagnostic_manager_set_werror(&driver->dm, false);

    // Get our filepath and attempt to create the file here.
    Filepath path;
    get_filepath(&path, driver->options.infile);
    SourceFile* source = source_manager_create_filepath(&sm, path);
    if (!source)
    {
        diagnostic_error(&driver->dm, "no such file or directory '%s'",
                path.path);
        okay = false;
        goto no_file;
    }

    // Now attempt to parse the translation unit here.
    Preprocessor pp;
    preprocessor_create(&pp, &driver->dm, &sm, source);

    // Parse the translation unit
    Ast ast = parse_translation_unit(&driver->dm, &pp);

    // Finally, check if there were any errors during parsing
    if (diagnostic_manager_get_error_count(&driver->dm) != 0)
    {
        okay = false;
        goto no_codegen;
    }

    // Now try to do codegen for the AST...
    codegen_translation_unit(&ast);

    // And make sure to free the ast and the preprocessor
no_codegen:
    ast_delete(&ast);
    preprocessor_delete(&pp);

no_file:
    source_manager_delete(&sm);

    return okay;
}

static int compiler_driver_execute(CompilerDriver* driver)
{
    // First try to parse the translation unit.
    if (compiler_driver_process_translation_unit(driver) == false)
    {
        return EXIT_FAILURE;
    }

    // Finally, emit the diagnostic count right at the very end.
    return EXIT_SUCCESS;
}

int compiler_driver_invoke(int argc, char** argv)
{
    CompilerDriver driver = (CompilerDriver)
    {
        .dm = diagnostic_manager_init(NULL),
        .options = (CompilerOptions) {0}
    };

    // First parse the compiler options from the inputted options
    if (!parse_compiler_options(&driver.options, &driver.dm, argc, argv))
    {
        return EXIT_FAILURE;
    }

    // Now we know we have good options we can go ahead and invoke the compiler
    // on the options.
    return compiler_driver_execute(&driver);
}
