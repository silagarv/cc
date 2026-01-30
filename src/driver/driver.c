#include "driver.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "driver/lang.h"
#include "files/filepath.h"
#include "util/arena.h"

#include "driver/diagnostic.h"
#include "driver/options.h"
#include "driver/target.h"
#include "driver/translation_unit.h"

#include "files/source_manager.h"

#include "lex/preprocessor.h"

#include "parse/parser.h"
#include "parse/ast.h"

#include "codegen/codegen.h"

typedef struct CompilerDriver {
    // The diagnostic manager for this driver
    DiagnosticManager dm;

    // The options we are using to compiler
    CompilerOptions options;

    // The arena that this driver will use to process the translation unit
    Arena arena;
} CompilerDriver;

CompilerDriver compiler_driver_create(void)
{
    CompilerDriver driver = (CompilerDriver)
    {
        .dm = diagnostic_manager_init(NULL),
        .options = (CompilerOptions) {0},
        .arena = arena_new(ARENA_DEFAULT_CHUNK_SIZE, ARENA_DEFAULT_ALIGNMENT)
    };

    return driver;
}

void compiler_driver_free(CompilerDriver* driver)
{
    arena_delete(&driver->arena);
}

TranslationUnit* compiler_driver_tu_allocate(CompilerDriver* driver)
{
    return arena_allocate_size(&driver->arena, sizeof(TranslationUnit));
}

static bool get_filepath(Filepath* path, const char* name, bool output,
        DiagnosticManager* dm)
{
    if (name == NULL && output == true)
    {
        name = "a.out";
    }

    assert(name);

    size_t len = strlen(name);
    if (len >= FILENAME_MAX)
    {
        diagnostic_error(dm, "file name too long '%s'", name);
        return false;
    }

    strcpy(path->path, name);
    path->len = len;

    return true;
}

static int compiler_driver_process_translation_unit(CompilerDriver* driver,
        TranslationUnit* tu)
{
    // Get the diagnostic manager from the driver so we can easily output any
    // error messages for this translation unit.
    DiagnosticManager* dm = &driver->dm;

    // Get input, output and the language standard all sorted before we properly
    // attempt to do any parsing of the translation unit as we might still have
    // to bail out at this point
    char* infile = driver->options.infile;
    Filepath input_path;
    if (!get_filepath(&input_path, infile, false, dm))
    {
        return EXIT_FAILURE;
    }

    char* outfile = driver->options.outfile;
    Filepath output_path;
    if (!get_filepath(&output_path, outfile, true, dm))
    {
        return EXIT_FAILURE;
    }

    LangStandard std = driver->options.standard;

    // Okay now create our target (dummy for now)
    Target t = (Target) {0};

    // Now we can attempt to create our translation unit.
    translation_unit_create(tu, input_path, output_path, std, t, dm);

    int success = translation_unit_process(tu);

    translation_unit_delete(tu);

    return success;
}
    
//     // Get our filepath and attempt to create the file here.
//     Filepath path;
//     get_filepath(&path, driver->options.infile);
//     SourceFile* source = source_manager_create_filepath(&sm, path);
//     if (!source)
//     {
//         diagnostic_error(&driver->dm, "no such file or directory '%s'",
//                 path.path);
//         okay = false;
//         goto no_file;
//     }

//     // Now attempt to parse the translation unit here.
//     Preprocessor pp;
//     preprocessor_create(&pp, &driver->dm, &sm, source);

//     // Parse the translation unit
//     Ast ast = parse_translation_unit(&driver->dm, &pp);

//     // Finally, check if there were any errors during parsing
//     if (diagnostic_manager_get_error_count(&driver->dm) != 0)
//     {
//         okay = false;
//         goto no_codegen;
//     }

//     // Now try to do codegen for the AST...
//     codegen_translation_unit(&ast);

//     // And make sure to free the ast and the preprocessor
// no_codegen:
//     ast_delete(&ast);
//     preprocessor_delete(&pp);

// no_file:
//     source_manager_delete(&sm);

//     return okay;

static int compiler_driver_execute(CompilerDriver* driver)
{
    // First try to parse the translation unit.
    TranslationUnit tu = {0};
    if (compiler_driver_process_translation_unit(driver, &tu) == false)
    {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static int compiler_driver_invoke_internal(int argc, char** argv,
        CompilerDriver* driver)
{
    // First parse the compiler options from the inputted options
    if (!parse_compiler_options(&driver->options, &driver->dm, argc, argv))
    {
        return EXIT_FAILURE;
    }

    // Now we know we have good options we can go ahead and invoke the compiler
    // on the options.
    return compiler_driver_execute(driver);
}

int compiler_driver_invoke(int argc, char** argv)
{
    CompilerDriver driver = compiler_driver_create();
    int status = compiler_driver_invoke_internal(argc, argv, &driver);
    compiler_driver_free(&driver);
    return status;
}
