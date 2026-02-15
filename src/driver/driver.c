#include "driver.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "util/panic.h"
#include "util/arena.h"

#include "driver/lang.h"
#include "files/filepath.h"

#include "driver/diagnostic.h"
#include "driver/options.h"
#include "driver/target.h"
#include "driver/translation_unit.h"

#include "files/source_manager.h"

#include "lex/preprocessor.h"

#include "parse/parser.h"
#include "parse/ast.h"

#include "codegen/codegen.h"

// TODO: get rid of below include
#include <llvm-c/Core.h>

// An enum of our compiler actions we can do. Noting that this may grow over 
// time.
typedef enum CompilerDriverAction {
    DRIVER_ACTION_HELP,
    DRIVER_ACTION_VERSION,
    DRIVER_ACTION_COMPILE
} CompilerDriverAction;

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

CompilerDriverAction compiler_driver_determine_action(const CompilerDriver* d)
{
    if (d->options.help)
    {
        return DRIVER_ACTION_HELP;
    }

    if (d->options.version)
    {
        return DRIVER_ACTION_VERSION;
    }

    return DRIVER_ACTION_COMPILE;
}

static void print_option(const char* flag, const char* desc)
{
    fprintf(stderr, "  %-25s\t%s\n", flag, desc);
}

static int compiler_driver_do_help(CompilerDriver* driver)
{
    // TODO: finish the help
    fprintf(stderr, "Usage: cc [options] file\n");
    fprintf(stderr, "Options:\n");
    print_option("--help", "Display this information.");
    print_option("-version", "Display version information.");
    print_option("-std=<value>", "Set the standard to compile for.");
    print_option("-O<opt-value>", "Set the preffered optimisation level.");
    print_option("-W<warning>", "Set the specified warning.");
    print_option("-w", "Silence all warnings.");
    print_option("-E", "Preprocess only; do not compile, assemble or link.");
    print_option("-S", "Compile only; do not assemble or link");
    print_option("-c", "Compile and assemble, but do not link");
    print_option("-o <file>", "Place the output into file");
    print_option("-fsyntax-only", "Only parse; do not compile or further");
    return EXIT_SUCCESS;
}

static int compiler_driver_do_version(CompilerDriver* driver)
{
    // TODO: finish the version section
    fprintf(stderr, "cc version 0.0.0\n");
    return EXIT_SUCCESS;
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
    CompilerOptions opts = driver->options;

    // Get the diagnostic manager from the driver so we can easily output any
    // error messages for this translation unit.
    DiagnosticManager* dm = &driver->dm;

    // Get input, output and the language standard all sorted before we properly
    // attempt to do any parsing of the translation unit as we might still have
    // to bail out at this point
    char* infile = opts.infile;
    Filepath input_path;
    if (!get_filepath(&input_path, infile, false, dm))
    {
        return EXIT_FAILURE;
    }

    char* outfile = opts.outfile;
    Filepath output_path;
    if (!get_filepath(&output_path, outfile, true, dm))
    {
        return EXIT_FAILURE;
    }

    // Okay now create our target (dummy for now)
    Target target = (Target) {0};

    // Now we can attempt to create our translation unit.
    translation_unit_create(tu, input_path, output_path, &driver->options,
            target, dm);

    int success = translation_unit_process(tu);

    translation_unit_delete(tu);

    return success;
}

static int compiler_driver_do_compile(CompilerDriver* driver)
{
    // First check that we actually have any input file. If we do not have any
    // files then we cannot proceed at all so simply abort compilation.
    if (driver->options.infile == NULL)
    {
        diagnostic_fatal_error(&driver->dm, "no input files");
        return EXIT_FAILURE;
    }

    // First try to parse the translation unit.
    TranslationUnit tu = {0};
    return compiler_driver_process_translation_unit(driver, &tu);
}

static int compiler_driver_execute(CompilerDriver* driver)
{
    // Determine what action to do based on our command line arguments and go
    // and then execute that action
    switch (compiler_driver_determine_action(driver))
    {
        case DRIVER_ACTION_HELP:
            return compiler_driver_do_help(driver);

        case DRIVER_ACTION_VERSION:
            return compiler_driver_do_version(driver);

        case DRIVER_ACTION_COMPILE:
            return compiler_driver_do_compile(driver);

        default:
            panic("invalid compiler action generated");
            return EXIT_FAILURE;
    }
}

static void compiler_driver_finish_init(CompilerDriver* driver)
{
    // Finish options set up here with our diagnostics engine if we got these
    diagnostic_manager_set_disable_warnings(&driver->dm, driver->options.w);
    diagnostic_manager_set_werror(&driver->dm, driver->options.werror);

    // TODO: set up backend specific stuff?
}

static int compiler_driver_invoke_internal(int argc, char** argv,
        CompilerDriver* driver)
{
    // First parse the compiler options from the inputted options
    if (!parse_compiler_options(&driver->options, &driver->dm, argc, argv))
    {
        return EXIT_FAILURE;
    }

    // Finish the initialisation of our driver and then go ahead and execute
    // the options.
    compiler_driver_finish_init(driver);

    // Now we know we have good options we can go ahead and invoke the compiler
    // on the options and return the result of doing that.
    return compiler_driver_execute(driver);
}

int compiler_driver_invoke(int argc, char** argv)
{
    // Now attempt to create and run our driver.
    CompilerDriver driver = compiler_driver_create();
    int status = compiler_driver_invoke_internal(argc, argv, &driver);
    compiler_driver_free(&driver);

    // TODO: get rid of this eventually, since this is mainly here for testing
    // purposes to ensure we aren't leaking memory elsewhere.
    LLVMShutdown();
    
    return status;
}
