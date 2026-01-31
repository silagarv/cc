#include "translation_unit.h"

#include <stdlib.h>

#include "codegen/codegen.h"
#include "driver/diagnostic.h"

#include "driver/options.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"

#include "parse/ast.h"
#include "parse/parser.h"

bool translation_unit_create(TranslationUnit* tu, Filepath main_file,
        Filepath out_file, CompilerOptions* options, Target target,
        DiagnosticManager* dm)
{

    *tu = (TranslationUnit)
    {
        .main_file = main_file,
        .out_file = out_file,
        .options = options,
        .target = target,
        .dm = dm,
        .sm = source_manager(),
        .ids = identifier_table_create(),
        .ast = ast_create()
    };

    diagnostic_manager_set_sm(tu->dm, &tu->sm);

    return true;
}

void translation_unit_delete(TranslationUnit* tu)
{
    source_manager_delete(&tu->sm);
    identifier_table_delete(&tu->ids);
    ast_delete(&tu->ast);
}

static bool translation_unit_parse(TranslationUnit* tu)
{
    // Attempt to create the parser exiting early if we fail
    Parser parser;
    if (!parser_create_for_translation_unit(&parser, tu->dm, &tu->sm,
            tu->main_file, &tu->ids, &tu->ast))
    {
        return false;
    }

    // Now attempt to parse the translation unit
    parse_translation_unit(&parser);

    // Don't forget to delete the parser once we are done, since we don't need
    // it or the preprocessor for anything anymore.
    parser_delete(&parser);

    // Now we need to see if we can continue on after parsing the translation
    // unit and go to do codegen. If we had no erorrs during parsing or 
    // preprocessing then we are good, otherwise just stop here.
    return diagnostic_manager_get_error_count(tu->dm) == 0;
}

// TODO: can this even fail?
static bool translation_unit_codegen(TranslationUnit* tu)
{
    codegen_translation_unit(&tu->main_file, &tu->target, tu->dm, tu->options,
            &tu->ast);

    return true;
}

int translation_unit_process(TranslationUnit* tu)
{
    // First check if we are only meant to preprocess and handle that
    if (tu->options->preprocess_only)
    {
        diagnostic_error(tu->dm, "preprocessing is not yet supported; "
                "compilation aborted");
        return EXIT_FAILURE;
    }

    // Then try to parse the translation unit to see if we are even able to do
    // code generation.
    if (translation_unit_parse(tu) == false)
    {
        return EXIT_FAILURE;
    }

    // TODO: dump ast at this point? Yes, need to add flag for that.

    // Also check if we are only wanting to do syntax only and no codegeneration
    // but also make sure we exit with success.
    if (tu->options->syntax_only)
    {
        return EXIT_SUCCESS;
    }

    // Now we can attempt to do code generation with the translation unit
    if (translation_unit_codegen(tu) == false)
    {
        return EXIT_FAILURE;
    }

    // Now we need to see if the -S option was specified
    if (tu->options->dump_assembly)
    {
        // Dump the assembly we are about to generate
        return EXIT_SUCCESS;
    }

    // Okay now we have done the codegeneration we need to take the generated
    // llvm and transform it into an object file

    // Now check if we need to do any linking at all
    if (tu->options->compile_only)
    {
        return EXIT_SUCCESS;
    }

    // Okay, now we have done all the compiling, we just need to do the linking
    // TODO: how tf do we do the linking... but we will cross that bridge later

    return EXIT_SUCCESS;
}
