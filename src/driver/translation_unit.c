#include "translation_unit.h"

#include <stdlib.h>

#include "driver/diagnostic.h"

#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "parse/ast.h"
#include "parse/parser.h"

bool translation_unit_create(TranslationUnit* tu, Filepath main_file,
        Filepath out_file, LangStandard std, Target target,
        DiagnosticManager* dm)
{

    *tu = (TranslationUnit)
    {
        .main_file = main_file,
        .out_file = out_file,
        .std = std,
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
    if (diagnostic_manager_get_error_count(tu->dm) != 0)
    {
        return false;
    }

    return true;
}

int translation_unit_process(TranslationUnit* tu)
{
    // First try to parse the translation unit returning failure if that doesn't
    // succeed.
    if (!translation_unit_parse(tu))
    {
        return EXIT_FAILURE;
    }

    // Now we can attempt to do code generation with the translation unit

    return EXIT_SUCCESS;
}
