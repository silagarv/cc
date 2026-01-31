#ifndef TRANSLATION_UNIT_H
#define TRANSLATION_UNIT_H

#include "driver/options.h"
#include "driver/target.h"
#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"

#include "parse/ast.h"

#include "codegen/codegen.h"

typedef struct TranslationUnit {
    // The path of the main file that we are wanting to compiler
    Filepath main_file;

    // The path of the filename that we would like to output to
    Filepath out_file;

    // The options for the compilation of this translation unit
    CompilerOptions* options;

    // Our target information so that we can initialize and parse correctly in
    // both the frontend and back end
    Target target;

    // The DiagnosticManger that the compiler driver is using
    DiagnosticManager* dm;

    // The source manager for this translation unit.
    SourceManager sm;

    // All the identifiers that we have in this translation unit. Note this is
    // not kept in the parser or preprocessor since we need it to live longer
    // than them since we would like to be able to access the names of decl's
    // during code generation.
    IdentifierTable ids;

    // The ast that we have for this translation unit
    Ast ast;

    // The code generation result that we have gotten.
    CodegenResult* result;

    // Other required information
} TranslationUnit;

bool translation_unit_create(TranslationUnit* tu, Filepath main_file,
        Filepath out_file, CompilerOptions* options, Target target,
        DiagnosticManager* dm);
void translation_unit_delete(TranslationUnit* tu);

int translation_unit_process(TranslationUnit* tu);

#endif /* TRANSLATION_UNIT_H */
