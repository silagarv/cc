#include "codegen.h"

#include <assert.h>

#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/statement.h"

#include "codegen/codegen_statement.h"
#include "codegen/codegen_declaration.h"

void codegen_translation_unit(const Filepath* input_file, const Target* target,
        DiagnosticManager* dm, const Ast* ast)
{
    // In order to generate any code we will need to traverse all declarations
    // that could be used for such. If it is an external decl it is a top level
    // declaration that is not for example a struct, typedef, etc...
    DeclarationList external_decls = ast_get_external_decls(ast);
    
    DeclarationListEntry* entry = declaration_list_iter(&external_decls);
    for (; entry != NULL; entry = declaration_list_next(entry))
    {
        // Get and generate code for the external declaration.
        Declaration* decl = declaration_list_entry_get(entry);
        codegen_external_declaration(decl);
    }
}

