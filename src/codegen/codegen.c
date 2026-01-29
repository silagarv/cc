#include "codegen.h"

#include <assert.h>

#include "codegen/codegen_statement.h"
#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/statement.h"

void codegen_external_variable(const Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_VARIABLE));

    printf("generating for variable: %s\n",
            identifier_cstr(declaration_get_identifier(declaration)));

    DeclarationLinkage linkage = declaration_variable_get_linkage(declaration);
    assert(linkage != DECLARATION_LINKAGE_NONE);

    panic("variable codegen not supported");
}

void codegen_external_function(const Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_FUNCTION));

    printf("generating for function: %s\n",
            identifier_cstr(declaration_get_identifier(declaration)));

    DeclarationLinkage linkage = declaration_function_get_linkage(declaration);
    assert(linkage != DECLARATION_LINKAGE_NONE);

    // TODO: generate the function start

    if (declaration_function_has_body(declaration))
    {
        Statement* body = declaration_function_get_body(declaration);
        codegen_function_body(body);
    }
}

void codegen_external_declaration(const Declaration* declaration)
{
    switch (declaration_get_kind(declaration))
    {
        case DECLARATION_VARIABLE:
            codegen_external_variable(declaration);
            break;

        case DECLARATION_FUNCTION:
            codegen_external_function(declaration);
            break;

        default:
            panic("cannot generate code for this type of external decl!");
            break;
    }
}

void codegen_translation_unit(const Ast* ast)
{
    // In order to generate any code we will need to traverse all declarations
    // that could be used for such. If it is an external decl it is a top level
    // declaration that is not for example a struct, typedef, etc...
    DeclarationList external_decls = ast_get_external_decls(ast);
    DeclarationListEntry* entry = declaration_list_iter(&external_decls);
    while (entry != NULL)
    {
        // Get and generate code for the external declaration.
        Declaration* decl = declaration_list_entry_get(entry);
        codegen_external_declaration(decl);

        // Move on to the next declaration.
        entry = declaration_list_next(entry);
    }
}

