#include "codegen_declaration.h"

#include <assert.h>

#include "parse/statement.h"

#include "codegen/codegen_statement.h"

void codegen_declaration(const Declaration* declaration)
{
    // TODO: generate IR for all of our declarations
}

void codegen_external_variable(const Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_VARIABLE));

    // printf("generating for variable: %s\n",
    //         identifier_cstr(declaration_get_identifier(declaration)));

    DeclarationLinkage linkage = declaration_variable_get_linkage(declaration);
    assert(linkage != DECLARATION_LINKAGE_NONE);

    // panic("variable codegen not supported");
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

