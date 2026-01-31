#include "codegen_declaration.h"

#include "lex/identifier_table.h"
#include "parse/declaration.h"
#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_statement.h"

void llvm_codegen_declaration(CodegenContext* context,
        const Declaration* declaration)
{

}

static void llvm_codegen_external_function(CodegenContext* context,
        const Declaration* declaration)
{
    Identifier* identifier = declaration_get_identifier(declaration);
    printf("function: %s\n", identifier_cstr(identifier));

    if (declaration_function_has_body(declaration))
    {
        Statement* body = declaration_function_get_body(declaration);
        llvm_codegen_function_body(context, body);
    }
}

static void llvm_codegen_external_variable(CodegenContext* context,
        const Declaration* declaration)
{
    Identifier* identifier = declaration_get_identifier(declaration);
    printf("variable: %s\n", identifier_cstr(identifier));
    printf("don't know how to gen variables yet!\n");
}

void llvm_codegen_external_declaration(CodegenContext* context,
        const Declaration* declaration)
{
    // Make sure to set the current external declaration
    context->current_external = declaration;

    // Now determine the type of external declaration we need to gen code for
    switch (declaration_get_kind(declaration))
    {
        case DECLARATION_FUNCTION:
            llvm_codegen_external_function(context, declaration);
            return;

        case DECLARATION_VARIABLE:
            llvm_codegen_external_variable(context, declaration);
            return;
        
        default:
            panic("bad external declaration kind");
            return;
    }
}
