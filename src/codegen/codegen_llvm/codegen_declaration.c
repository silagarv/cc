#include "codegen_declaration.h"

#include "codegen/codegen_llvm/codegen_llvm.h"
#include "codegen/codegen_llvm/codegen_util.h"
#include "lex/identifier_table.h"

#include "parse/declaration.h"
#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_statement.h"
#include <llvm-c-18/llvm-c/Core.h>
#include <llvm-c-18/llvm-c/Types.h>

void llvm_codegen_declaration(CodegenContext* context,
        const Declaration* declaration)
{

}

static void llvm_codegen_external_function(CodegenContext* context,
        const Declaration* declaration)
{
    // TODO: static functions?

    // First get the llvm codegen from the context
    CodegenLLVM* llvm = context->backend_specific;
    LLVMModuleRef module = llvm->module;

    // First get the name of the function that we are wanting to codegen for
    Identifier* id = declaration_get_identifier(declaration);
    const char* name = identifier_cstr(id);

    printf("function: %s\n", name);

    // First create the function type fro the function declaration.
    LLVMTypeRef fn_ty = llvm_create_function_type(context, declaration);
    LLVMValueRef fn = LLVMAddFunction(module, name, fn_ty);

    // Now we have created the function type set the be specific context
    llvm->function = fn;

    // Yay, now we have done all of the start stuff we can begin generating a
    // body for this function.
    if (declaration_function_has_body(declaration))
    {
        Statement* body = declaration_function_get_body(declaration);
        llvm_codegen_function_body(context, body);
    }

    printf("\n");
}

static void llvm_codegen_external_variable(CodegenContext* context,
        const Declaration* declaration)
{
    Identifier* identifier = declaration_get_identifier(declaration);
    printf("variable: %s\n", identifier_cstr(identifier));
    printf("don't know how to gen variables yet!\n");
    printf("\n");
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
