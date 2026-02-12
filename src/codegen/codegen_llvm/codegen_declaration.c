#include "codegen_declaration.h"

#include <stddef.h>
#include <assert.h>

#include "driver/diagnostic.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/declaration.h"
#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"
#include "codegen/codegen_llvm/codegen_util.h"
#include "codegen/codegen_llvm/codegen_statement.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

// should only be used for local declarations
LLVMValueRef llvm_codegen_variable_declaration(CodegenContext* context,
        const Declaration* variable)
{
    assert(declaration_is(variable, DECLARATION_VARIABLE));
    assert(!declaration_variable_has_linkage(variable));

    // Specific code for static locals might be needed
    if (declaration_get_storage_class(variable) == STORAGE_STATIC)
    {
        panic("unable to handle static locals yet!");
    }

    // First get the llvm codegen from the context
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef bb = llvm->basic_block;
    
    // Now get all of the information we need to create the alloca
    QualifiedType type = declaration_get_type(variable);
    LLVMTypeRef llvm_ty = llvm_get_type(context, &type);

    // Now create the alloca
    LLVMPositionBuilderAtEnd(b, bb);
    LLVMValueRef var_alloca = LLVMBuildAlloca(b, llvm_ty, "");

    // TODO: here we should build the initializer as well! but that will not 
    // be done
    if (declaration_variable_has_initializer(variable))
    {
        Identifier* id = declaration_get_identifier(variable);
        diagnostic_warning_at(context->dm, declaration_get_location(variable),
                "initializer cannot be created for '%s'; this is not "
                "implemented", identifier_cstr(id));
    }

    // Finally, once everything is build we need to insert this into our 
    // declaration to value ref map so that we are able to remember this for
    // later and reference it :)
    llvm_codegen_add_declaration(context, variable, var_alloca);

    return var_alloca;
}

LLVMValueRef llvm_codegen_declaration(CodegenContext* context,
        const Declaration* declaration)
{
    switch (declaration_get_kind(declaration))
    {
        case DECLARATION_VARIABLE:
            return llvm_codegen_variable_declaration(context, declaration);

        default:
            panic("unable to codegen this declaration type yet!");
            return NULL;
    }
}

static LLVMLinkage translate_linkage_to_llvm(DeclarationLinkage linkage)
{
    switch (linkage)
    {
        case DECLARATION_LINKAGE_EXTERNAL:
            return LLVMExternalLinkage;

        case DECLARATION_LINKAGE_INTERNAL:
            return LLVMInternalLinkage;

        case DECLARATION_LINKAGE_NONE:
        default:
            panic("should not try to get linkage when there is none!");
            return LLVMInternalLinkage; /* Dummy answer */

    }
}

static void llvm_codegen_external_function(CodegenContext* context,
        const Declaration* declaration)
{
    // Don't generate code for a function that doesn't have a definition
    if (!declaration_function_has_definition(declaration))
    {
        return;
    }

    // First get the llvm codegen from the context
    CodegenLLVM* llvm = context->backend_specific;
    LLVMModuleRef module = llvm->module;
    LLVMContextRef c = llvm->context;

    // First get the name of the function that we are wanting to codegen for
    Identifier* id = declaration_get_identifier(declaration);
    const char* name = identifier_cstr(id);

    // First create the function type fro the function declaration.
    LLVMTypeRef fn_ty = llvm_create_function_type(context, declaration);
    LLVMValueRef fn = LLVMAddFunction(module, name, fn_ty);

    // Once we have created the function don't forget to set it's linkage
    DeclarationLinkage linkage = declaration_function_get_linkage(declaration);
    LLVMSetLinkage(fn, translate_linkage_to_llvm(linkage));

    // Also set the inline status if we had it by getting the 'inlinehint' 
    // attribute and adding it to the function.
    if (declaration_function_is_inline(declaration))
    {
        const char* const inline_attr_name = "inlinehint";
        unsigned inline_id = LLVMGetEnumAttributeKindForName(inline_attr_name,
                strlen(inline_attr_name));
        LLVMAttributeRef attr = LLVMCreateEnumAttribute(c, inline_id, 0);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex, attr);
    }

    // TODO: at this point should we add the funciton to our list of decls???
    
    // Now we have created the function type set the be specific context
    llvm->function_type = fn_ty;
    llvm->function = fn;

    // Yay, now we have done all of the start stuff we can begin generating a
    // body for this function.
    Declaration* defn = declaration_function_get_definition(declaration);
    Statement* body = declaration_function_get_body(defn);
    
    llvm_codegen_function_body(context, body);
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
