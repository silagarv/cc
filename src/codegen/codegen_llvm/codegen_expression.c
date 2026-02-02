#include "codegen_expression.h"

#include <assert.h>

#include "parse/literal_parser.h"
#include "parse/type.h"
#include "util/panic.h"

#include "parse/expression.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

static LLVMValueRef llvm_codegen_integer_constant(CodegenContext* ctx,
        const Expression* expression)
{
    assert(expression_is(expression, EXPRESSION_INTEGER_CONSTANT));

    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;

    // Get the expression type and thus the llvm type for the integer constant
    QualifiedType expr_type = expression_get_qualified_type(expression);
    // TODO: get the type properly
    LLVMTypeRef llvm_ty = LLVMInt32TypeInContext(c);

    // Then get the actual integer value itself and then generate the constant
    IntegerValue val = expression_integer_get_value(expression);
    unsigned long long n = integer_value_get_value(&val);

    return LLVMConstInt(llvm_ty, n, false);    
}

LLVMValueRef llvm_codegen_expression(CodegenContext* c, const Expression* e)
{
    assert(e != NULL);

    switch (expression_get_kind(e))
    {
        case EXPRESSION_INTEGER_CONSTANT:
            return llvm_codegen_integer_constant(c, e);

        default:
            panic("bad expression or don't know how to gen for it");
            return NULL;
    }
}

LLVMValueRef llvm_codegen_condition(CodegenContext* ctx, const Expression* e)
{
    CodegenLLVM* llvm = ctx->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;

    // Make sure to generate the code for the expression
    LLVMValueRef llvm_expr = llvm_codegen_expression(ctx, e);

    // Then get the type of the expression we generated and compare it with 0
    LLVMTypeRef llvm_type = LLVMTypeOf(llvm_expr);
    LLVMValueRef llvm_zero = LLVMConstInt(llvm_type, 0, false);

    // Finally build the comparison so that we are able to properly compare the
    // result.
    return LLVMBuildICmp(b, LLVMIntNE, llvm_expr, llvm_zero, "");
}
