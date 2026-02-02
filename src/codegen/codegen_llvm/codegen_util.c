#include "codegen_util.h"

#include <stddef.h>
#include <assert.h>

#include "util/arena.h"

#include "parse/declaration.h"
#include "parse/type.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <llvm-c/DataTypes.h>

LLVMTypeRef llvm_get_type(CodegenContext* context, const QualifiedType* type)
{
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;

    // Before doing anything get the real base type for this
    QualifiedType real_type = qualified_type_get_canonical(type);

    if (!qualified_type_is(&real_type, TYPE_S_INT))
    {
        panic("can only use int32's for now!!!");
    }

    // Simply get the int32 type for now
    return LLVMInt32TypeInContext(c);
}

LLVMTypeRef llvm_create_function_type(CodegenContext* context,
        const Declaration* declaration)
{
    // First get the usual types in context and all that
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;

    QualifiedType type = declaration_get_type(declaration);
    assert(qualified_type_is(&type, TYPE_FUNCTION));

    // Also get the return type of the function so that we can use that as the
    // return.
    QualifiedType return_type = type_function_get_return(&type);
    LLVMTypeRef ret = LLVMInt32TypeInContext(c);

    // Get the parameters and add them all into a function so that we can use
    // it to create the function type
    size_t parm_count = type_function_get_param_count(&type);
    LLVMTypeRef* parms = arena_allocate_size(&context->arena,
            sizeof(LLVMTypeRef) * parm_count);
    
    TypeFunctionParameter* curr = type_function_get_params(&type);
    size_t i = 0;
    for (; curr != NULL; curr = type_function_parameter_get_next(curr), i++)
    {
        assert(i < parm_count);

        parms[i] = LLVMInt32TypeInContext(c);  
    }
    assert(i == parm_count);

    // Also get if the type is variadic
    bool variadic = type_function_is_variadic(&type);

    return LLVMFunctionType(ret, parms, parm_count, variadic);
}
