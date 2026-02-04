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

// TODO: how to handle getting signed vs zero extension for these types
LLVMTypeRef llvm_get_type(CodegenContext* context, const QualifiedType* type)
{
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;

    // Before doing anything get the real base type for this
    QualifiedType real_type = qualified_type_get_canonical(type);
    TypeKind kind = qualified_type_get_kind(&real_type);
    switch (kind)
    {
        case TYPE_VOID:
            return LLVMVoidTypeInContext(c);

        case TYPE_BOOL:
            return LLVMInt1TypeInContext(c);

        case TYPE_CHAR:
        case TYPE_U_CHAR:
        case TYPE_S_CHAR:
            return LLVMInt8TypeInContext(c);

        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
            return LLVMInt16TypeInContext(c);

        case TYPE_S_INT:
        case TYPE_U_INT:
            return LLVMInt32TypeInContext(c);

        case TYPE_S_LONG:
        case TYPE_U_LONG:
        case TYPE_S_LONG_LONG:
        case TYPE_U_LONG_LONG:
            return LLVMInt64TypeInContext(c);

        case TYPE_FLOAT:
            return LLVMFloatTypeInContext(c);

        case TYPE_DOUBLE:
            return LLVMDoubleTypeInContext(c);

        case TYPE_LONG_DOUBLE:
            return LLVMFP128TypeInContext(c);

        case TYPE_IMAGINARY:
        case TYPE_COMPLEX:
            panic("imaginary and complex types are not implemented at all");
            return NULL;

        case TYPE_ARRAY:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ENUM:
        case TYPE_FUNCTION:
            panic("llvm_get_type unimplemented for this type");        
            return NULL;

        case TYPE_POINTER:
        {
            QualifiedType inner_type = type_pointer_get_pointee(&real_type);
            LLVMTypeRef inner_llvm = llvm_get_type(context, &inner_type);
            return LLVMPointerType(inner_llvm, 0);
        }

        case TYPE_TYPEDEF:
            panic("typedefs should have been removed before llvm_get_type");
            return NULL;

        case TYPE_ERROR:
            panic("attempting llvm_get_type on error type");
            return NULL;

        default:
            panic("unknown type in llvm_get_type");
            return NULL;
    }
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
    LLVMTypeRef ret = llvm_get_type(context, &return_type);

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

        QualifiedType parm_type = type_function_parameter_get_type(curr);
        parms[i] = llvm_get_type(context, &parm_type);  
    }
    assert(i == parm_count);

    // Also get if the type is variadic
    bool variadic = type_function_is_variadic(&type);

    // Then now we can finally create the function type
    return LLVMFunctionType(ret, parms, parm_count, variadic);
}
