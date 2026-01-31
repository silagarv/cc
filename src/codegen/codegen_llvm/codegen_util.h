#ifndef CODEGEN_UTIL_LLVM_H
#define CODEGEN_UTIL_LLVM_H

#include "parse/type.h"
#include "parse/declaration.h"

#include "codegen/codegen.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

LLVMTypeRef llvm_get_type(CodegenContext* context, const QualifiedType* type);

LLVMTypeRef llvm_create_function_type(CodegenContext* context,
        const Declaration* declaration);

#endif /* CODEGEN_UTIL_LLVM_H */
