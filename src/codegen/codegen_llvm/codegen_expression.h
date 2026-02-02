#ifndef CODEGEN_EXPRESSION_LLVM_H
#define CODEGEN_EXPRESSION_LLVM_H

#include "parse/expression.h"

#include "codegen/codegen.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

LLVMValueRef llvm_codegen_expression(CodegenContext* c, const Expression* e);
LLVMValueRef llvm_codegen_condition(CodegenContext* c, const Expression* e);

#endif /* CODEGEN_EXPRESSION_LLVM_H */
