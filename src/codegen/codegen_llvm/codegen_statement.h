#ifndef CODEGEN_STATEMENT_LLVM_H
#define CODEGEN_STATEMENT_LLVM_H

#include "parse/statement.h"

#include "codegen/codegen.h"

void llvm_codegen_function_body(CodegenContext* context, Statement* body);

#endif /* CODEGEN_STATEMENT_LLVM_H */
