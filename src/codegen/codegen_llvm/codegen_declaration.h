#ifndef CODEGEN_DECLARATION_LLVM_H
#define CODEGEN_DECLARATION_LLVM_H

#include "parse/declaration.h"

#include "codegen/codegen.h"

#include <llvm-c/Types.h>

LLVMValueRef llvm_codegen_declaration(CodegenContext* context,
        const Declaration* declaration);
void llvm_codegen_external_declaration(CodegenContext* context,
        const Declaration* declaration);

#endif /* CODEGEN_DECLARATION_LLVM_H */
