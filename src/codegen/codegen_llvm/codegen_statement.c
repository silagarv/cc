#include "codegen_statement.h"

#include <stdio.h>
#include <assert.h>

#include "parse/statement.h"

void llvm_codegen_function_body(CodegenContext* context, Statement* body)
{
    assert(statement_is(body, STATEMENT_COMPOUND));

    printf("generating function body\n");
}
