#include "codegen_statement.h"

#include <stdio.h>
#include <assert.h>

#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

void llvm_codegen_statement(CodegenContext* context, Statement* stmt);

void llvm_codegen_empty_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_EMPTY));

    // Nothing to do since this is an empty statement
    (void) context;
    (void) stmt;
}

void llvm_codegen_compound_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_COMPOUND));

    Statement* curr = statement_compound_get_first(stmt);
    for (; curr != NULL; curr = statement_get_next(curr))
    {
        llvm_codegen_statement(context, curr);
    }
}

void llvm_codegen_expression_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_EXPRESSION));

}

void llvm_codegen_if_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_IF));

    // Get our backend specific data before doing anything...
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;

    // Create our basic blocks before doing anything
    LLVMBasicBlockRef jump_true = NULL;
    LLVMBasicBlockRef jump_false = NULL;
    LLVMBasicBlockRef jump_end = NULL;

    // Now we need to get the true part and the false part.
    Statement* true_part = statement_if_get_true_part(stmt);
    Statement* false_part = statement_if_get_false_part(stmt);

    //  Note, that since the false part can be null we will need to more 
    // intelligently set our jump targets for after our if.
    // e.g.
    // if (cond)
    //     ... <- jump true
    // else
    //     ... <- jump false
    // ... <- jump end
    // ^^^^^^^^^
    // So if we have no jump false, we'll want to just jump to the end :)
    if (false_part == NULL)
    {
        jump_false = jump_end;
    }

    // TODO: codegen the condition and create a jump to the different blocks
    Expression* condition = statement_if_get_condition(stmt);
    
}

void llvm_codegen_switch_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));
    
}

void llvm_codegen_for_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));
    
}

void llvm_codegen_while_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_WHILE));
    
}

void llvm_codegen_do_while_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DO_WHILE));
    

}

void llvm_codegen_goto_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_GOTO));
    
    // TODO: will need to get the current target to goto to...
}

void llvm_codegen_continue_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CONTINUE));

    // TODO: will need to get the current target to continue to...    
}

void llvm_codegen_break_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_BREAK));
    
    // TODO: will need to get the current target to break to...
}

void llvm_codegen_return_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_RETURN));

    // See if we need to generate the expression for this statement
    Expression* expr_opt = statement_return_get_expression(stmt);
    if (expr_opt != NULL)
    {
        // TODO: generate the expression...
    }

    // TODO: finish generating the rest of the return for this basic block
}

void llvm_codegen_decl_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DECLARATION));
    
}

void llvm_codegen_label_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_LABEL));
}

void llvm_codegen_case_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CASE));
    
}

void llvm_codegen_default_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DEFAULT));
    
}

void llvm_codegen_statement(CodegenContext* context, Statement* stmt)
{
    switch (statement_get_kind(stmt))
    {
        case STATEMENT_EMPTY:
            llvm_codegen_empty_statement(context, stmt);
            return;

        case STATEMENT_COMPOUND:
            llvm_codegen_compound_statement(context, stmt);
            return;

        case STATEMENT_EXPRESSION:
            llvm_codegen_expression_statement(context, stmt);
            return;

        case STATEMENT_IF:
            llvm_codegen_if_statement(context, stmt);
            return;

        case STATEMENT_SWITCH:
            llvm_codegen_switch_statement(context, stmt);
            return;

        case STATEMENT_FOR:
            llvm_codegen_for_statement(context, stmt);
            return;

        case STATEMENT_WHILE:
            llvm_codegen_while_statement(context, stmt);
            return;

        case STATEMENT_DO_WHILE:
            llvm_codegen_do_while_statement(context, stmt);
            return;

        case STATEMENT_GOTO:
            llvm_codegen_goto_statement(context, stmt);
            return;

        case STATEMENT_CONTINUE:
            llvm_codegen_continue_statement(context, stmt);
            return;

        case STATEMENT_BREAK:
            llvm_codegen_break_statement(context, stmt);
            return;

        case STATEMENT_RETURN:
            llvm_codegen_return_statement(context, stmt);
            return;

        case STATEMENT_DECLARATION:
            llvm_codegen_decl_statement(context, stmt);
            return;

        case STATEMENT_LABEL:
            llvm_codegen_label_statement(context, stmt);
            return;

        case STATEMENT_CASE:
            llvm_codegen_case_statement(context, stmt);
            return;

        case STATEMENT_DEFAULT:
            llvm_codegen_default_statement(context, stmt);
            return;

        case STATEMENT_ERROR:
            panic("attmpeting to generate code for error statement");
            return;

        default:
            panic("attempting to generate code for bad statement type");
            return;
    }
}

void llvm_codegen_function_body(CodegenContext* context, Statement* body)
{
    assert(statement_is(body, STATEMENT_COMPOUND));

    printf("generating function body\n");
    llvm_codegen_compound_statement(context, body);
}
