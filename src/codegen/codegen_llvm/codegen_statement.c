#include "codegen_statement.h"

#include <stdio.h>
#include <assert.h>

#include "codegen/codegen_llvm/codegen_expression.h"
#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

void llvm_maybe_create_branch_to(LLVMBasicBlockRef src, LLVMBuilderRef builder,
        LLVMBasicBlockRef dest)
{
    if (LLVMGetBasicBlockTerminator(src) != NULL)
    {
        return;
    }

    LLVMPositionBuilderAtEnd(builder, src);
    LLVMBuildBr(builder, dest);
}

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
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Now we need to get the true part and the false part.
    Statement* true_part = statement_if_get_true_part(stmt);
    Statement* false_part = statement_if_get_false_part(stmt);

    // Gen if we are going to generate the true and false parts. Note: we can
    // safely assume that the true part will never be NULL
    bool gen_true_part = !statement_is_empty(true_part);
    bool gen_false_part = false_part != NULL && !statement_is_empty(false_part);

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

    // Create our basic blocks before doing anything and automatically set their
    // jump target to jump to the end in the event that we don't generate code
    // for them at all
    LLVMBasicBlockRef jump_end = LLVMCreateBasicBlockInContext(c, "");
    LLVMBasicBlockRef jump_true = jump_end;
    LLVMBasicBlockRef jump_false = jump_end;

    if (gen_true_part)
    {
        jump_true = LLVMCreateBasicBlockInContext(c, "");
    }

    if (gen_false_part)
    {
        jump_false = LLVMCreateBasicBlockInContext(c, "");
    }

    // Okay, now for each of our blocks, set it as the current block, and then
    // go and generate code for it. Do this for both the true and false blocks
    // if we need to generate code for them. Also ensure that when generating
    // code we make sure we end up branching to the block at the end if required
    if (gen_true_part)
    {
        llvm->basic_block = jump_true;
        llvm_codegen_statement(context, true_part);
        llvm_maybe_create_branch_to(jump_true, b, jump_end);
    }

    if (gen_false_part)
    {
        llvm->basic_block = jump_false;
        llvm_codegen_statement(context, false_part);
        llvm_maybe_create_branch_to(jump_false, b, jump_end);
    }

    // Finally, generate the code to evaluate the condition and 
    LLVMPositionBuilderAtEnd(b, current_bb);
    Expression* condition = statement_if_get_condition(stmt);
    LLVMValueRef llvm_cond = llvm_codegen_condition(context, condition);
    LLVMBuildCondBr(b, llvm_cond, jump_true, jump_false);

    // Finally, once everything is generated apped all of our basic blocks as
    // required
    if (gen_true_part)
    {
        LLVMAppendExistingBasicBlock(fn, jump_true);
    }
    
    if (gen_false_part)
    {
        LLVMAppendExistingBasicBlock(fn, jump_false);
    }

    // Append the jump block to the end of the function.
    LLVMAppendExistingBasicBlock(fn, jump_end);
    
    // Finally, set the current basic block as the jump end target
    llvm->basic_block = jump_end;
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
    
    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Make sure we put the return in the correct place
    LLVMPositionBuilderAtEnd(b, bb);

    // TODO: will need to get the current target to break to...
    LLVMBasicBlockRef jump_target = NULL;
    LLVMBuildBr(b, jump_target);

    // TODO: do we need to create another basic block or something???
}

void llvm_codegen_return_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_RETURN));

    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // Make sure we don't try to generate code for a basic block with a 
    // terminator already since this leads to invalid IR!!!
    assert(LLVMGetBasicBlockTerminator(bb) == NULL);

    // Make sure we put the return in the correct place
    LLVMPositionBuilderAtEnd(b, bb);

    // See if we need to generate the expression for this statement which will
    // require different code paths.
    Expression* expr_opt = statement_return_get_expression(stmt);
    if (expr_opt != NULL)
    {
        LLVMValueRef ret_val = llvm_codegen_expression(context, expr_opt);
        LLVMBuildRet(b, ret_val);
    }
    else
    {
        LLVMBuildRetVoid(b);
    }

    // Now, since a return statement must end a block we will need to append a
    // new block onto the end of the current one. But we must only do this if
    // the return statement has a statement after it
    // TODO: should we even bother appending a basic block after it since that
    // TODO: code would be unreachable anyways?
    Statement* next = statement_get_next(stmt);
    if (next != NULL && !statement_is_empty(next))
    {
        llvm->basic_block = LLVMAppendBasicBlockInContext(c, fn, "");
    }
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

    // Append a new basic block to the end of the function so that we have a 
    // block to add all of our generated code to
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;

    // Create the basic block and add it and the end of the function
    llvm->basic_block = LLVMAppendBasicBlockInContext(c, fn, "");
    llvm_codegen_compound_statement(context, body);

    // Finally get the last block from the function and generate an implicit 
    // return if the last block does not have a return statement
}
