#include "codegen_statement.h"

#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#include "parse/declaration.h"
#include "parse/expression.h"
#include "parse/expression_eval.h"
#include "parse/statement.h"

#include "codegen/codegen.h"
#include "codegen/codegen_llvm/codegen_llvm.h"
#include "codegen/codegen_llvm/codegen_declaration.h"
#include "codegen/codegen_llvm/codegen_expression.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>
#include <string.h>

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

    Expression* expr = statement_expression_get(stmt);
    LLVMValueRef gen_expr = llvm_codegen_expression(context, expr);
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
    LLVMBasicBlockRef jump_true = LLVMCreateBasicBlockInContext(c, "");
    LLVMBasicBlockRef jump_false = jump_end;
    if (false_part != NULL)
    {
        jump_false = LLVMCreateBasicBlockInContext(c, "");
    }

    // Emit the condition to the current basic block
    llvm->basic_block = current_bb;
    Expression* condition = statement_if_get_condition(stmt);
    LLVMValueRef llvm_cond = llvm_codegen_condition(context, condition);
    LLVMPositionBuilderAtEnd(b, current_bb);
    LLVMBuildCondBr(b, llvm_cond, jump_true, jump_false);

    // Okay, now for each of our blocks, set it as the current block, and then
    // go and generate code for it. Do this for both the true and false blocks
    // if we need to generate code for them. Also ensure that when generating
    // code we make sure we end up branching to the block at the end if required
    LLVMAppendExistingBasicBlock(fn, jump_true);

    llvm->basic_block = jump_true;
    llvm_codegen_statement(context, true_part);
    llvm_maybe_create_branch_to(llvm->basic_block, b, jump_end);

    if (false_part != NULL)
    {
        LLVMAppendExistingBasicBlock(fn, jump_false);

        llvm->basic_block = jump_false;
        llvm_codegen_statement(context, false_part);
        llvm_maybe_create_branch_to(llvm->basic_block, b, jump_end);
    }
    
    // Generate the code to evaluate the condition since we have now created
    // our jump targets an dknow where we are needing to jump to
    LLVMAppendExistingBasicBlock(fn, jump_end);
    
    // If the current block we ended on doesn't branch to anything make it go to
    // the ending block.
    llvm_maybe_create_branch_to(llvm->basic_block, b, jump_end);

    // Finally, set the current basic block as the jump end target so that we
    // can conitnue to add blocks onto the end after this if statement.
    llvm->basic_block = jump_end;
}

void llvm_codegen_switch_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));
    
    // Get our backend specific data before doing anything...
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get all of the information we need from the switch statement
    Expression* cond = statement_switch_get_condition(stmt);
    Statement* body = statement_switch_get_body(stmt);    
    Statement* case_start = statement_switch_get_cases(stmt);
    Statement* case_default = statement_switch_get_default(stmt);

    // First count all of the cases in the switch since we will need that in
    // order to build a switch with llvm
    Statement* curr_case = case_start;
    unsigned int num_cases = 0;
    for (; curr_case != NULL; curr_case = statement_case_get_next(curr_case))
    {
        num_cases++;
    }

    // Create the jump else block / jump default and also create the block that
    // we exit the switch to.
    LLVMBasicBlockRef llvm_default = NULL;
    if (case_default != NULL)
    {
        llvm_default = LLVMCreateBasicBlockInContext(c, "");
    }
    LLVMBasicBlockRef llvm_exit = LLVMCreateBasicBlockInContext(c, "");

    // Generate code for the current expression then build the llvm siwtch
    LLVMValueRef llvm_cond = llvm_codegen_expression(context, cond);

    // Determine what block we should jump to in the event that we don't match
    // any of the cases. If we have a defualt then we will jump to that,
    // otherwise we should just to after the swtich.
    LLVMBasicBlockRef llvm_else = llvm_exit;
    if (case_default != NULL)
    {
        llvm_else = llvm_default;
    }

    // Now create the switch with our parameters
    LLVMPositionBuilderAtEnd(b, current_bb);
    LLVMValueRef llvm_switch = LLVMBuildSwitch(b, llvm_cond, llvm_else,
            num_cases);

    // Now we have created a statement terminator we want to create add a new
    // basic block onto the end of this switch statement. So append a new basic
    // block, create a jump and then set the current basic block. This helps 
    // allow dead code like
    //
    // switch (...) {
    //     int a; <- this would be in the previous basic block otherwise    
    //     case 0:
    //         ...
    // }
    LLVMBasicBlockRef next_bb = LLVMAppendBasicBlockInContext(c, fn, "");
    llvm->basic_block = next_bb;

    // Now push the fact the we have a new break target onto that stack
    llvm_codegen_push_break(context, llvm_exit, llvm_switch, llvm_default);

    // Codegen the switch body.
    llvm_codegen_statement(context, body);

    // Finally, before the finaly thing to do we should check that the current
    // basic block terminated and act accordingly
    if (LLVMGetBasicBlockTerminator(llvm->basic_block) == NULL)
    {
        LLVMPositionBuilderAtEnd(b, llvm->basic_block);
        LLVMBuildBr(b, llvm_exit);
    }
        
    llvm_codegen_pop_jumps(context);

    LLVMAppendExistingBasicBlock(fn, llvm_exit);
    llvm->basic_block = llvm_exit;
}

void llvm_codegen_for_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));

    // Get our backend specific data before doing anything...
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get everything that we need from the statement
    Statement* init = statement_for_get_init(stmt);
    Expression* cond = statement_for_get_condition(stmt);
    Expression* inc = statement_for_get_inc(stmt);
    Statement* body = statement_for_get_body(stmt);

    // First gen the init if we have it
    if (init != NULL)
    {
        llvm_codegen_statement(context, init);
    }

    // Create jump back to the start of this current block and then break will 
    // jump to the end of the loop.
    LLVMBasicBlockRef jump_condition = LLVMAppendBasicBlockInContext(c, fn, "");
    LLVMBasicBlockRef loop_body = LLVMAppendBasicBlockInContext(c, fn, "");
    LLVMBasicBlockRef jump_increment = LLVMCreateBasicBlockInContext(c, "");
    LLVMBasicBlockRef jump_break = LLVMCreateBasicBlockInContext(c, "");

    // Now we want to push our jump targets since we have them
    llvm_codegen_push_break_continue(context, jump_break, jump_increment);

    // End the current block and create an unconditional branch to the 
    // continue target.
    LLVMPositionBuilderAtEnd(b, current_bb);
    LLVMBuildBr(b, jump_condition);

    // Since we are in the continue target the main purpose of this block is to
    // decide do we execute the loop body or skip it. So gen the condition for
    // this and then conditionallly branch. If we don't have a condition we want
    // to generate an unconditional branch to the body
    if (cond != NULL)
    {
        llvm->basic_block = jump_condition;
        LLVMValueRef llvm_cond = llvm_codegen_condition(context, cond);
        LLVMPositionBuilderAtEnd(b, jump_condition);
        LLVMBuildCondBr(b, llvm_cond, loop_body, jump_break);
    }
    else
    {
        LLVMPositionBuilderAtEnd(b, jump_condition);
        LLVMBuildBr(b, loop_body);
    }

    // Now we want to codegen the body and then also codegen the incrementing
    // if it exists. After the incrementing we take an unconditional branch to
    // the continue section.
    llvm->basic_block = loop_body;
    llvm_codegen_statement(context, body);
    llvm_maybe_create_branch_to(llvm->basic_block, b, jump_increment);

    LLVMAppendExistingBasicBlock(fn, jump_increment);
    if (inc != NULL)
    {
        llvm->basic_block = jump_increment;
        llvm_codegen_expression(context, inc);
    }
    llvm_maybe_create_branch_to(jump_increment, b, jump_condition);

    // Finally we want to add our jump break right to the end after we have done
    // codegeneration for all of our existing blocks
    llvm->basic_block = jump_break;
    LLVMAppendExistingBasicBlock(fn, jump_break);

    // Finally, pop our jump targets since we have them
    llvm_codegen_pop_jumps(context);
}

void llvm_codegen_while_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_WHILE));
    
    // Get our backend specific data before doing anything...
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get the information we need from our while statement
    Expression* cond = statement_while_get_condition(stmt);
    Statement* body = statement_while_get_body(stmt);

    // First create our jump targets for the while condition. Jump continue will
    // jump back to the start of this current block and then break will jump to
    // the end of the loop.
    LLVMBasicBlockRef jump_continue = LLVMAppendBasicBlockInContext(c, fn, "");
    LLVMBasicBlockRef loop_body = LLVMAppendBasicBlockInContext(c, fn, "");
    LLVMBasicBlockRef jump_break = LLVMCreateBasicBlockInContext(c, "");

    // Now we want to push our jump targets since we have them
    llvm_codegen_push_break_continue(context, jump_break, jump_continue);
    
    // First end the current block and create an unconditional branch to the 
    // continue target.
    LLVMPositionBuilderAtEnd(b, current_bb);
    LLVMBuildBr(b, jump_continue);
    
    // Since we are in the continue target the main purpose of this block is to
    // decide do we execute the loop body or skip it. So gen the condition for
    // this and then conditionallly branch
    llvm->basic_block = jump_continue;
    LLVMValueRef llvm_cond = llvm_codegen_condition(context, cond);
    LLVMPositionBuilderAtEnd(b, jump_continue);
    LLVMBuildCondBr(b, llvm_cond, loop_body, jump_break);

    // Now we want to codegen the body and create a branch back to the continue
    llvm->basic_block = loop_body;
    llvm_codegen_statement(context, body);
    llvm_maybe_create_branch_to(llvm->basic_block, b, jump_continue);

    // Finally we want to add our jump break right to the end after we have done
    // codegeneration for all of our existing blocks
    llvm->basic_block = jump_break;
    LLVMAppendExistingBasicBlock(fn, jump_break);

    // Finally, pop our jump targets since we have them
    llvm_codegen_pop_jumps(context);
}

void llvm_codegen_do_while_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DO_WHILE));
    
    // Get our backend specific data before doing anything...
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMValueRef fn = llvm->function;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get the information we need from our do-while statement
    Expression* cond = statement_do_while_get_condition(stmt);
    Statement* body = statement_do_while_get_body(stmt);

    // First create our jump targets for the while condition. Jump continue will
    // jump back to the start of this current block and then break will jump to
    // the end of the loop.
    LLVMBasicBlockRef loop_body = LLVMAppendBasicBlockInContext(c, fn, "");
    LLVMBasicBlockRef jump_continue = LLVMCreateBasicBlockInContext(c, "");
    LLVMBasicBlockRef jump_break = LLVMCreateBasicBlockInContext(c, "");

    // Now we want to push our jump targets since we have them
    llvm_codegen_push_break_continue(context, jump_break, jump_continue);

    // First end the current block and create an unconditional branch to the 
    // loop body
    LLVMPositionBuilderAtEnd(b, current_bb);
    LLVMBuildBr(b, loop_body);

    // Now we want to do all of the codegeneration for the loop body
    llvm->basic_block = loop_body;
    llvm_codegen_statement(context, body);
    llvm_maybe_create_branch_to(llvm->basic_block, b, jump_continue);

    // Now we want to do the code generation for the condition. Don't forget to
    // append the block since we can now know it's location in the function
    LLVMAppendExistingBasicBlock(fn, jump_continue);

    llvm->basic_block = jump_continue;
    LLVMValueRef llvm_cond = llvm_codegen_condition(context, cond);
    LLVMPositionBuilderAtEnd(b, jump_continue);
    LLVMBuildCondBr(b, llvm_cond, loop_body, jump_break);

    // Finally append the break block to the end of the loop
    llvm->basic_block = jump_break;
    LLVMAppendExistingBasicBlock(fn, jump_break);

    // Finally, pop our jump targets since we have them
    llvm_codegen_pop_jumps(context);
}

void llvm_codegen_goto_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_GOTO));
    
    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef bb = llvm->basic_block;

    // First get the label declaration from the goto.
    Declaration* label = statement_goto_get(stmt);

    // Then we will need to loopuk the declaration and create a declaration for
    // the basic block if it does not already exist. Otherwise, if it exists
    // simply convert it to a basic block so that we can use it.
    LLVMValueRef vbb = llvm_codegen_get_declaration(context, label);
    LLVMBasicBlockRef label_bb = NULL;
    if (vbb == NULL)
    {
        label_bb = LLVMCreateBasicBlockInContext(c, "");
    }
    else
    {
        label_bb = LLVMValueAsBasicBlock(vbb);
    }

    // Okay now what we need to do it to create an unconditional jump to that 
    // basic block
    LLVMPositionBuilderAtEnd(b, bb);
    LLVMBuildBr(b, label_bb);

    // Now we will append another basic block onto the function
    llvm->basic_block = LLVMAppendBasicBlockInContext(c, fn, "");
}

void llvm_codegen_continue_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CONTINUE));

    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    LLVMBasicBlockRef target = llvm_codegen_get_continue(context);
    LLVMPositionBuilderAtEnd(b, bb);
    LLVMBuildBr(b, target);
}

void llvm_codegen_break_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_BREAK));
    
    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMBasicBlockRef bb = llvm->basic_block;

    LLVMBasicBlockRef target = llvm_codegen_get_break(context);
    LLVMPositionBuilderAtEnd(b, bb);
    LLVMBuildBr(b, target);
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
    if (next != NULL)
    {
        llvm->basic_block = LLVMAppendBasicBlockInContext(c, fn, "");
    }
}

void llvm_codegen_decl_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DECLARATION));

    if (statement_declaration_is_single(stmt))
    {
        Declaration* declaration = statement_declaration_get_signle(stmt);
        assert(declaration != NULL);
        llvm_codegen_declaration(context, declaration);
    }
    else
    {
        DeclarationListEntry* list = statement_declaration_get_multiple(stmt);
        while (list != NULL)
        {
            Declaration* declaration = declaration_list_entry_get(list);
            assert(declaration != NULL);
            llvm_codegen_declaration(context, declaration);
            list = declaration_list_next(list);
        }
    }
}

void llvm_codegen_label_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_LABEL));

    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get the label declaration from this statement
    Declaration* label = statement_label_get(stmt);
    Statement* body = statement_label_get_body(stmt);

    // Okay we now need to look up if we have already created code for this 
    // label yet or not. If we have not seen a previous goto then append the
    // block to the end and add the declaration in, otherwise append the block
    // we had previously created.
    LLVMValueRef label_vr = llvm_codegen_get_declaration(context, label);
    LLVMBasicBlockRef label_bb = NULL;
    if (label_vr == NULL)
    {
        label_bb = LLVMAppendBasicBlockInContext(c, fn, "");

        label_vr = LLVMBasicBlockAsValue(label_bb);
        llvm_codegen_add_declaration(context, label, label_vr);
    }
    else
    {
        label_bb = LLVMValueAsBasicBlock(label_vr);
        LLVMAppendExistingBasicBlock(fn, label_bb);
    }

    // Now we need to end the current back block and produce a jump to this but,
    // only do this if the previous basic block DID NOT have a terminator. e.g
    // the folloing could occur:
    //                 void foo(void) {
    //                     ...
    //                     return;
    //                 label:
    //                     ...
    //                 }
    if (LLVMGetBasicBlockTerminator(current_bb) == NULL)
    {
        LLVMPositionBuilderAtEnd(b, current_bb);
        LLVMBuildBr(b, label_bb);
    }

    // Okay we have now build an unconditional jump to this basic block which is
    // good. We now just need to generate the code for this label. So set this
    // to be the current basic block and just generate the statement
    llvm->basic_block = label_bb;
    llvm_codegen_statement(context, body);
}

void llvm_codegen_case_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CASE));
    assert(llvm_codegen_get_switch(context) != NULL);
    
    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef current_bb = llvm->basic_block;

    // Get the information requried from the statement
    ExpressionIntegerValue value = statement_case_get_value(stmt);
    int64_t int_value = expression_integer_value_get(&value);
    Statement* body = statement_case_get_body(stmt);

    // TODO: llvm int32incontext isn't the right answer... but how should we
    // TODO: model it nicely?
    LLVMValueRef llvm_value = LLVMConstInt(LLVMInt32TypeInContext(c), int_value,
            false);

    // Then create a new basic block for this one and build an unconditional
    // branch to this one if the previous block did not have a terminator. e.g.
    // in a case and the writer wanted fallthrough to the next case statement
    LLVMBasicBlockRef new_bb = LLVMAppendBasicBlockInContext(c, fn, "");    
    if (LLVMGetBasicBlockTerminator(current_bb) == NULL)
    {
        LLVMPositionBuilderAtEnd(b, current_bb);
        LLVMBuildBr(b, new_bb);
    }

    // Then add this case to the current switch stack.
    LLVMAddCase(llvm_codegen_get_switch(context), llvm_value, new_bb);

    // Finally, codegen the statement for this switch case
    llvm->basic_block = new_bb;
    llvm_codegen_statement(context, body);
}

void llvm_codegen_default_statement(CodegenContext* context, Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DEFAULT));
    assert(llvm_codegen_get_switch(context) != NULL);
    assert(llvm_codegen_get_default(context) != NULL);
    
    // Get all of the context we need in order to generate this 'return'
    CodegenLLVM* llvm = context->backend_specific;
    LLVMContextRef c = llvm->context;
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMBasicBlockRef current_bb = llvm->basic_block;
    LLVMBasicBlockRef current_default = llvm_codegen_get_default(context);

    // Now get things we will need from the statement
    Statement* body = statement_default_get_body(stmt);

    // Terminate the current block if need with a jump to this default one
    if (LLVMGetBasicBlockTerminator(current_bb) == NULL)
    {
        LLVMPositionBuilderAtEnd(b, current_bb);
        LLVMBuildBr(b, current_default);
    }

    // Now we can apped our default block in a sensible position since we didn't
    // want to append it before to hopefully generate more human readable LLVMIR
    LLVMAppendExistingBasicBlock(fn, current_default);
    
    // Now we can set the current basic block and generate the statment
    llvm->basic_block = current_default;
    llvm_codegen_statement(context, body);
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
    LLVMBuilderRef b = llvm->builder;
    LLVMValueRef fn = llvm->function;
    LLVMTypeRef fn_ty = llvm->function_type;

    // Create the basic block and add it and the end of the function
    llvm->basic_block = LLVMAppendBasicBlockInContext(c, fn, "");
    
    // As the first instruction of the first basic block, generate an alloca for
    // a return value if the last block does not have a terminator
    LLVMTypeRef fn_return_type = LLVMGetReturnType(fn_ty);
    bool void_return = LLVMGetTypeKind(fn_return_type) == LLVMVoidTypeKind;

    LLVMValueRef maybe_return = NULL;
    if (!void_return)
    {   
        LLVMPositionBuilderAtEnd(b, llvm->basic_block);
        maybe_return = LLVMBuildAlloca(b, fn_return_type, "");
    }

    // Then generate the function body
    llvm_codegen_compound_statement(context, body);

    // Finally get the last block from the function and generate an implicit 
    // return if the last block does not have a return statement
    if (LLVMGetBasicBlockTerminator(llvm->basic_block) == NULL)
    {
        LLVMPositionBuilderAtEnd(b, llvm->basic_block);
        if (void_return)
        {
            LLVMBuildRetVoid(b);
        }
        else
        {
            // Build the return by loading the value from the alloca.
            LLVMBuildRet(b, LLVMBuildLoad2(b, fn_return_type, maybe_return,
                    ""));
        }
    }
}
