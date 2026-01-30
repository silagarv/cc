#include "codegen_statement.h"

#include <assert.h>

#include "parse/declaration.h"
#include "parse/expression.h"
#include "parse/statement.h"

#include "codegen_expression.h"

void codegen_statement(const Statement* stmt);

void codegen_compound_statement(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_COMPOUND));

    Statement* curr = statement_compound_get_first(stmt);
    for (; curr != NULL; curr = statement_get_next(curr))
    {
        codegen_statement(curr);
    }
}

void codegen_declaration_statement(const Statement* stmt)
{
    // TODO: i think there might be a few bugs here to do with multiple
    // TODO: declarations in one statement
    // TODO: int a, b;
    // TODO:        ^ I believe this is the only one we'll get    
}

void codegen_label_statement(const Statement* stmt)
{
    
}

void codegen_case_statement(const Statement* stmt)
{
    
}

void codegen_default_statement(const Statement* stmt)
{
    
}

void codegen_expression_statement(const Statement* stmt)
{

}

void codegen_if_statement(const Statement* stmt)
{   
    // First get the condition from the if, evaluate it. Then if it compares
    // to non-zero, take the true branch, otherwise, take the false branch.
    Expression* condition = NULL;
    
}

void codegen_switch_statement(const Statement* stmt)
{
    
}

void codegen_for_statement(const Statement* stmt)
{
    
}

void codegen_while_statement(const Statement* stmt)
{
    
}

void codegen_do_while_statement(const Statement* stmt)
{
    
}

void codegen_goto_statement(const Statement* stmt)
{
    // First get the label declaration from the goto statement.
    Declaration* label = NULL;
}

void codegen_continue_statement(const Statement* stmt)
{
    
}

void codegen_break_statement(const Statement* stmt)
{
    
}

void codegen_return_statement(const Statement* stmt)
{
    // First generate the expression if we have such an expression.
    Expression* expression = statement_return_get_expression(stmt);
    if (expression != NULL)
    {
        codegen_expression(expression);
    }

    // Then generate the return part of the expression
}

void codegen_function_body(const Statement* stmt)
{
    codegen_compound_statement(stmt);
}

void codegen_statement(const Statement* stmt)
{
    switch (statement_get_kind(stmt))
    {
        // This is simple a ';' as a statement so nothing to do here
        case STATEMENT_EMPTY:
            return;

        case STATEMENT_COMPOUND:
            codegen_compound_statement(stmt);
            return;

        case STATEMENT_DECLARATION:
            codegen_declaration_statement(stmt);
            return;

        case STATEMENT_LABEL:
            codegen_label_statement(stmt);
            return;

        case STATEMENT_CASE:
            codegen_case_statement(stmt);  
            return;

        case STATEMENT_DEFAULT:
            codegen_default_statement(stmt);
            return;

        case STATEMENT_EXPRESSION:
            codegen_expression_statement(stmt);
            return;

        case STATEMENT_IF:
            codegen_if_statement(stmt);
            return;

        case STATEMENT_SWITCH:
            codegen_switch_statement(stmt);
            return;

        case STATEMENT_FOR:
            codegen_for_statement(stmt);
            return;

        case STATEMENT_WHILE:
            codegen_while_statement(stmt);
            return;

        case STATEMENT_DO_WHILE:
            codegen_do_while_statement(stmt);
            return;

        case STATEMENT_GOTO:
            codegen_goto_statement(stmt);
            return;

        case STATEMENT_CONTINUE:
            codegen_continue_statement(stmt);
            return;

        case STATEMENT_BREAK:
            codegen_break_statement(stmt);
            return;

        case STATEMENT_RETURN:
            codegen_return_statement(stmt);
            return;

        case STATEMENT_ERROR:
        default:
            panic("attempting codegen on error or unknown statement");
            return;
    }
}
