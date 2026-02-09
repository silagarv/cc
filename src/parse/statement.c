#include "statement.h"

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "util/panic.h"
#include "util/vec.h"

#include "parse/ast_allocator.h"

vector_of_impl(Statement*, Statement, statement)

StatementType statement_get_kind(const Statement* stmt)
{
    return stmt->base.type;
}

void statement_set_next(Statement* stmt, Statement* next)
{
    stmt->base.next = next;
}

bool statement_has_next(const Statement* stmt)
{
    return stmt->base.next != NULL;
}

Statement* statement_get_next(const Statement* stmt)
{
    return stmt->base.next;
}

static Statement* statement_create_base(AstAllocator* allocator,
        size_t size, StatementType type)
{
    Statement* stmt = ast_allocator_alloc(allocator, size);
    stmt->base.type = type;
    stmt->base.next = NULL;

    return stmt;
}

Statement* statement_create_error(AstAllocator* allocator)
{
    return statement_create_base(allocator, sizeof(StatementError), 
            STATEMENT_ERROR);
}

Statement* statement_create_label(AstAllocator* allocator,
        Location identifier_location, Location colon_location,
        Declaration* label_decl, Statement* statement)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementLabel),
            STATEMENT_LABEL);

    stmt->label_stmt.identifier_location = identifier_location;
    stmt->label_stmt.colon_location = colon_location;
    stmt->label_stmt.label = label_decl;
    stmt->label_stmt.statement = statement;

    return stmt;
}

Declaration* statement_label_get(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_LABEL));

    return stmt->label_stmt.label;
}

Statement* statement_label_get_body(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_LABEL));

    return stmt->label_stmt.statement;
}

Statement* statement_create_case(AstAllocator* allocator, 
        Location case_location, Location colon_location, Expression* expr,
        ExpressionIntegerValue value, Statement* body)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementCase),
            STATEMENT_CASE);
    stmt->case_stmt.case_location = case_location;
    stmt->case_stmt.colon_location = colon_location;
    stmt->case_stmt.constant_expression = expr;
    stmt->case_stmt.expression_value = value;
    stmt->case_stmt.statement = body;
    stmt->case_stmt.next_case = NULL;

    return stmt;
}

void statement_case_set_next(Statement* stmt, Statement* next)
{
    assert(statement_is(stmt, STATEMENT_CASE));
    assert(statement_is(next, STATEMENT_CASE));
    assert(stmt->case_stmt.next_case == NULL);

    stmt->case_stmt.next_case = next;
}

Statement* statement_case_get_next(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CASE));

    return stmt->case_stmt.next_case;
}

ExpressionIntegerValue statement_case_get_value(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CASE));

    return stmt->case_stmt.expression_value;
}

Statement* statement_case_get_body(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_CASE));

    return stmt->case_stmt.statement;
}

Statement* statement_create_default(AstAllocator* allocator,
        Location default_location, Location colon_location, 
        Statement* body)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementDefault),
            STATEMENT_DEFAULT);
    stmt->default_stmt.default_location = default_location;
    stmt->default_stmt.colon_location = colon_location;
    stmt->default_stmt.statement = body;

    return stmt;
}

// TODO: figure out how to create an array of statements or at least do some
// linked list style thing for them...
Statement* statement_create_compound(AstAllocator* allocator,
        Location opening_curly, Location closing_curly, Statement* first)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementCompound),
            STATEMENT_COMPOUND);
    stmt->compound_stmt.opening_curly = opening_curly;
    stmt->compound_stmt.closing_curly = closing_curly;
    stmt->compound_stmt.first = first;

    return stmt;
}

Statement* statement_compound_get_first(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_COMPOUND));

    return stmt->compound_stmt.first;
}

Statement* statement_create_expression(AstAllocator* allocator, 
        Location semi_location, Expression* expression)
{
    Statement* stmt = statement_create_base(allocator, 
            sizeof(StatementExpression), STATEMENT_EXPRESSION);
    stmt->expression_stmt.semi_location = semi_location;
    stmt->expression_stmt.expression = expression;

    return stmt;
}

Expression* statement_expression_get(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_EXPRESSION));

    return stmt->expression_stmt.expression;
}

Statement* statement_create_if(AstAllocator* allocator, Location if_location,
        Location left_paren, Location right_paren, Location else_location,
        Expression* condition, Statement* true_part, Statement* false_part)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementIf),
            STATEMENT_IF);
    stmt->if_stmt.if_location = if_location;
    stmt->if_stmt.left_paren = left_paren;
    stmt->if_stmt.right_paren = right_paren;
    stmt->if_stmt.else_location = else_location;
    stmt->if_stmt.expression = condition;
    stmt->if_stmt.true_part = true_part;
    stmt->if_stmt.false_part = false_part;

    return stmt;
}

Expression* statement_if_get_condition(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_IF));

    return stmt->if_stmt.expression;
}

Statement* statement_if_get_true_part(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_IF));

    return stmt->if_stmt.true_part;
}

Statement* statement_if_get_false_part(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_IF));

    return stmt->if_stmt.false_part;
}

Statement* statement_create_while(AstAllocator* allocator, 
        Location while_location, Location left_paren, Location right_paren,
        Expression* condition)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementWhile),
            STATEMENT_WHILE);
    stmt->while_stmt.while_location = while_location;
    stmt->while_stmt.left_paren = left_paren;
    stmt->while_stmt.right_paren = right_paren;
    stmt->while_stmt.expression =condition;
    stmt->while_stmt.body = NULL;

    return stmt;
}

void statement_while_set_body(Statement* while_statement, Statement* body)
{
    assert(while_statement->base.type == STATEMENT_WHILE);
    assert(while_statement->while_stmt.body == NULL);

    while_statement->while_stmt.body = body;
}

Expression* statement_while_get_condition(const Statement* statement)
{
    return statement->while_stmt.expression;
}

Statement* statement_while_get_body(const Statement* statement)
{
    return statement->while_stmt.body;
}

// Note: a do while has extremely limited information when created but gets
// all of its information later...
Statement* statement_create_do_while(AstAllocator* allocator,
        Location do_location)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementDoWhile),
            STATEMENT_DO_WHILE);
    stmt->do_while_stmt.do_location = do_location;

    return stmt;
}

void statement_do_while_set_body(Statement* do_while_statement,
        Location while_location, Location left_paren, Location right_paren,
        Expression* condition, Statement* body)
{
    do_while_statement->do_while_stmt.while_location = while_location;
    do_while_statement->do_while_stmt.left_paren = left_paren;
    do_while_statement->do_while_stmt.right_paren = right_paren;
    do_while_statement->do_while_stmt.expression = condition;
    do_while_statement->do_while_stmt.body = body;  
}

Expression* statement_do_while_get_condition(const Statement* statement)
{
    return statement->do_while_stmt.expression;
}

Statement* statement_do_while_get_body(const Statement* statement)
{
    return statement->do_while_stmt.body;
}

Statement* statement_create_for(AstAllocator* allocator, Location for_location,
        Location left_paren, Location right_paren, Statement* init,
        Expression* cond, Expression* inc)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementFor),
            STATEMENT_FOR);
    stmt->for_stmt.for_location = for_location;
    stmt->for_stmt.left_paren = left_paren;
    stmt->for_stmt.right_paren = right_paren;
    stmt->for_stmt.init = init;
    stmt->for_stmt.condition = cond;
    stmt->for_stmt.increment = inc;
    stmt->for_stmt.body = NULL;

    return stmt;
}

Statement* statement_for_get_init(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));

    return stmt->for_stmt.init;
}

Expression* statement_for_get_condition(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));

    return stmt->for_stmt.condition;
}

Expression* statement_for_get_inc(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));

    return stmt->for_stmt.increment;
}

Statement* statement_for_get_body(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_FOR));

    return stmt->for_stmt.body;
}

void statement_for_set_body(Statement* for_statement, Statement* body)
{
    assert(for_statement->base.type == STATEMENT_FOR);
    assert(for_statement->for_stmt.body == NULL);

    for_statement->for_stmt.body = body;
}

Statement* statement_create_switch(AstAllocator* allocator, 
        Location switch_location, Location left_paren, Location right_paren,
        Expression* cond)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementSwitch),
            STATEMENT_SWITCH);
    stmt->switch_stmt.switch_location = switch_location;
    stmt->switch_stmt.left_paren = left_paren;
    stmt->switch_stmt.right_paren = right_paren;
    stmt->switch_stmt.expression = cond;
    stmt->switch_stmt.body = NULL;
    
    return stmt;
}

void statement_switch_set_body(Statement* switch_statement, Statement* body)
{
    assert(switch_statement->base.type == STATEMENT_SWITCH);
    assert(switch_statement->switch_stmt.body == NULL);

    switch_statement->switch_stmt.body = body;
}

Expression* statement_switch_get_condition(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));

    return stmt->switch_stmt.expression;
}

Statement* statement_switch_get_body(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));

    return stmt->switch_stmt.body;
}

Statement* statement_switch_get_cases(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));

    return stmt->switch_stmt.cases;
}

Statement* statement_switch_get_default(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_SWITCH));

    return stmt->switch_stmt.default_stmt;
}

Statement* statement_create_goto(AstAllocator* allocator, 
        Location goto_location, Location semi_location, Declaration* label)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementGoto),
            STATEMENT_GOTO);
    stmt->goto_stmt.goto_location = goto_location;
    stmt->goto_stmt.semi_location = semi_location;
    stmt->goto_stmt.label = label;

    return stmt;
}

Declaration* statement_goto_get(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_GOTO));

    return stmt->goto_stmt.label;
}

Statement* statement_create_contine(AstAllocator* allocator, 
        Location continue_location, Location semi_location)
{
    Statement* stmt = statement_create_base(allocator, 
            sizeof(StatementContinue), STATEMENT_CONTINUE);
    stmt->continue_stmt.continue_location = continue_location;
    stmt->continue_stmt.semi_location = semi_location;

    return stmt;
}

Statement* statement_create_break(AstAllocator* allocator, 
        Location break_location, Location semi_location)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementBreak),
            STATEMENT_BREAK);
    stmt->break_stmt.break_location = break_location;
    stmt->break_stmt.semi_location = semi_location;

    return stmt;
}

Statement* statement_create_return(AstAllocator* allocator, 
        Location return_location, Location semi_location,
        Expression* expression)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementReturn),
            STATEMENT_RETURN);
    stmt->return_stmt.return_location = return_location;
    stmt->return_stmt.location_semi = semi_location;
    stmt->return_stmt.expression_opt = expression;

    return stmt;
}

Expression* statement_return_get_expression(const Statement* stmt)
{
    return stmt->return_stmt.expression_opt;
}

Statement* statement_create_empty(AstAllocator* allocator, 
        Location semi_location)
{
    Statement* stmt = statement_create_base(allocator, sizeof(StatementEmpty),
            STATEMENT_EMPTY);
    stmt->empty_stmt.semi_location = semi_location;

    return stmt;
}

Statement* statement_create_declaration(AstAllocator* allocator,
        Location semi_location, Declaration* declaration)
{
    Statement* stmt = statement_create_base(allocator, 
            sizeof(StatementDeclaration), STATEMENT_DECLARATION);

    stmt->declaration_stmt.semi_location = semi_location;
    stmt->declaration_stmt.declaration = declaration;

    return stmt;
}

Declaration* statement_declaration_get(const Statement* stmt)
{
    assert(statement_is(stmt, STATEMENT_DECLARATION));

    return stmt->declaration_stmt.declaration;
}

bool statement_is(const Statement* stmt, StatementType type)
{
    assert(stmt);

    return stmt->base.type == type;
}

bool statement_is_empty(const Statement* stmt)
{
    if (stmt == NULL)
    {
        return false;
    }

    if (statement_is(stmt, STATEMENT_EMPTY))
    {
        return true;
    }

    if (statement_is(stmt, STATEMENT_COMPOUND))
    {
        Statement* first = statement_compound_get_first(stmt);
        if (first == NULL)
        {
            return true;
        }
    }

    return false;
}

