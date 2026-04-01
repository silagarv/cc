#include "statement.h"

#include <stddef.h>
#include <stdint.h>
#include <assert.h>

#include "util/panic.h"

#include "files/location.h"

#include "ast/ast_fwd.h"
#include "ast/allocator.h"

size_t statement_get_size(StatementType type)
{
    switch (type)
    {
        case STATEMENT_ERROR: return sizeof(StatementError);
        case STATEMENT_COMPOUND: return sizeof(StatementCompound);
        case STATEMENT_LABEL: return sizeof(StatementLabel);
        case STATEMENT_CASE: return sizeof(StatementCase);
        case STATEMENT_DEFAULT: return sizeof(StatementDefault);
        case STATEMENT_EXPRESSION: return sizeof(StatementExpression);
        case STATEMENT_IF: return sizeof(StatementIf);
        case STATEMENT_SWITCH: return sizeof(StatementSwitch);
        case STATEMENT_FOR: return sizeof(StatementFor);
        case STATEMENT_WHILE: return sizeof(StatementWhile);
        case STATEMENT_DO_WHILE: return sizeof(StatementDoWhile);
        case STATEMENT_GOTO: return sizeof(StatementGoto);
        case STATEMENT_CONTINUE: return sizeof(StatementContinue);
        case STATEMENT_BREAK: return sizeof(StatementBreak);
        case STATEMENT_RETURN: return sizeof(StatementReturn);
        case STATEMENT_EMPTY: return sizeof(StatementEmpty);
        case STATEMENT_DECLARATION: return sizeof(StatementDeclaration);
    }

    panic("unreachable");
    return 0;
}

Statement* statement_base_create(AstAllocator* ast, StatementType type)
{
    Statement* stmt = ast_allocator_alloc(ast, statement_get_size(type));
    stmt->base_stmt = (StatementBase)
    {
        .kind = type,
        .next = NULL
    };

    return stmt;
}

StatementType statement_type(const Statement* stmt)
{
    return stmt->base_stmt.kind;
}

Statement* statement_next(const Statement* stmt)
{
    return stmt->base_stmt.next;
}

void statement_base_set_next(Statement* stmt, Statement* next)
{
    stmt->base_stmt.next = next;
}

bool statement_is(const Statement* stmt, StatementType type)
{
    return statement_type(stmt) == type;
}

Statement* statement_error_create(AstAllocator* ast, Location loc)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_ERROR);
    stmt->error_stmt.loc = loc;

    return stmt;
}

Location statement_error_location(const Statement* stmt)
{
    return stmt->error_stmt.loc;
}

Statement* statement_create_compound(AstAllocator* ast, Location lcurly,
        Statement* first, Statement* last, Location rcurly)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_COMPOUND);
    stmt->comp_stmt.lcurly = lcurly;
    stmt->comp_stmt.rcurly = rcurly;
    stmt->comp_stmt.first = first;
    stmt->comp_stmt.last = last;

    return stmt;
}

Location statement_compound_lcurly(const Statement* stmt)
{
    return stmt->comp_stmt.lcurly;
}

Location statement_compound_rcurly(const Statement* stmt)
{
    return stmt->comp_stmt.rcurly;
}

Statement* statement_compound_first(const Statement* stmt)
{
    return stmt->comp_stmt.first;
}

Statement* statement_compound_last(const Statement* stmt)
{
    return stmt->comp_stmt.last;
}

Statement* statement_create_label(AstAllocator* ast, Location identifier,
        Location colon, Declaration* decl, Statement* stmt)
{
    Statement* new_stmt = statement_base_create(ast, STATEMENT_LABEL);
    new_stmt->label_stmt.identifier = identifier;
    new_stmt->label_stmt.colon = colon;
    new_stmt->label_stmt.decl = decl;
    new_stmt->label_stmt.stmt = stmt;

    return new_stmt;
}

Location statement_label_identifier(const Statement* stmt)
{
    return stmt->label_stmt.identifier;
}

Location statement_label_colon(const Statement* stmt)
{
    return stmt->label_stmt.colon;
}

Declaration* statement_label_declaration(const Statement* stmt)
{
    return stmt->label_stmt.decl;
}

Statement* statement_label_statement(const Statement* stmt)
{
    return stmt->label_stmt.stmt;
}

Statement* statement_create_case(AstAllocator* ast, Location case_loc,
        Location colon, Expression* expr, int64_t value, Statement* body)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_CASE);
    stmt->case_stmt.case_loc = case_loc;
    stmt->case_stmt.colon = colon;
    stmt->case_stmt.expr = expr;
    stmt->case_stmt.value = value;
    stmt->case_stmt.stmt = body;

    return stmt;
}

Location statement_case_case(const Statement* stmt)
{
    return stmt->case_stmt.case_loc;
}

Location statement_case_colon(const Statement* stmt)
{
    return stmt->case_stmt.colon;
}

Expression* statement_case_expression(const Statement* stmt)
{
    return stmt->case_stmt.expr;
}

int64_t statement_case_value(const Statement* stmt)
{
    return stmt->case_stmt.value;
}

Statement* statement_case_statement(const Statement* stmt)
{
    return stmt->case_stmt.stmt;
}

Statement* statement_case_next(const Statement* stmt)
{
    return stmt->case_stmt.next_case;
}

void statement_case_set_next(Statement* stmt, Statement* next)
{
    stmt->case_stmt.next_case = next;
}

Statement* statement_create_default(AstAllocator* ast, Location default_loc,
        Location colon, Statement* body)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_DEFAULT);
    stmt->default_stmt.default_loc = default_loc;
    stmt->default_stmt.colon = colon;
    stmt->default_stmt.stmt = body;

    return stmt;
}

Location statement_default_default(const Statement* stmt)
{
    return stmt->default_stmt.default_loc;
}

Location statement_default_colon(const Statement* stmt)
{
    return stmt->default_stmt.colon;
}

Statement* statement_default_statement(const Statement* stmt)
{
    return stmt->default_stmt.stmt;
}

Statement* statement_create_expression(AstAllocator* ast,  Location semi,
        Expression* expr)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_EXPRESSION);
    stmt->expr_stmt.semi = semi;
    stmt->expr_stmt.expr = expr;

    return stmt;
}

Location statement_expression_semi(const Statement* stmt)
{
    return stmt->expr_stmt.semi;
}

Expression* statement_expression_expression(const Statement* stmt)
{
    return stmt->expr_stmt.expr;
}

Statement* statement_create_if(AstAllocator* ast, Location if_loc,
        Location lparen, Location rparen, Location else_loc, Expression* cond,
        Statement* then_part, Statement* else_part)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_IF);
    stmt->if_stmt.if_loc = if_loc;
    stmt->if_stmt.lparen = lparen;
    stmt->if_stmt.rparen = rparen;
    stmt->if_stmt.else_loc = else_loc;
    stmt->if_stmt.cond = cond;
    stmt->if_stmt.then_part = then_part;
    stmt->if_stmt.else_part = else_part;

    return stmt;
}

Location statement_if_if(const Statement* stmt)
{
    return stmt->if_stmt.if_loc;
}

Location statement_if_lparen(const Statement* stmt)
{
    return stmt->if_stmt.lparen;
}

Location statement_if_rparen(const Statement* stmt)
{
    return stmt->if_stmt.rparen;
}

Location statement_if_else(const Statement* stmt)
{
    return stmt->if_stmt.else_loc;
}

Expression* statement_if_condition(const Statement* stmt)
{
    return stmt->if_stmt.cond;
}

Statement* statement_if_then_part(const Statement* stmt)
{
    return stmt->if_stmt.then_part;
}

Statement* statement_if_else_part(const Statement* stmt)
{
    return stmt->if_stmt.else_part;
}

Statement* statement_create_switch(AstAllocator* ast, Location switch_loc,
        Location lparen, Location rparen, Expression* cond, Statement* body,
        Statement* first_case, Statement* default_case)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_SWITCH);
    stmt->switch_stmt.switch_loc = switch_loc;
    stmt->switch_stmt.lparen = lparen;
    stmt->switch_stmt.rparen = rparen;
    stmt->switch_stmt.expr = cond;
    stmt->switch_stmt.body = body;
    stmt->switch_stmt.first_case = first_case;
    stmt->switch_stmt.default_case = default_case;

    return stmt;
}

Location statement_switch_switch(const Statement* stmt)
{
    return stmt->switch_stmt.switch_loc;
}

Location statement_switch_lparen(const Statement* stmt)
{
    return stmt->switch_stmt.lparen;
}

Location statement_switch_rparen(const Statement* stmt)
{
    return stmt->switch_stmt.rparen;
}

Expression* statement_switch_condition(const Statement* stmt)
{
    return stmt->switch_stmt.expr;
}

Statement* statement_switch_body(const Statement* stmt)
{
    return stmt->switch_stmt.body;
}

Statement* statement_switch_first_case(const Statement* stmt)
{
    return stmt->switch_stmt.first_case;
}

Statement* statement_switch_default(const Statement* stmt)
{
    return stmt->switch_stmt.default_case;
}

Statement* statement_create_while(AstAllocator* ast, Location while_loc,
        Location lparen, Location rparen, Expression* cond, Statement* body)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_WHILE);
    stmt->while_stmt.while_loc = while_loc;
    stmt->while_stmt.lparen = lparen;
    stmt->while_stmt.rparen = rparen;
    stmt->while_stmt.cond = cond;
    stmt->while_stmt.body = body;

    return stmt;
}

Location statement_while_while(const Statement* stmt)
{
    return stmt->while_stmt.while_loc;
}

Location statement_while_lparen(const Statement* stmt)
{
    return stmt->while_stmt.lparen;
}

Location statement_while_rparen(const Statement* stmt)
{
    return stmt->while_stmt.rparen;
}

Expression* statement_while_condition(const Statement* stmt)
{
    return stmt->while_stmt.cond;
}

Statement* statement_while_body(const Statement* stmt)
{
    return stmt->while_stmt.body;
}

Statement* statement_create_do(AstAllocator* ast, Location do_loc,
        Location while_loc, Location lparen, Location rparen, Expression* cond,
        Statement* body)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_DO_WHILE);
    stmt->do_while_stmt.do_loc = do_loc;
    stmt->do_while_stmt.while_loc = while_loc;
    stmt->do_while_stmt.lparen = lparen;
    stmt->do_while_stmt.rparen = rparen;
    stmt->do_while_stmt.expr = cond;
    stmt->do_while_stmt.body = body;

    return stmt;
}

Location statement_do_do(const Statement* stmt)
{
    return stmt->do_while_stmt.do_loc;
}

Location statement_do_while(const Statement* stmt)
{
    return stmt->do_while_stmt.while_loc;
}

Location statement_do_lparen(const Statement* stmt)
{
    return stmt->do_while_stmt.lparen;
}

Location statement_do_rparen(const Statement* stmt)
{
    return stmt->do_while_stmt.rparen;
}

Expression* statement_do_condition(const Statement* stmt)
{
    return stmt->do_while_stmt.expr;
}

Statement* statement_do_body(const Statement* stmt)
{
    return stmt->do_while_stmt.body;
}

Statement* statement_create_for(AstAllocator* ast, Location for_loc,
        Location lparen, Location rparen, Statement* init, Expression* cond,
        Expression* inc, Statement* body)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_FOR);
    stmt->for_stmt.for_loc = for_loc;
    stmt->for_stmt.lparen = lparen;
    stmt->for_stmt.rparen = rparen;
    stmt->for_stmt.init = init;
    stmt->for_stmt.cond = cond;
    stmt->for_stmt.inc = inc;
    stmt->for_stmt.body = body;

    return stmt;
}

Location statement_for_for(const Statement* stmt)
{
    return stmt->for_stmt.for_loc;
}

Location statement_for_lparen(const Statement* stmt)
{
    return stmt->for_stmt.lparen;
}

Location statement_for_rparen(const Statement* stmt)
{
    return stmt->for_stmt.rparen;
}

Statement* statement_for_init(const Statement* stmt)
{
    return stmt->for_stmt.init;
}

Expression* statement_for_condition(const Statement* stmt)
{
    return stmt->for_stmt.cond;
}

Expression* statement_for_inc(const Statement* stmt)
{
    return stmt->for_stmt.inc;
}

Statement* statement_for_body(const Statement* stmt)
{
    return stmt->for_stmt.body;
}

Statement* statement_create_goto(AstAllocator* ast, Location goto_loc,
        Location semi, Declaration* label)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_GOTO);
    stmt->goto_stmt.goto_loc = goto_loc;
    stmt->goto_stmt.semi = semi;
    stmt->goto_stmt.label = label;

    return stmt;
}     

Location statement_goto_goto(const Statement* stmt)
{
    return stmt->goto_stmt.goto_loc;
}

Location statement_goto_semi(const Statement* stmt)
{
    return stmt->goto_stmt.semi;
}

Declaration* statement_goto_label(const Statement* stmt)
{
    return stmt->goto_stmt.label;
}

Statement* statement_create_contine(AstAllocator* ast, Location continue_loc,
        Location semi)
{   
    Statement* stmt = statement_base_create(ast, STATEMENT_CONTINUE);
    stmt->continue_stmt.continue_loc = continue_loc;
    stmt->continue_stmt.semi = semi;

    return stmt;
}

Location statement_continue_continue(const Statement* stmt)
{
    return stmt->continue_stmt.continue_loc;
}

Location statement_continue_semi(const Statement* stmt)
{
    return stmt->continue_stmt.semi;
}

Statement* statement_create_break(AstAllocator* ast, Location break_loc,
        Location semi)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_BREAK);
    stmt->break_stmt.break_loc = break_loc;
    stmt->break_stmt.semi = semi;

    return stmt;
}

Location statement_break_break(const Statement* stmt)
{
    return stmt->break_stmt.break_loc;
}

Location statement_break_semi(const Statement* stmt)
{
    return stmt->break_stmt.semi;
}

Statement* statement_create_return(AstAllocator* ast, Location return_loc,
        Location semi, Expression* expr)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_RETURN);
    stmt->return_stmt.return_loc = return_loc;
    stmt->return_stmt.semi = semi;
    stmt->return_stmt.expr = expr;

    return stmt;
}

Location statement_return_return(const Statement* stmt)
{
    return stmt->return_stmt.return_loc;
}

Location statement_return_semi(const Statement* stmt)
{
    return stmt->return_stmt.semi;
}

Expression* statement_return_expression(const Statement* stmt)
{
    return stmt->return_stmt.expr;
}

Statement* statement_create_empty(AstAllocator* ast, Location semi)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_EMPTY);
    stmt->empty_stmt.semi = semi;
    
    return stmt;
}

Location statement_empty_semi(const Statement* stmt)
{
    return stmt->empty_stmt.semi;
}

Statement* statement_create_declaration(AstAllocator* ast, Location semi,
        Declaration* decl)
{
    Statement* stmt = statement_base_create(ast, STATEMENT_DECLARATION);
    stmt->decl_stmt.semi = semi;
    stmt->decl_stmt.decl = decl;

    return stmt;
}

Location statement_declaration_semi(const Statement* stmt)
{
    return stmt->decl_stmt.semi;
}

Declaration* statement_declaration_declaration(const Statement* stmt)
{
    return stmt->decl_stmt.decl;
}

