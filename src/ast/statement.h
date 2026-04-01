#ifndef AST_STATEMENT_H
#define AST_STATEMENT_H

#include <stdint.h>

#include "files/location.h"

#include "ast/ast_fwd.h"
#include "ast/allocator.h"

typedef enum StatementType {
    STATEMENT_ERROR,

    STATEMENT_COMPOUND,

    STATEMENT_LABEL,
    STATEMENT_CASE,
    STATEMENT_DEFAULT,

    STATEMENT_EXPRESSION,

    STATEMENT_IF,
    STATEMENT_SWITCH,

    STATEMENT_FOR,
    STATEMENT_WHILE,
    STATEMENT_DO_WHILE,
    
    STATEMENT_GOTO,
    STATEMENT_CONTINUE,
    STATEMENT_BREAK,
    STATEMENT_RETURN,

    STATEMENT_EMPTY,

    STATEMENT_DECLARATION,

    STATEMENT_ASM
} StatementType;

// The base structure of a statement which is used in all statement types.
typedef struct StatementBase {
    StatementType kind;
    Statement* next;
} StatementBase;

StatementType statement_type(const Statement* stmt);
Statement* statement_next(const Statement* stmt);
void statement_base_set_next(Statement* stmt, Statement* next);

bool statement_is(const Statement* stmt, StatementType type);

// An error statement to act as a placeholder in the AST so that we are able 
// to simply create an error and keep parsing
typedef struct StatementError {
    StatementBase base;
    Location loc;
} StatementError;

Statement* statement_error_create(AstAllocator* ast, Location loc);
Location statement_error_location(const Statement* stmt);

// A compound statement which is also used to represent a function body.
typedef struct StatementCompound {
    StatementBase base;
    Location lcurly;
    Location rcurly;
    Statement* first;
    Statement* last;
} StatementCompound;

Statement* statement_create_compound(AstAllocator* ast, Location lcurly,
        Statement* first, Statement* last, Location rcurly);
Location statement_compound_lcurly(const Statement* stmt);
Location statement_compound_rcurly(const Statement* stmt);
Statement* statement_compound_first(const Statement* stmt);
Statement* statement_compound_last(const Statement* stmt);

// A label statement i.e. the target of a GOTO
typedef struct StatementLabel {
    StatementBase base;
    Location identifier;
    Location colon;
    Declaration* decl;
    Statement* stmt;
} StatementLabel;

Statement* statement_create_label(AstAllocator* ast, Location identifier,
        Location colon, Declaration* decl, Statement* stmt);
Location statement_label_identifier(const Statement* stmt);
Location statement_label_colon(const Statement* stmt);
Declaration* statement_label_declaration(const Statement* stmt);
Statement* statement_label_statement(const Statement* stmt);

// A case statement. Only available in switch statements.
typedef struct StatementCase {
    StatementBase base;
    Location case_loc;
    Location colon;
    Expression* expr;
    int64_t value;
    Statement* stmt;
    Statement* next_case;
} StatementCase;

Statement* statement_create_case(AstAllocator* ast, Location case_loc,
        Location colon, Expression* expr, int64_t value, Statement* body);
Location statement_case_case(const Statement* stmt);
Location statement_case_colon(const Statement* stmt);
Expression* statement_case_expression(const Statement* stmt);
int64_t statement_case_value(const Statement* stmt);
Statement* statement_case_statement(const Statement* stmt);
Statement* statement_case_next(const Statement* stmt);
void statement_case_set_next(Statement* stmt, Statement* next);

// A default statement
typedef struct StatementDefault {
    StatementBase base;
    Location default_loc;
    Location colon;
    Statement* stmt;
} StatementDefault;

Statement* statement_create_default(AstAllocator* ast, Location default_loc,
        Location colon, Statement* body);
Location statement_default_default(const Statement* stmt);
Location statement_default_colon(const Statement* stmt);
Statement* statement_default_statement(const Statement* stmt);

// An expression statement
typedef struct StatementExpression {
    StatementBase base;
    Location semi;
    Expression* expr;
} StatementExpression;

Statement* statement_create_expression(AstAllocator* ast,  Location semi,
        Expression* expr);
Location statement_expression_semi(const Statement* stmt);
Expression* statement_expression_expression(const Statement* stmt);

// An if statement
typedef struct StatementIf {
    StatementBase base;
    Location if_loc;
    Location lparen;
    Location rparen;
    Location else_loc;
    Expression* cond;
    Statement* then_part;
    Statement* else_part;
} StatementIf;

Statement* statement_create_if(AstAllocator* ast, Location if_loc,
        Location lparen, Location rparen, Location else_loc, Expression* cond,
        Statement* then_part, Statement* else_part);
Location statement_if_if(const Statement* stmt);
Location statement_if_lparen(const Statement* stmt);
Location statement_if_rparen(const Statement* stmt);
Location statement_if_else(const Statement* stmt);
Expression* statement_if_condition(const Statement* stmt);
Statement* statement_if_then_part(const Statement* stmt);
Statement* statement_if_else_part(const Statement* stmt);

// A switch statement
typedef struct StatementSwitch {
    StatementBase base;
    Location switch_loc;
    Location lparen;
    Location rparen;
    Expression* expr;
    Statement* body;
    Statement* first_case;
    Statement* default_case;
} StatementSwitch;

Statement* statement_create_switch(AstAllocator* ast, Location switch_loc,
        Location lparen, Location rparen, Expression* cond, Statement* body,
        Statement* first_case, Statement* default_case);
Location statement_switch_switch(const Statement* stmt);
Location statement_switch_lparen(const Statement* stmt);
Location statement_switch_rparen(const Statement* stmt);
Expression* statement_switch_condition(const Statement* stmt);
Statement* statement_switch_body(const Statement* stmt);
Statement* statement_switch_first_case(const Statement* stmt);
Statement* statement_switch_default(const Statement* stmt);

// A while statement
typedef struct StatementWhile {
    StatementBase base;
    Location while_loc;
    Location lparen;
    Location rparen;
    Expression* cond;
    Statement* body;
} StatementWhile;

Statement* statement_create_while(AstAllocator* ast, Location while_loc,
        Location lparen, Location rparen, Expression* cond, Statement* body);
Location statement_while_while(const Statement* stmt);
Location statement_while_lparen(const Statement* stmt);
Location statement_while_rparen(const Statement* stmt);
Expression* statement_while_condition(const Statement* stmt);
Statement* statement_while_body(const Statement* stmt);

// A do while statement
typedef struct StatementDoWhile {
    StatementBase base;
    Location do_loc;
    Location while_loc;
    Location lparen;
    Location rparen;
    Expression* expr;
    Statement* body;
} StatementDoWhile;

Statement* statement_create_do(AstAllocator* ast, Location do_loc,
        Location while_loc, Location lparen, Location rparen, Expression* cond,
        Statement* body);
Location statement_do_do(const Statement* stmt);
Location statement_do_while(const Statement* stmt);
Location statement_do_lparen(const Statement* stmt);
Location statement_do_rparen(const Statement* stmt);
Expression* statement_do_condition(const Statement* statement);
Statement* statement_do_body(const Statement* statement);

// A for statement
typedef struct StatementFor {
    StatementBase base;
    Location for_loc;
    Location lparen;
    Location rparen;
    Statement* init;
    Expression* cond;
    Expression* inc;
    Statement* body;
} StatementFor;

Statement* statement_create_for(AstAllocator* ast, Location for_loc,
        Location lparen, Location rparen, Statement* init, Expression* cond,
        Expression* inc, Statement* body);
Location statement_for_for(const Statement* stmt);
Location statement_for_lparen(const Statement* stmt);
Location statement_for_rparen(const Statement* stmt);
Statement* statement_for_init(const Statement* stmt);
Expression* statement_for_condition(const Statement* stmt);
Expression* statement_for_inc(const Statement* stmt);
Statement* statement_for_body(const Statement* stmt);

// A goto statement
typedef struct StatementGoto {
    StatementBase base;
    Location goto_loc;
    Location semi;
    Declaration* label;
} StatementGoto;

Statement* statement_create_goto(AstAllocator* ast, Location goto_loc,
        Location semi, Declaration* label);
Location statement_goto_goto(const Statement* stmt);
Location statement_goto_semi(const Statement* stmt);
Declaration* statement_goto_label(const Statement* stmt);

// A continue statement
typedef struct StatementContinue {
    StatementBase base;
    Location continue_loc;
    Location semi;
} StatementContinue;

Statement* statement_create_contine(AstAllocator* ast, Location continue_loc,
    Location semi);
Location statement_continue_continue(const Statement* stmt);
Location statement_continue_semi(const Statement* stmt);

// A break statement, only available in loops, or a switch
typedef struct StatementBreak {
    StatementBase base;
    Location break_loc;
    Location semi;
} StatementBreak;

Statement* statement_create_break(AstAllocator* ast, Location break_loc,
    Location semi);
Location statement_break_break(const Statement* stmt);
Location statement_break_semi(const Statement* stmt);

// A return statement from a function
typedef struct StatementReturn {
    StatementBase base;
    Location return_loc;
    Location semi;
    Expression* expr;
} StatementReturn;

Statement* statement_create_return(AstAllocator* ast, Location return_loc,
        Location semi, Expression* expr);
Location statement_return_return(const Statement* stmt);
Location statement_return_semi(const Statement* stmt);
Expression* statement_return_expression(const Statement* stmt);

// An empty statement representing a statement which is just a semi-colon
typedef struct StatementEmpty {
    StatementBase base;
    Location semi;
} StatementEmpty;

Statement* statement_create_empty(AstAllocator* ast, Location semi);
Location statement_empty_semi(const Statement* stmt);

// A declaration statement which holds the statement which is either a single
// or group of different declarations.
typedef struct StatementDeclaration {
    StatementBase base;
    Location semi;
    Declaration* decl;
} StatementDeclaration;

Statement* statement_create_declaration(AstAllocator* ast, Location semi,
        Declaration* decl);
Location statement_declaration_semi(const Statement* stmt);
Declaration* statement_declaration_declaration(const Statement* stmt);

// The statement union itself. Simply a type punned union.
union Statement {
    StatementBase base_stmt;

    StatementCompound comp_stmt;

    StatementLabel label_stmt;
    StatementCase case_stmt;
    StatementDefault default_stmt;

    StatementIf if_stmt;
    StatementSwitch switch_stmt;

    StatementWhile while_stmt;
    StatementDoWhile do_while_stmt;
    StatementFor for_stmt;

    StatementGoto goto_stmt;
    StatementContinue continue_stmt;
    StatementBreak break_stmt;
    StatementReturn return_stmt;

    StatementEmpty empty_stmt;

    StatementExpression expr_stmt;

    StatementDeclaration decl_stmt;

    StatementError error_stmt;
};

#endif /* AST_STATEMENT_H */
