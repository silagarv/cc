#ifndef STATEMENT_H
#define STATEMENT_H

#include "parse/expression_eval.h"
#include "util/vec.h"

#include "files/location.h"

#include "parse/ast_allocator.h"
#include "parse/expression.h"
#include "parse/declaration.h"

enum StatementType {
    STATEMENT_ERROR = -1,

    STATEMENT_LABEL,
    STATEMENT_CASE,
    STATEMENT_DEFAULT,

    STATEMENT_COMPOUND,

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

    STATEMENT_DECLARATION
};
typedef enum StatementType StatementType;

typedef struct StatementError StatementError;
typedef struct StatementBase StatementBase;
typedef struct StatementLabel StatementLabel;
typedef struct StatementCase StatementCase;
typedef struct StatementDefault StatementDefault;
typedef struct StatementCompound StatementCompound;
typedef struct StatementExpression StatementExpression;
typedef struct StatementIf StatementIf;
typedef struct StatementSwitch StatementSwitch;
typedef struct StatementWhile StatementWhile;
typedef struct StatementDoWhile StatementDoWhile;
typedef struct StatementFor StatementFor;
typedef struct StatementGoto StatementGoto;
typedef struct StatementContinue StatementContinue;
typedef struct StatementBreak StatementBreak;
typedef struct StatementReturn StatementReturn;
typedef struct StatementEmpty StatementEmpty;
typedef struct StatementDeclaration StatementDeclaration;

typedef union Statement Statement;

struct StatementBase {
    StatementType type;
    Statement* next;
};

// An error statement
struct StatementError {
    StatementBase base;
    // TODO: I would eventlually like to at least add a location here
};

// Labelled statements
struct StatementLabel {
    StatementBase base;
    Location identifier_location;
    Location colon_location;
    Declaration* label;
    Statement* statement;
};

struct StatementCase {
    StatementBase base;
    Location case_location;
    Location colon_location;
    Expression* constant_expression;
    ExpressionIntegerValue expression_value;
    Statement* statement;

    Statement* next_case; // Since the next field in base is used
};

struct StatementDefault {
    StatementBase base;
    Location default_location;
    Location colon_location;
    Statement* statement;
};

struct StatementCompound {
    StatementBase base;
    Location opening_curly;
    Location closing_curly;
    Statement* first;
};

struct StatementExpression {
    StatementBase base;
    Location semi_location;
    Expression* expression;
};

// selection statement
struct StatementIf {
    StatementBase base;
    Location if_location;
    Location left_paren;
    Location right_paren;
    Location else_location;
    Expression* expression;
    Statement* true_part;
    Statement* false_part; // implies and else (but how to represent?)
};

struct StatementSwitch {
    StatementBase base;
    Location switch_location;
    Location left_paren;
    Location right_paren;
    Expression* expression;

    Statement* body; // The general body statement
    Statement* cases; // The start of all of the case statements
    Statement* default_stmt; // The default statement if present
};

// Iteration statement
struct StatementWhile {
    StatementBase base;
    Location while_location;
    Location left_paren;
    Location right_paren;
    Expression* expression;
    Statement* body;
};

struct StatementDoWhile {
    StatementBase base;
    Location do_location;
    Location while_location;
    Location left_paren;
    Location right_paren;
    Expression* expression;
    Statement* body;
};

struct StatementFor {
    StatementBase base;
    Location for_location;
    Location left_paren;
    Location right_paren;
    Statement* init;
    Expression* condition;
    Expression* increment;
    Statement* body;
};

// Jump statement
struct StatementGoto {
    StatementBase base;
    Location goto_location;
    Location semi_location;
    Declaration* label;
};

struct StatementContinue {
    StatementBase base;
    Location continue_location;
    Location semi_location;
};

struct StatementBreak {
    StatementBase base;
    Location break_location;
    Location semi_location;
};

struct StatementReturn {
    StatementBase base;
    Location return_location;
    Location location_semi;
    Expression* expression_opt;
    // TODO: should we point to the current function here?
};

struct StatementEmpty {
    StatementBase base;
    Location semi_location;
};

struct StatementDeclaration {
    StatementBase base;
    Location semi_location;
    Declaration* declaration;
};

union Statement {
    // Base statement for getting type and such
    StatementBase base;

    // The error case if we get a bad statement
    StatementError error_stmt;

    // Labelled statements
    StatementLabel label_stmt;
    StatementCase case_stmt;
    StatementDefault default_stmt;

    // Compound ... duh
    StatementCompound compound_stmt;

    // Expression statement
    StatementExpression expression_stmt;

    // Selection statements
    StatementIf if_stmt;
    StatementSwitch switch_stmt;

    // Iteration Statements
    StatementWhile while_stmt;
    StatementDoWhile do_while_stmt;
    StatementFor for_stmt;

    // Jump statements
    StatementGoto goto_stmt;
    StatementContinue continue_stmt;
    StatementBreak break_stmt;
    StatementReturn return_stmt;

    // An empty statement (just a ';')
    StatementEmpty empty_stmt;

    // A declaration of a variable. Special type of statement.
    StatementDeclaration declaration_stmt;
};

vector_of_decl(Statement*, Statement, statement);

// TODO: some of below needs to be redone since some of these statements need
// to exist before we can make a body

StatementType statement_get_kind(const Statement* stmt);
void statement_set_next(Statement* stmt, Statement* next);
Statement* statement_get_next(const Statement* stmt);

Statement* statement_create_error(AstAllocator* allocator);

Statement* statement_create_label(AstAllocator* allocator, 
        Location identifier_location, Location colon_location,
        Declaration* label_decl, Statement* body);

Statement* statement_create_case(AstAllocator* allocator, 
        Location case_location, Location colon_location, Expression* expr,
        ExpressionIntegerValue value, Statement* body);
void statement_case_set_next(Statement* stmt, Statement* next);
Statement* statement_case_get_next(const Statement* stmt);
ExpressionIntegerValue statement_case_get_value(const Statement* stmt);

Statement* statement_create_default(AstAllocator* allocator,
        Location default_location, Location colon_location, Statement* body);

Statement* statement_create_compound(AstAllocator* allocator,
        Location opening_curly, Location closing_curly, Statement* first);
Statement* statement_compound_get_first(const Statement* stmt);

Statement* statement_create_expression(AstAllocator* allocator, 
        Location semi_location, Expression* expression);

Statement* statement_create_if(AstAllocator* allocator, Location if_location,
        Location left_paren, Location right_paren, Location else_location,
        Expression* condition, Statement* true_part, Statement* false_part);

Statement* statement_create_while(AstAllocator* allocator, 
        Location while_location, Location left_paren, Location right_paren,
        Expression* condition);

void statement_while_set_body(Statement* while_statement, Statement* body);

// Note: a do while has extremely limited information when created but gets
// all of its information later...
Statement* statement_create_do_while(AstAllocator* allocator,
        Location do_location);

void statement_do_while_set_body(Statement* do_while_statement,
        Location while_location, Location left_paren, Location right_paren,
        Expression* condition, Statement* body);

Statement* statement_create_for(AstAllocator* allocator, Location for_location,
        Location left_paren, Location right_paren, Statement* init,
        Expression* cond, Expression* inc);

void statement_for_set_body(Statement* for_statement, Statement* body);

Statement* statement_create_switch(AstAllocator* allocator, 
        Location switch_location, Location left_paren, Location right_paren,
        Expression* cond);

void statement_switch_set_body(Statement* switch_statement, Statement* body);

Statement* statement_create_goto(AstAllocator* allocator, 
        Location goto_location, Location semi_location, Declaration* label);

Statement* statement_create_contine(AstAllocator* allocator,
        Location continue_location, Location semi_location);

Statement* statement_create_break(AstAllocator* allocator, 
        Location break_location, Location semi_location);

Statement* statement_create_return(AstAllocator* allocator,
        Location return_location, Location semi_location,
        Expression* expression);
Expression* statement_return_get_expression(const Statement* stmt);

Statement* statement_create_empty(AstAllocator* allocator,
        Location semi_location);

Statement* statement_create_declaration(AstAllocator* allocator,
        Location semi_location, Declaration* declaration);

bool statement_is(const Statement* stmt, StatementType type);

#endif /* STATEMENT_H */
