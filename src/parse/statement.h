#ifndef STATEMENT_H
#define STATEMENT_H

#include "files/location.h"

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
typedef struct StatmentGoto StatmentGoto;
typedef struct StatmentContinue StatmentContinue;
typedef struct StatmentBreak StatmentBreak;
typedef struct StatmentReturn StatmentReturn;
typedef struct StatementEmpty StatementEmpty;
typedef struct StatementDeclaration StatementDeclaration;

typedef union Statement Statement;

struct StatementBase {
    StatementType type;
};

// An error statement
struct StatementError {
    StatementBase base;
    // TODO: should we even contain anything in the erorr case
};

// Labelled statements
struct StatementLabel {
    StatementBase base;
    Location identifier_location;
    Location colon_location;
    DeclarationLabel* label;
    Statement* statement;
};

struct StatementCase {
    StatementBase base;
    Location case_location;
    Location colon_location;
    Expression* constant_expression;
    IntegerValue expression_value;
    Statement* statement;
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
    Statement* statement;
    size_t statement_count;
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
    Statement* body;
};

// Iteration statement
struct StatementWhile {
    StatementBase base;
    Location while_location;
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
struct StatmentGoto {
    StatementBase base;
    Location goto_location;
    Location semi_location;
    Declaration* label;
};

struct StatmentContinue {
    StatementBase base;
    Location continue_location;
    Location semi_location;
    Statement* target; // the top level statement to go to
};

struct StatmentBreak {
    StatementBase base;
    Location break_location;
    Location semi_location;
    Statement* target; // the statement we are breaking from
};

struct StatmentReturn {
    StatementBase base;
    Location return_location;
    Location location_semi;
    Expression* expression_opt;
};

struct StatementEmpty {
    StatementBase base;
    Location semi_location;
};

struct StatementDeclaration {
    StatementBase base;
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
    StatmentGoto goto_stmt;
    StatmentContinue continue_stmt;
    StatmentBreak break_stmt;
    StatmentReturn return_stmt;

    // An empty statement (just a ';')
    StatementEmpty empty_stmt;

    // A declaration of a variable. Special type of statement.
    StatementDeclaration declaration_stmt;
};

Statement* statement_create_error(void);

Statement* statement_create_label(Location identifier_location, 
        Location colon_location, DeclarationLabel* label_decl, 
        Statement* body);

// TODO: case, default, compound, expression, while, do, for

Statement* statement_create_goto(Location goto_location, Location semi_location,
        Declaration* label);

Statement* statement_create_contine(Location continue_location, 
        Location semi_location, Statement* target_location);

Statement* statement_create_break(Location break_location,
        Location semi_location, Statement* target_location);

Statement* statement_create_return(Location return_location,
        Location semi_location, Expression* expression);

Statement* statement_create_empty(Location semi_location);

#endif /* STATEMENT_H */
