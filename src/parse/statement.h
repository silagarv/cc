#ifndef STATEMENT_H
#define STATEMENT_H

#include "util/str.h"

#include "lex/location.h"

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
    STATEMENT_EMPTY,
    
    STATEMENT_GOTO,
    STATEMENT_CONTINUE,
    STATEMENT_BREAK,
    STATEMENT_RETURN
};
typedef enum StatementType StatementType;

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

typedef union Statement Statement;

struct StatementBase {
    StatementType type;
    Location loc;
    Statement* parent; /* the parent statement of the execution*/
};

// Labelled statements
struct StatementLabel {
    StatementBase base;
    String name;
    Statement* statement;
};

struct StatementCase {
    StatementBase base;
    Expression* constant_expression;
    Statement* statement;
};

struct StatementDefault {
    StatementBase base;
    Statement* statement;
};

struct StatementCompound {
    StatementBase base;
    // Maybe add a block item list opt?
    Statement* statement;
    size_t statement_count;
};

struct StatementExpression {
    StatementBase base;
    Expression* expression;
};

// selection statement
struct StatementIf {
    StatementBase base;
    Expression* expression;
    Statement* true_part;
    Statement* false_part;
};

struct StatementSwitch {
    StatementBase base;
    Expression* expression;
    Statement* body;
    // All of the cases in the switch
    StatementCase* first_case;
    StatementCase* last_case;
    StatementDefault* default_label;
};

// Iteration statement
struct StatementWhile {
    StatementBase base;
    Expression* expression;
    Statement* body;
};

struct StatementDoWhile {
    StatementBase base;
    Statement* body;
    Expression* expression;
};

struct StatementFor {
    StatementBase base;
    Expression* initialisation;
    Expression* condition;
    Statement* body;
};

// Jump statement
struct StatmentGoto {
    StatementBase base;
    String label_name; // This will need to be changed in the future
};

struct StatmentContinue {
    StatementBase base;
};

struct StatmentBreak {
    StatementBase base;
};

struct StatmentReturn {
    StatementBase base;
    Expression* expression_opt;
};

union Statement {
    // Base statement for getting type and such
    StatementBase base;

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
};

#endif /* STATEMENT_H */
