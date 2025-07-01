#ifndef STATEMENT_H
#define STATEMENT_H

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
typedef struct StatmentLabel StatmentLabel;
typedef struct StatmentCase StatmentCase;
typedef struct StatmentDefault StatmentDefault;
typedef struct StatmentCompound StatmentCompound;
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
};

// Labelled statements
struct StatmentLabel {
    StatementBase base;
    // Identifier in here somehow?
    Statement* statement;
};

struct StatmentCase {
    StatementBase base;
    Expression* constant_expression;
    Statement* statement;
};

struct StatmentDefault {
    StatementBase base;
    Statement* statement;
};

struct StatmentCompound {
    StatementBase base;
    // Maybe add a block item list opt?
    Statement* statement;
    size_t used;
    size_t allocated;
};

struct StatementExpression {
    StatementBase base;
    Expression* opt;
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
};

// Jump statement
struct StatmentGoto {
    StatementBase base;
    // Need to have some kind of label thing
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
    StatmentLabel label_stmt;
    StatmentCase case_stmt;
    StatmentDefault default_stmt;

    // Compound ... duh
    StatmentCompound compound_stmt;

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
