#ifndef EXPRESSION_H
#define EXPRESSION_H

// TODO: figure out how we want to implement expressions

enum ExpressionType {
    EXPRESSION_ERROR = -1,

    EXPRESSION_IDENTIFIER,
    EXPRESSION_CONSTANT,
    EXPRESSION_STRING_LITERAL,

    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_CONDITIONAL,
    EXPRESSION_ASSIGNMENT,
};
typedef enum ExpressionType ExpressionType;

typedef struct ExpressionBase ExpressionBase;

// Our primary expressions
typedef struct ExpressionIdentifier ExpressionIdentifier;
typedef struct ExpressionConstant ExpressionConstant;
typedef struct ExpressionStringLiteral ExpressionStringLiteral;


typedef struct ExpressionConditional ExpressionConditional;
typedef struct ExpressionAssignment ExpressionAssignment;

typedef union Expression Expression;

#endif /* EXPRESSION_H */
