#ifndef EXPRESSION_H
#define EXPRESSION_H

// TODO: figure out how we want to implement expressions

enum ExpressionType {
    EXPRESSION_ERROR = -1,

    /* an identifer in the symbol table */
    EXPRESSION_IDENTIFIER,
    
    /* these 4 below are anything that is considered a constant */
    EXPRESSION_INTEGER_CONSTANT,
    EXPRESSION_FLOATING_CONSTANT,
    EXPRESSION_ENUMERATION_CONSTANT,
    EXPRESSION_CHARACTER_CONSTANT,

    /* string literal after concatenation */
    EXPRESSION_STRING_LITERAL,

    EXPRESSION_UNARY,
    EXPRESSION_BINARY,
    EXPRESSION_CONDITIONAL,
    EXPRESSION_ASSIGNMENT,
    EXPRESSION_LIST /* Comma seperated expressions*/
};
typedef enum ExpressionType ExpressionType;

typedef struct ExpressionBase ExpressionBase;

// Our primary expressions
typedef struct ExpressionIdentifier ExpressionIdentifier;
typedef struct ExpressionInteger ExpressionInteger;
typedef struct ExpressionFloat ExpressionFloat;
typedef struct ExpressionCharacter ExpressionCharacter;
typedef struct ExpressionStringLiteral ExpressionStringLiteral;

typedef struct ExpressionUnary ExpressionUnary;
typedef struct ExpressionBinary ExpressionBinary;
typedef struct ExpressionConditional ExpressionConditional;
typedef struct ExpressionAssignment ExpressionAssignment;
typedef struct ExpressionList ExpressionList;

typedef union Expression Expression;



Expression* expression_create(void);
void expression_delete(Expression* expression);

#endif /* EXPRESSION_H */
