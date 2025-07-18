#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "lex/location.h"

#include "parse/type.h"

typedef enum ExpressionType {
    EXPRESSION_ERROR,

    EXPRESSION_REFERENCE, // an identifier in the symbol table
    
    EXPRESSION_INTEGER_CONSTANT,
    EXPRESSION_FLOATING_CONSTANT,
    EXPRESSION_ENUMERATION_CONSTANT,
    EXPRESSION_CHARACTER_CONSTANT,
    EXPRESSION_STRING_LITERAL,

    EXPRESSION_ARRAY_ACCESS,
    EXPRESSION_FUNCTION_CALL,
    EXPRESSION_MEMBER_ACCESS,
    EXPRESSION_MEMBER_POINTER_ACCESS,
    EXPRESSION_COMPOUND_LITERAL,
    EXPRESSION_SIZE_OF,
    EXPRESSION_CAST,

    EXPRESSION_UNARY_ADDRESS,
    EXPRESSION_UNARY_DEREFERENCE,
    EXPRESSION_UNARY_PLUS,
    EXPRESSION_UNARY_MINUS,
    EXPRESSION_UNARY_BIT_NOT,
    EXPRESSION_UNARY_NOT,
    EXPRESSION_UNARY_PRE_INCREMENT,
    EXPRESSION_UNARY_PRE_DECREMENT,
    EXPRESSION_UNARY_POST_INCREMENT,
    EXPRESSION_UNARY_POST_DECREMENT,

    EXPRESSION_BINARY_TIMES,
    EXPRESSION_BINARY_DIVIDE,
    EXPRESSION_BINARY_MODULO,
    EXPRESSION_BINARY_ADD,
    EXPRESSION_BINARY_SUBTRACT,
    EXPRESSION_BINARY_SHIFT_LEFT,
    EXPRESSION_BINARY_SHIFT_RIGHT,
    EXPRESSION_BINARY_LESS_THAN,
    EXPRESSION_BINARY_GREATER_THAN,
    EXPRESSION_BINARY_LESS_THAN_EQUAL,
    EXPRESSION_BINARY_GREATER_THAN_EQUAL,
    EXPRESSION_BINARY_EQUAL,
    EXPRESSION_BINARY_NOT_EQUAL,
    EXPRESSION_BINARY_AND,
    EXPRESSION_BINARY_XOR,
    EXPRESSION_BINARY_OR,
    EXPRESSION_BINARY_LOGICAL_AND,
    EXPRESSION_BINARY_LOGICAL_OR,
    EXPRESSION_BINARY_ASSIGN,
    EXPRESSION_BINARY_TIMES_ASSIGN,
    EXPRESSION_BINARY_DIVIDE_ASSIGN,
    EXPRESSION_BINARY_MODULO_ASSIGN,
    EXPRESSION_BINARY_ADD_ASSIGN,
    EXPRESSION_BINARY_SUBTRACT_ASSIGN,
    EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN,
    EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN,
    EXPRESSION_BINARY_AND_ASSIGN,
    EXPRESSION_BINARY_XOR_ASSIGN,
    EXPRESSION_BINARY_OR_ASSIGN,

    EXPRESSION_CONDITIONAL,
    EXPRESSION_COMMA // Here we just use the binary expression...
} ExpressionType;

typedef union Expression Expression;

typedef struct ExpressionBase {
    ExpressionType kind;
    Location loc;
    Type* type;

    bool is_parenthesized;
} ExpressionBase;

// Our primary expressions
typedef struct ExpressionReference {
    ExpressionBase base;
    String name;
} ExpressionReference;

typedef struct ExpressionInteger {
    ExpressionBase base;
    
} ExpressionInteger;

typedef struct ExpressionFloat ExpressionFloat;

typedef struct ExpressionEnumeration {
    ExpressionBase base;
} ExpressionEnumeration;

// 6.4.4.4 (Characters have type int)
typedef struct ExpressionCharacter {
    ExpressionBase base;
    int value;
    bool multibyte;
} ExpressionCharacter;

typedef struct ExpressionStringLiteral ExpressionStringLiteral;

typedef struct ExpressionArrayAccess ExpressionArrayAccess;
typedef struct ExpressionFunctionCall ExpressionFunctionCall;

typedef struct ExpressionMemberAccess {
    ExpressionBase base;
    Expression* lhs;
    String member;
    Location member_loc;
} ExpressionMemberAccess;

typedef struct ExpressionCompoundLiteral ExpressionCompoundLiteral;
typedef struct ExpressionSizeOf ExpressionSizeOf;
typedef struct ExpressionCast ExpressionCast;

typedef struct ExpressionUnary {
    ExpressionBase base;
    Expression* lhs;
} ExpressionUnary;

typedef struct ExpressionBinary {
    ExpressionBase base;
    Expression* lhs;
    Expression* rhs;
} ExpressionBinary;

typedef struct ExpressionConditional {
    ExpressionBase base;
    Expression* condition;
    Expression* true_part;
    Expression* false_part;
} ExpressionConditional;

union Expression {
    ExpressionBase base;

    ExpressionInteger integer;

    ExpressionCharacter character;

    ExpressionUnary unary;
    ExpressionBinary binary;
    ExpressionConditional conditional;
};

Expression* expression_unary(Expression* expr, ExpressionType type, Location loc);
Expression* expression_binary(Expression* lhs, Expression* rhs, ExpressionType type, Location loc);

#endif /* EXPRESSION_H */
