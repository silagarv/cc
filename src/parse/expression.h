#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "files/location.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

#include "parse/ast_allocator.h"
#include "parse/type.h"
#include "parse/literal_parser.h"

typedef enum ExpressionType {
    EXPRESSION_ERROR,

    EXPRESSION_REFERENCE, // an identifier in the symbol table

    EXPRESSION_ENUMERATION_CONSTANT,
    
    EXPRESSION_INTEGER_CONSTANT,    
    EXPRESSION_FLOATING_CONSTANT,
    EXPRESSION_CHARACTER_CONSTANT,
    EXPRESSION_STRING_LITERAL,

    EXPRESSION_ARRAY_ACCESS,
    EXPRESSION_FUNCTION_CALL,
    EXPRESSION_MEMBER_ACCESS,
    EXPRESSION_MEMBER_POINTER_ACCESS,
    EXPRESSION_COMPOUND_LITERAL,
    EXPRESSION_SIZEOF_TYPE,
    EXPRESSION_SIZEOF_EXPRESSION,
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
    EXPRESSION_COMMA, // Here we just use the binary expression...
    
    EXPRESSION_PARENTHESISED
} ExpressionType;

typedef union Expression Expression;

typedef struct ExpressionBase {
    ExpressionType kind;
    Type* type;
    LocationRange range;
} ExpressionBase;

// Our primary expressions
typedef struct ExpressionReference {
    ExpressionBase base;
    Location identifier_loc;
    Identifier* Identifier;
} ExpressionReference;

typedef struct ExpressionInteger {
    ExpressionBase base;
    Location num_location;
    IntegerValue value;
} ExpressionInteger;

typedef struct ExpressionFloat ExpressionFloat;

typedef struct ExpressionEnumeration {
    ExpressionBase base;
    Identifier* enumeration;
} ExpressionEnumeration;

// 6.4.4.4 (Characters have type int)
typedef struct ExpressionCharacter {
    ExpressionBase base;
    CharValue value;
} ExpressionCharacter;

typedef struct ExpressionStringLiteral {
    ExpressionBase base;
    StringLiteral value;
} ExpressionStringLiteral;

// A high level array access this will be converted into c's final form later
typedef struct ExpressionArrayAccess {
    ExpressionBase base;
    Location lbracket_loc;
    Location rbracket_loc;
    Expression* lhs;
    Expression* member;
} ExpressionArrayAccess;

typedef struct ExpressionFunctionCall {
    ExpressionBase base;
    Location lparen_loc;
    Location rparen_loc;
    Expression* lhs;
    Expression** arguments;
    size_t num_arguments;
} ExpressionFunctionCall;

typedef struct ExpressionMemberAccess {
    ExpressionBase base;
    Location location_op;
    Expression* lhs;
    Identifier* member;
    bool is_arrow;
} ExpressionMemberAccess;

// TODO: how to do this
typedef struct ExpressionCompoundLiteral ExpressionCompoundLiteral;

typedef struct ExpressionSizeofType {
    ExpressionBase base;
    Location sizeof_loc;
    Location lparen_loc;
    Location rparen_loc;
    Type* type;
} ExpressionSizeofType;

typedef struct ExpressionSizeofExpression {
    ExpressionBase base;
    Location sizeof_loc;
    Expression* expression;
} ExpressionSizeofExpression;

typedef struct ExpressionCast {
    ExpressionBase base;
    Location lparen_loc;
    Location rparen_loc;
    Type* type;
    Expression* rhs;
}ExpressionCast;

typedef struct ExpressionUnary {
    ExpressionBase base;
    Location op_loc;
    Expression* lhs;
} ExpressionUnary;

typedef struct ExpressionBinary {
    ExpressionBase base;
    Location op_loc;
    Expression* lhs;
    Expression* rhs;
} ExpressionBinary;

typedef struct ExpressionConditional {
    ExpressionBase base;
    Location question;
    Location colon;
    Expression* condition;
    Expression* true_part;
    Expression* false_part;
} ExpressionConditional;

typedef struct ExpressionParenthesised {
    ExpressionBase base;
    Location lparen_loc;
    Location rparen_loc;
    Expression* inside;
} ExpressionParenthesised;

typedef struct ExpressionError {
    ExpressionBase base;
} ExpressionError;

union Expression {
    ExpressionBase base;

    ExpressionInteger integer;
    ExpressionCharacter character;
    ExpressionStringLiteral string;

    ExpressionUnary unary;
    ExpressionBinary binary;
    ExpressionConditional conditional;

    ExpressionError error;
};

Expression* expression_create_error(AstAllocator* allocator,
        LocationRange range);



#endif /* EXPRESSION_H */
