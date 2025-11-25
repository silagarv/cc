#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "files/location.h"

#include "lex/token.h"
#include "lex/identifier_table.h"

#include "parse/ast_allocator.h"
#include "parse/type.h"
#include "parse/literal_parser.h"

union Initializer;

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
    // The expression type
    ExpressionType kind;

    // The type of expression that we have
    QualifiedType type;

    // Is this an expression that should not be further semantically checked?
    bool poisoned;
} ExpressionBase;

// Our primary expressions
typedef struct ExpressionReference {
    ExpressionBase base;

    // The identifier this refers to and the location we saw it at
    Identifier* identifier;
    Location identifier_loc;
    
    // The declaration that this expression refers too
    union Declaration* declaration;
} ExpressionReference;

typedef struct ExpressionInteger {
    ExpressionBase base;
    Location num_location;
    IntegerValue value;
} ExpressionInteger;

typedef struct ExpressionFloating {
    ExpressionBase base;
    Location num_location;
    FloatingValue value;
} ExpressionFloating;

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
    bool lhs_is_array; // since c has 0[a] and a[0] being allowed
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
typedef struct ExpressionCompoundLiteral {
    ExpressionBase base;
    Location lparen_loc;
    Location rparen_loc;
    Location lcurly_loc;
    Location rcurly_loc;
    QualifiedType type;
    union Initializer* initializer;
} ExpressionCompoundLiteral;

typedef struct ExpressionSizeofType {
    ExpressionBase base;
    Location sizeof_loc;
    Location lparen_loc;
    Location rparen_loc;
    QualifiedType target_type;
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
    QualifiedType type;
    Expression* rhs;
    bool implicit;
} ExpressionCast;

typedef struct ExpressionUnary {
    ExpressionBase base;
    Location op_loc;
    Expression* rhs;
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

    ExpressionParenthesised parenthesised;
    ExpressionReference reference;
    ExpressionInteger integer;
    ExpressionFloating floating;
    ExpressionCharacter character;
    ExpressionStringLiteral string;

    ExpressionArrayAccess array;
    ExpressionFunctionCall call;
    ExpressionMemberAccess access;
    ExpressionCompoundLiteral compound_literal;

    ExpressionSizeofExpression sizeof_expression;
    ExpressionSizeofType sizeof_type;
    
    ExpressionCast cast;

    ExpressionUnary unary;
    ExpressionBinary binary;
    ExpressionConditional conditional;

    ExpressionError error;
};

bool expression_is(const Expression* expr, ExpressionType type);
Location expression_get_location(const Expression* expr);

// Get the type of the expression
ExpressionType expression_get_kind(const Expression* expr);
bool expression_is_invalid(const Expression* expr);
void expression_set_invalid(Expression* expr);
QualifiedType expression_get_qualified_type(const Expression* expr);

Expression* expression_create_error(AstAllocator* allocator,
        Type* error_type);

Expression* expression_create_parenthesised(AstAllocator* allocator,
        Location lparen_loc, Location rparen_loc, Expression* inside);
Expression* expression_parenthesised_get_inner(const Expression* expr);

Expression* expression_create_reference(AstAllocator* allocator,
        Identifier* identifier, Location location,
        union Declaration* declaration, QualifiedType expr_type);
    
Expression* expression_create_integer(AstAllocator* allocator,
        Location location, IntegerValue value, QualifiedType type);
    
Expression* expression_create_float(AstAllocator* allocator, Location location,
        FloatingValue value, QualifiedType type);

Expression* expression_create_character(AstAllocator* allocator,
        Location location, CharValue value, QualifiedType expr_type);

Expression* expression_create_array(AstAllocator* allocator, 
        Location lbracket_loc, Location rbracket_loc, Expression* lhs,
        Expression* member, QualifiedType expr_type, bool lhs_is_array);

Expression* expression_create_unary(AstAllocator* allocator, 
        ExpressionType type, Location op_loc, Expression* expression,
        QualifiedType expr_type);

Expression* expression_create_binary(AstAllocator* allocator, 
        ExpressionType type, Location op_loc, Expression* lhs, Expression* rhs,
        QualifiedType expr_type);

// TODO: somehow we will need to be able to fold expressions...
// TODO: so we will need to set up some stuff here to do that. This will also
// TODO: be useful for any preprocessor work that we have to do.

#endif /* EXPRESSION_H */
