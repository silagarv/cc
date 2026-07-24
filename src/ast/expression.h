#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include <stdbool.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "ast/ast_fwd.h"
#include "ast/type.h"
#include "ast/value.h"
#include "parse/ast_allocator.h"

typedef enum ExpressionType {
    EXPR_ERROR,

    EXPRESSION_PARENTHESISED,
    
    EXPRESSION_REFERENCE,
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
    EXPRESSION_CAST_IMPLICIT,

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
    EXPRESSION_BINARY_COMMA,

    EXPRESSION_CONDITIONAL,

    EXPRESSION_ARRAY_DECAY, // Array decay to pointer
    EXPRESSION_LVALUE_CAST // lvalue to rvalue implicit case
} ExpressionType;

typedef struct ExpressionBase {
    ExpressionType kind;
    QualifiedType type;
    bool poisoned;
} ExpressionBase;

ExpressionType expression_kind(const Expression* expr);
QualifiedType expression_type(const Expression* expr);
bool expression_valid(const Expression* expr);
bool expression_invalid(const Expression* expr);

bool expression_is(const Expression* expr, ExpressionType kind);

typedef struct ExpressionParenthesised {
    ExpressionBase base;
    Location lparen;
    Location rparen;
    Expression* inner;
} ExpressionParenthesised;

Expression* expression_create_parenthesised(AstAllocator* ast, Location lparen,
        Location rparen, Expression* inner);
Location expression_parenthesised_lparen(const Expression* expr);
Location expression_parenthesised_rparen(const Expression* expr);
Expression* expression_parenthesised_inner(const Expression* expr);

typedef struct ExpressionReference {
    ExpressionBase base;
    Identifier* ident;
    Location location;
    Declaration* decl;
} ExpressionReference;

Expression* expression_create_reference(AstAllocator* ast, Location location,
        Identifier* ident, Declaration* decl);
Location expression_reference_location(const Expression* expr);
Identifier* expression_reference_identifier(const Expression* expr);
Declaration* expression_reference_declaration(const Expression* expr);

typedef struct ExpressionInteger {
    ExpressionBase base;
    Location location;
    IntegerValue value;
} ExpressionInteger;

Expression* expression_create_integer(AstAllocator* ast, Location location,
        IntegerValue value);
Location expression_integer_location(const Expression* expr);
IntegerValue expression_integer_value(const Expression* expr);

typedef struct ExpressionFloating {
    ExpressionBase base;
    Location location;
    FloatingValue value;
} ExpressionFloating;

Expression* expression_create_floating(AstAllocator* ast, Location location,
        FloatingValue value);
Location expression_floating_location(const Expression* expr);
FloatingValue expression_floating_value(const Expression* expr);

typedef struct ExpressionCharacter {
    ExpressionBase base;
    CharValue value;
    Location location;
} ExpressionCharacter;

Expression* expression_create_character(AstAllocator* ast, Location location,
        CharValue value);
Location expression_character_location(const Expression* expr);
CharValue expression_character_value(const Expression* expr);

typedef struct ExpressionStringLiteral {
    ExpressionBase base;
    Location location; // Also have a location* for all of the string literals?
    StringLiteral value;
} ExpressionStringLiteral;

Expression* expression_create_string(AstAllocator* ast, Location location,
        StringLiteral value);
Location expression_string_location(const Expression* expr);
StringLiteral expression_string_value(const Expression* expr);

typedef struct ExpressionArrayAccess {
    ExpressionBase base;
    Location lbracket;
    Location rbracket;
    Expression* lhs;
    Expression* rhs;
} ExpressionArrayAccess;

typedef struct ExpressionFunctionCall {
    ExpressionBase base;
    Location lparen;
    Location rparen;
    Expression* lhs;
    Expression** arguments;
    size_t num_arguments;
} ExpressionFunctionCall;

typedef struct ExpressionMemberAccess {
    ExpressionBase base;
    Location location;
    Expression* lhs;
    Declaration* member;
    bool is_arrow;
} ExpressionMemberAccess;

typedef struct ExpressionCompoundLiteral {
    ExpressionBase base;
    Location lparen;
    Location rparen;
    QualifiedType type;
    Initializer* init;
} ExpressionCompoundLiteral;

typedef struct ExpressionSizeof {
    ExpressionBase base;
    Location sizeof_loc;
    Location lparen;
    Location rparen;
    union {
        QualifiedType target_type;
        Expression* expr;
    } inner;
} ExpressionSizeof;

typedef struct ExpressionCast {
    ExpressionBase base;
    Location lparen;
    Location rparen;
    Expression* inner;
} ExpressionCast;

typedef struct ExpressionUnary {
    ExpressionBase base;
    Location operator;
    Expression* expr;
} ExpressionUnary;

typedef struct ExpressionBinary {
    ExpressionBase base;
    Location operator;
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

typedef struct ExpressionError {
    ExpressionBase base;
    Location location;
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

    ExpressionSizeof sizeof_expr;
    ExpressionCast cast;

    ExpressionUnary unary;
    ExpressionBinary binary;
    ExpressionConditional conditional;

    ExpressionError error;
};

#endif /* AST_EXPRESSION_H */
