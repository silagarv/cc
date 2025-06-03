#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdbool.h>

// Our token types here
// Note that we use the pp definitions of tokens mainly which are then converted
// on demand by the parser. E.g. if its a number it gets converted, and properly
// checked there, and e.g. string literals are concatenated by parser
enum TokenType {
    TOKEN_UNKNOWN,

    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_DOT,
    TOKEN_ARROW,
    TOKEN_PLUS_PLUS,
    TOKEN_MINUS_MINUS,
    TOKEN_AND,
    TOKEN_STAR,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_TILDE,
    TOKEN_NOT,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_LT_LT,
    TOKEN_GT_GT,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LT_EQUAL,
    TOKEN_GT_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_NOT_EQUAL,
    TOKEN_XOR,
    TOKEN_OR,
    TOKEN_AND_AND,
    TOKEN_OR_OR,
    TOKEN_QUESTION,
    TOKEN_COLON,
    TOKEN_SEMI,
    TOKEN_ELIPSIS,
    TOKEN_EQUAL,
    TOKEN_STAR_EQUAL,
    TOKEN_SLASH_EQUAL,
    TOKEN_PERCENT_EQUAL,
    TOKEN_PLUS_EQUAL,
    TOKEN_MINUS_EQUAL,
    TOKEN_LT_LT_EQUAL,
    TOKEN_GT_GT_EQUAL,
    TOKEN_AND_EQUAL,
    TOKEN_XOR_EQUAL,
    TOKEN_OR_EQUAL,
    TOKEN_COMMA,
    TOKEN_HASH,
    TOKEN_HASH_HASH,
    TOKEN_LT_COLON,
    TOKEN_COLON_GT,
    TOKEN_LT_PERCENT,
    TOKEN_PERCENT_GT,
    TOKEN_PERCENT_COLON,
    TOKEN_PERCENT_COLON_PERCENT_COLON,

    TOKEN_AUTO,
    TOKEN_BREAK,
    TOKEN_CASE,
    TOKEN_CHAR,
    TOKEN_CONST,
    TOKEN_CONTINUE,
    TOKEN_DEFAULT,
    TOKEN_DO,
    TOKEN_DOUBLE,
    TOKEN_ELSE,
    TOKEN_ENUM,
    TOKEN_EXTERN,
    TOKEN_FLOAT,
    TOKEN_FOR,
    TOKEN_GOTO,
    TOKEN_IF,
    TOKEN_INLINE,
    TOKEN_INT,
    TOKEN_LONG,
    TOKEN_REGISTER,
    TOKEN_RESTRICT,
    TOKEN_RETURN,
    TOKEN_SHORT,
    TOKEN_SIGNED,
    TOKEN_SIZEOF,
    TOKEN_STATIC,
    TOKEN_STRUCT,
    TOKEN_SWITCH,
    TOKEN_TYPEDEF,
    TOKEN_UNION,
    TOKEN_UNSIGNED,
    TOKEN_VOID,
    TOKEN_VOLATILE,
    TOKEN_WHILE,
    TOKEN_BOOL,
    TOKEN_COMPLEX,
    TOKEN_IMAGINARY,

    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_CHARACTER,
    TOKEN_STRING,

    TOKEN_HEADER_NAME,
    TOKEN_NEWLINE,

    TOKEN_EOF
};
typedef enum TokenType TokenType;

struct Token {
    TokenType type;

    
};
typedef struct Token Token;

#endif /* TOKEN_H */
