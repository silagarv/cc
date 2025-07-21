#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdbool.h>

#include "util/str.h"

#include "lex/location.h"

// Our token types here
// Note that we use the pp definitions of tokens mainly which are then converted
// on demand by the parser. E.g. if its a number it gets converted, and properly
// checked there, and e.g. string literals are concatenated by parser
typedef enum TokenType {
    TOKEN_UNKNOWN = 0,
    TOKEN_EOF,

    // Punctuators
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

    // Keyword tokens
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
    TOKEN__BOOL,
    TOKEN__COMPLEX,
    TOKEN__IMAGINARY,

    TOKEN___FUNC__, // __func__

    // Special tokens begin here
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_CHARACTER,
    TOKEN_WIDE_CHARACTER,
    TOKEN_STRING,
    TOKEN_WIDE_STRING,

    // Special preprocessing token names
    TOKEN_PP_DEFINE,
    TOKEN_PP_UNDEF,
    TOKEN_PP_INCLUDE,
    TOKEN_PP_IF,
    TOKEN_PP_IFDEF,
    TOKEN_PP_IFNDEF,
    TOKEN_PP_ELSE,
    TOKEN_PP_ELIF,
    TOKEN_PP_ENDIF,
    TOKEN_PP_LINE,
    TOKEN_PP_ERROR,
    TOKEN_PP_PRAGMA,
    
    TOKEN_PP_HEADER_NAME,
    TOKEN_PP_MACRO_PARAMATER,

    TOKEN_PP_EOD,

    /* the last token to get the number of tokens */
    TOKEN_LAST
} TokenType;

// TODO: eventually I would Like to use this structure within the token
typedef struct IdentifierNode {
    String value;
    uint32_t hash;
} IdentifierNode;

typedef struct LiteralNode {
    String value;
} LiteralNode;

typedef union TokenData {
    IdentifierNode* identifier;
    LiteralNode* literal;
} TokenData;

// TODO: goal token structure. Nice an compact enough strucutre with alot of
// information about the token and it's contents
// typedef struct Token {
//     Location start; // the starting character in the token
//     Location end; // the ending character within the token

//     TokenType type; // The type of token
    
//     bool start_of_line; // it the token at the start of  a line
//     bool leading_space; // does the token have at least 1 leading whitepsace
//     bool disable_expand; // should the token not be expanded

//     TokenData data; // data the token needs
// } Token;

typedef struct Token {
    TokenType type;
    Location loc;

    String opt_value;
    
    bool leading_space;
    bool start_of_line;
    bool disable_expand;
} Token;

typedef struct TokenList {
    Token* tokens;
    size_t used;
    size_t allocated;
} TokenList;

typedef struct TokenStream {
    Token* tokens;
    size_t count;

    size_t current_token;
} TokenStream;

TokenData token_create_identifier_node(String* string);
TokenData token_create_literal_node(String* string);

void token_free_data(Token* tok);
void token_free(Token* tok);

const char* token_type_get_name(TokenType type);

bool token_has_opt_value(Token* tok);
const char* token_get_name(Token* tok);
const char* token_get_string(Token* tok);

size_t token_get_length(Token* tok);

bool token_equal_string(Token* tok, const char* str);
bool token_equal_token(Token* tok1, Token* tok2);

TokenList token_list_allocate(void);
void token_list_free(TokenList* list);

Token* token_list_next(TokenList* list);

TokenStream token_list_to_stream(const TokenList* list);

#endif /* TOKEN_H */
