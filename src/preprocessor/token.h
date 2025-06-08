#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdbool.h>

#include "util/static_string.h"

#include "preprocessor/location.h"

// Our token types here
// Note that we use the pp definitions of tokens mainly which are then converted
// on demand by the parser. E.g. if its a number it gets converted, and properly
// checked there, and e.g. string literals are concatenated by parser
enum TokenType {
    TOKEN_UNKNOWN = -1,
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
    TOKEN_BOOL,
    TOKEN_COMPLEX,
    TOKEN_IMAGINARY,

    // Special tokens begin here
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_CHARACTER,
    TOKEN_STRING,

    // Special preprocessing token names
    // TOKEN_DEFINE,
    // TOKEN_UNDEF,
    // TOKEN_INCLUDE,
    // /* TOKEN_IF, */
    // TOKEN_IFDEF,
    // TOKEN_IFNDEF,
    // /* TOKEN_ELSE, */
    // TOKEN_ELIF,
    // TOKEN_ENDIF,
    // TOKEN_LINE,
    // TOKEN_ERROR,
    // TOKEN_PRAGMA,
    
    TOKEN_HEADER_NAME,
    TOKEN_MACRO_PARAMATER,
    TOKEN_NEWLINE,
};
typedef enum TokenType TokenType;

// TODO: maybe turn opt value into a pointer if we want to save space???
struct Token {
    TokenType type; // the type of token
    Location loc; // location id (which includes alot more info)

    StaticString opt_value; // optional value for specific tokens if needed

    // Some flags for the token
    bool is_space_before; // was space before token
    bool is_expansion_forbidden; // are we allowed to expand token?
};
typedef struct Token Token;

struct TokenList {
    Token* tokens;
    size_t used;
    size_t allocated;
};
typedef struct TokenList TokenList;

void token_freshen_up(Token* tok);
void token_free(Token* tok);

const char* token_type_get_name(TokenType type);

const char* token_get_name(Token* tok);
const char* token_get_string(Token* tok);

size_t token_get_length(Token* tok);

bool token_equal_string(Token* tok, const char* str);
bool token_equal_token(Token* tok1, Token* tok2);

bool token_concatenate(Token* tok1, Token* tok2, Token* dest);

bool token_stringize(Token* src, Token* dest);
bool token_list_stringize(TokenList* token, Token* dest);

bool token_string_cat(Token* tok1, Token* tok2, Token* dest);
bool token_list_string_cat(TokenList* list, Token* dest);

// Perhaps some stuff here for concatenations and other things

#endif /* TOKEN_H */
