#include "token.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "util/panic.h"
#include "util/static_string.h"

#include "preprocessor/location.h"

void token_freshen_up(Token* tok)
{
    *tok = (Token) {0};
    tok->loc = LOCATION_INVALID;
    tok->type = TOKEN_UNKNOWN;
}

static bool token_has_opt_value(Token* tok)
{
    switch (tok->type)
    {
        /* intentional fallthrough all below here */
        case TOKEN_UNKNOWN:
        case TOKEN_IDENTIFIER:
        case TOKEN_NUMBER:
        case TOKEN_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_HEADER_NAME:
        case TOKEN_MACRO_PARAMATER:
            return true;

        default:
            return false;
    }
}

void token_free(Token* tok)
{
    if (token_has_opt_value(tok))
    {
        static_string_free(&tok->opt_value);
    }
    
    // otherwise there is nothing else to do since tok itself is stored elsewhre
}

const char* token_type_get_name(TokenType type)
{
    // DO NOT ADD DEFAULT CASE HERE to trigger -Wswitch if anything added
    switch(type)
    {
        case TOKEN_UNKNOWN: return "<unknown-token>";
        case TOKEN_EOF: return "<end-of-file>";
        case TOKEN_LBRACKET: return "[";
        case TOKEN_RBRACKET: return "]";
        case TOKEN_LPAREN: return "(";
        case TOKEN_RPAREN: return ")";
        case TOKEN_LCURLY: return "{";
        case TOKEN_RCURLY: return "}";
        case TOKEN_DOT: return ".";
        case TOKEN_ARROW: return "->";
        case TOKEN_PLUS_PLUS: return "++";
        case TOKEN_MINUS_MINUS: return "--";
        case TOKEN_AND: return "&";
        case TOKEN_STAR: return "*";
        case TOKEN_PLUS: return "+";
        case TOKEN_MINUS: return "-";
        case TOKEN_TILDE: return "~";
        case TOKEN_NOT: return "!";
        case TOKEN_SLASH: return "/";
        case TOKEN_PERCENT: return "%";
        case TOKEN_LT_LT: return "<<";
        case TOKEN_GT_GT: return ">>";
        case TOKEN_LT: return "<";
        case TOKEN_GT: return ">";
        case TOKEN_LT_EQUAL: return "<=";
        case TOKEN_GT_EQUAL: return ">=";
        case TOKEN_EQUAL_EQUAL: return "==";
        case TOKEN_NOT_EQUAL: return "!=";
        case TOKEN_XOR: return "^";
        case TOKEN_OR: return "|";
        case TOKEN_AND_AND: return "&&";
        case TOKEN_OR_OR: return "||";
        case TOKEN_QUESTION: return "?";
        case TOKEN_COLON: return ":";
        case TOKEN_SEMI: return ";";
        case TOKEN_ELIPSIS: return "...";
        case TOKEN_EQUAL: return "=";
        case TOKEN_STAR_EQUAL: return "*=";
        case TOKEN_SLASH_EQUAL: return "/=";
        case TOKEN_PERCENT_EQUAL: return "%=";
        case TOKEN_PLUS_EQUAL: return "+=";
        case TOKEN_MINUS_EQUAL: return "-=";
        case TOKEN_LT_LT_EQUAL: return "<<=";
        case TOKEN_GT_GT_EQUAL: return ">>=";
        case TOKEN_AND_EQUAL: return "&=";
        case TOKEN_XOR_EQUAL: return "^=";
        case TOKEN_OR_EQUAL: return "|=";
        case TOKEN_COMMA: return ",";
        case TOKEN_HASH: return "#";
        case TOKEN_HASH_HASH: return "##";
        case TOKEN_LT_COLON: return "<:";
        case TOKEN_COLON_GT: return ":>";
        case TOKEN_LT_PERCENT: return "<%";
        case TOKEN_PERCENT_GT: return "%>";
        case TOKEN_PERCENT_COLON: return "%:";
        case TOKEN_PERCENT_COLON_PERCENT_COLON: return "%:%:";
        case TOKEN_AUTO: return "auto";
        case TOKEN_BREAK: return "break";
        case TOKEN_CASE: return "case";
        case TOKEN_CHAR: return "char";
        case TOKEN_CONST: return "const";
        case TOKEN_CONTINUE: return "continue";
        case TOKEN_DEFAULT: return "default";
        case TOKEN_DO: return "do";
        case TOKEN_DOUBLE: return "double";
        case TOKEN_ELSE: return "else";
        case TOKEN_ENUM: return "enum";
        case TOKEN_EXTERN: return "extern";
        case TOKEN_FLOAT: return "float";
        case TOKEN_FOR: return "for";
        case TOKEN_GOTO: return "goto";
        case TOKEN_IF: return "if";
        case TOKEN_INLINE: return "inline";
        case TOKEN_INT: return "int";
        case TOKEN_LONG: return "long";
        case TOKEN_REGISTER: return "register";
        case TOKEN_RESTRICT: return "restrict";
        case TOKEN_RETURN: return "return";
        case TOKEN_SHORT: return "short";
        case TOKEN_SIGNED: return "signed";
        case TOKEN_SIZEOF: return "sizeof";
        case TOKEN_STATIC: return "static";
        case TOKEN_STRUCT: return "struct";
        case TOKEN_SWITCH: return "switch";
        case TOKEN_TYPEDEF: return "typedef";
        case TOKEN_UNION: return "union";
        case TOKEN_UNSIGNED: return "unsigned";
        case TOKEN_VOID: return "void";
        case TOKEN_VOLATILE: return "volatile";
        case TOKEN_WHILE: return "while";
        case TOKEN_BOOL: return "_Bool";
        case TOKEN_COMPLEX: return "_Complex";
        case TOKEN_IMAGINARY: return "_Imaginary";
        case TOKEN_IDENTIFIER: return "<identifier>";
        case TOKEN_NUMBER: return "<preprocessing-number>";
        case TOKEN_CHARACTER: return "<character-constant>";
        case TOKEN_STRING: return "<string literal>";
        case TOKEN_HEADER_NAME: return "<<INTERNAL_HEADER_NAME>>"; 
        case TOKEN_MACRO_PARAMATER: return "<<INTERNAL_MACRO_PARAM>>"; 
        case TOKEN_NEWLINE: return "<newline-token>";
    }

    panic("unable to get token type in token_get_name");
    return ""; // quiet -fanalyzer warning since it must be non-null
}

const char* token_get_name(Token* tok)
{
    return token_type_get_name(tok->type);
}
const char* token_get_string(Token* tok)
{   
    if (tok->type == TOKEN_NEWLINE)
    {
        return "\n"; /* special case here */
    }

    if (tok->type == TOKEN_EOF)
    {
        return ""; /* second special case*/
    }

    if (!token_has_opt_value(tok))
    {
        return token_get_name(tok);
    }

    return static_string_get_ptr(&tok->opt_value);
}

size_t token_get_length(Token* tok)
{
    // Bit of a cheat here
    // TODO: maybe turn into switch??? or maybe not??
    return strlen(token_get_string(tok));
}

bool token_equal_string(Token* tok, const char* str)
{
    const size_t str_length = strlen(str);
    const size_t token_len = token_get_length(tok);

    if (str_length != token_len)
    {
        return false;
    }

    const char* token_string = token_get_string(tok);

    return (strncmp(str, token_string, str_length) == 0);
}

bool token_equal_token(Token* tok1, Token* tok2)
{
    const bool same_type = (tok1->type == tok2->type);
    if (same_type && !token_has_opt_value(tok1))
    {
        return true;
    }
    else if (same_type /*&& token_has_opt_value(tok1)*/)
    {
        const size_t tok1_length = token_get_length(tok1);
        const size_t tok2_length = token_get_length(tok2);

        // check token lengths equal
        if (tok1_length != tok2_length)
        {
            return false;
        }

        const char* tok1_str = token_get_string(tok1);
        const char* tok2_str = token_get_string(tok2);

        if (!strncmp(tok1_str, tok2_str, tok1_length))
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    return false;
}

static bool can_tokens_concatenate(Token* tok1, Token* tok2);

bool token_concatenate(Token* tok1, Token* tok2, Token* dest);

bool token_stringize(Token* src, Token* dest);
bool token_list_stringize(TokenList* token, Token* dest);

bool token_string_cat(Token* tok1, Token* tok2, Token* dest);
bool token_list_string_cat(TokenList* list, Token* dest);
