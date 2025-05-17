#include "token.h"

#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include "core/panic.h"


const char* token_get_name(Token* tok)
{
    switch (tok->type)
    {
        case TOKEN_EOF: return "end-of-file";
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
        case TOKEN_IDENTIFIER: return "identifier";
        case TOKEN_NUMBER: return "preprocessing number";
        case TOKEN_CHARACTER: return "character constant";
        case TOKEN_STRING: return "string literal";
        case TOKEN_HEADER_NAME: return "<<INTERNAL_HEADER_NAME>>"; 
        case TOKEN_NEWLINE: return "newline"; 
        case TOKEN_PARAM: return "<<INTERNAL_MACRO_PARAM>>"; 
        case TOKEN_UNKNOWN: return "unknown";
    }

    panic("invalid token type in token_get_name");
    return NULL; // appease compiler
}

bool is_token_str_equal(Token* tok, const char* str)
{
    const size_t str_len = strlen(str);
    if (tok->len == str_len && !strncmp(tok->start, str, str_len))
    {
        return true;
    }

    return false;
}

bool is_token_equal(Token* tok1, Token* tok2)
{
    panic("unimplemented is_token_equal");
    return false;
}

bool is_token_keyword(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
        case TOKEN_LBRACKET:
        case TOKEN_RBRACKET:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LCURLY:
        case TOKEN_RCURLY:
        case TOKEN_DOT:
        case TOKEN_ARROW:
        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS:
        case TOKEN_AND:
        case TOKEN_STAR:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_TILDE:
        case TOKEN_NOT:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
        case TOKEN_LT_LT:
        case TOKEN_GT_GT:
        case TOKEN_LT:
        case TOKEN_GT:
        case TOKEN_LT_EQUAL:
        case TOKEN_GT_EQUAL:
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_NOT_EQUAL:
        case TOKEN_XOR:
        case TOKEN_OR:
        case TOKEN_AND_AND:
        case TOKEN_OR_OR:
        case TOKEN_QUESTION:
        case TOKEN_COLON:
        case TOKEN_SEMI:
        case TOKEN_ELIPSIS:
        case TOKEN_EQUAL:
        case TOKEN_STAR_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_LT_LT_EQUAL:
        case TOKEN_GT_GT_EQUAL:
        case TOKEN_AND_EQUAL:
        case TOKEN_XOR_EQUAL:
        case TOKEN_OR_EQUAL:
        case TOKEN_COMMA:
        case TOKEN_HASH:
        case TOKEN_HASH_HASH:
        case TOKEN_LT_COLON:
        case TOKEN_COLON_GT:
        case TOKEN_LT_PERCENT:
        case TOKEN_PERCENT_GT:
        case TOKEN_PERCENT_COLON:
        case TOKEN_PERCENT_COLON_PERCENT_COLON:
        case TOKEN_IDENTIFIER:
        case TOKEN_NUMBER:
        case TOKEN_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_HEADER_NAME: 
        case TOKEN_NEWLINE: 
        case TOKEN_PARAM: 
        case TOKEN_UNKNOWN:
            return false;

        case TOKEN_AUTO:
        case TOKEN_BREAK:
        case TOKEN_CASE:
        case TOKEN_CHAR:
        case TOKEN_CONST:
        case TOKEN_CONTINUE:
        case TOKEN_DEFAULT:
        case TOKEN_DO:
        case TOKEN_DOUBLE:
        case TOKEN_ELSE:
        case TOKEN_ENUM:
        case TOKEN_EXTERN:
        case TOKEN_FLOAT:
        case TOKEN_FOR:
        case TOKEN_GOTO:
        case TOKEN_IF:
        case TOKEN_INLINE:
        case TOKEN_INT:
        case TOKEN_LONG:
        case TOKEN_REGISTER:
        case TOKEN_RESTRICT:
        case TOKEN_RETURN:
        case TOKEN_SHORT:
        case TOKEN_SIGNED:
        case TOKEN_SIZEOF:
        case TOKEN_STATIC:
        case TOKEN_STRUCT:
        case TOKEN_SWITCH:
        case TOKEN_TYPEDEF:
        case TOKEN_UNION:
        case TOKEN_UNSIGNED:
        case TOKEN_VOID:
        case TOKEN_VOLATILE:
        case TOKEN_WHILE:
        case TOKEN_BOOL:
        case TOKEN_COMPLEX:
        case TOKEN_IMAGINARY:
            return true;
    }

    panic("unknown token type");
    return false; // appease compiler
}

bool is_token_identifier(Token* tok)
{
    if (is_token_keyword(tok) || tok->type == TOKEN_IDENTIFIER)
    {
        return true;
    }

    return false;
}

bool is_token_punctuator(Token* tok)
{
    switch (tok->type)
    {   
        case TOKEN_EOF:
        case TOKEN_IDENTIFIER:
        case TOKEN_NUMBER:
        case TOKEN_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_HEADER_NAME: 
        case TOKEN_NEWLINE: 
        case TOKEN_PARAM: 
        case TOKEN_UNKNOWN:
        case TOKEN_AUTO:
        case TOKEN_BREAK:
        case TOKEN_CASE:
        case TOKEN_CHAR:
        case TOKEN_CONST:
        case TOKEN_CONTINUE:
        case TOKEN_DEFAULT:
        case TOKEN_DO:
        case TOKEN_DOUBLE:
        case TOKEN_ELSE:
        case TOKEN_ENUM:
        case TOKEN_EXTERN:
        case TOKEN_FLOAT:
        case TOKEN_FOR:
        case TOKEN_GOTO:
        case TOKEN_IF:
        case TOKEN_INLINE:
        case TOKEN_INT:
        case TOKEN_LONG:
        case TOKEN_REGISTER:
        case TOKEN_RESTRICT:
        case TOKEN_RETURN:
        case TOKEN_SHORT:
        case TOKEN_SIGNED:
        case TOKEN_SIZEOF:
        case TOKEN_STATIC:
        case TOKEN_STRUCT:
        case TOKEN_SWITCH:
        case TOKEN_TYPEDEF:
        case TOKEN_UNION:
        case TOKEN_UNSIGNED:
        case TOKEN_VOID:
        case TOKEN_VOLATILE:
        case TOKEN_WHILE:
        case TOKEN_BOOL:
        case TOKEN_COMPLEX:
        case TOKEN_IMAGINARY:
            return false;

        case TOKEN_LBRACKET:
        case TOKEN_RBRACKET:
        case TOKEN_LPAREN:
        case TOKEN_RPAREN:
        case TOKEN_LCURLY:
        case TOKEN_RCURLY:
        case TOKEN_DOT:
        case TOKEN_ARROW:
        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS:
        case TOKEN_AND:
        case TOKEN_STAR:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_TILDE:
        case TOKEN_NOT:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
        case TOKEN_LT_LT:
        case TOKEN_GT_GT:
        case TOKEN_LT:
        case TOKEN_GT:
        case TOKEN_LT_EQUAL:
        case TOKEN_GT_EQUAL:
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_NOT_EQUAL:
        case TOKEN_XOR:
        case TOKEN_OR:
        case TOKEN_AND_AND:
        case TOKEN_OR_OR:
        case TOKEN_QUESTION:
        case TOKEN_COLON:
        case TOKEN_SEMI:
        case TOKEN_ELIPSIS:
        case TOKEN_EQUAL:
        case TOKEN_STAR_EQUAL:
        case TOKEN_SLASH_EQUAL:
        case TOKEN_PERCENT_EQUAL:
        case TOKEN_PLUS_EQUAL:
        case TOKEN_MINUS_EQUAL:
        case TOKEN_LT_LT_EQUAL:
        case TOKEN_GT_GT_EQUAL:
        case TOKEN_AND_EQUAL:
        case TOKEN_XOR_EQUAL:
        case TOKEN_OR_EQUAL:
        case TOKEN_COMMA:
        case TOKEN_HASH:
        case TOKEN_HASH_HASH:
        case TOKEN_LT_COLON:
        case TOKEN_COLON_GT:
        case TOKEN_LT_PERCENT:
        case TOKEN_PERCENT_GT:
        case TOKEN_PERCENT_COLON:
        case TOKEN_PERCENT_COLON_PERCENT_COLON:
            return true;
    }

    panic("unknown token type");
    return false; // appease compiler
}

bool is_token_defineable(Token* tok)
{   
    // Can't define something that isnt an identifier
    if (!is_token_identifier(tok))
    {
        return false;
    }
    
    // check if the strings are equal 
    if (is_token_str_equal(tok, "defined"))
    {   
        return false;
    }

    // we have an identifier and the token is not equal to "defined"
    return true;
}

bool is_token_type(Token* tok, TokenType type)
{
    return (tok->type == type) ? true : false;
}
