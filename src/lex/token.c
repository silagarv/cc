#include "token.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "util/panic.h"
#include "util/xmalloc.h"
#include "util/str.h"
#include "util/hash.h"

#include "lex/identifier_table.h"

void token_set_flag(Token* token, TokenFlags flag)
{
    token->flags |= flag;
}

void token_unset_flag(Token* token, TokenFlags flag)
{
    token->flags &= ~flag;
}

bool token_has_flag(const Token* token, TokenFlags flag)
{
    return (token->flags & flag) != 0;
}

bool token_is_type(const Token* token, TokenType type)
{
    return token->type == type;
}

bool token_is_identifier(const Token* token)
{
    switch (token->type) 
    {
        // PP specific keywords but only in specific situations
        case TOKEN_PP_DEFINE:
        case TOKEN_PP_UNDEF:
        case TOKEN_PP_INCLUDE:
        case TOKEN_PP_IF:
        case TOKEN_PP_IFDEF:
        case TOKEN_PP_IFNDEF:
        case TOKEN_PP_ELSE:
        case TOKEN_PP_ELIF:
        case TOKEN_PP_ENDIF:
        case TOKEN_PP_LINE:
        case TOKEN_PP_ERROR:
        case TOKEN_PP_PRAGMA:

        // Keywords
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
        case TOKEN__BOOL:
        case TOKEN__COMPLEX:
        case TOKEN__IMAGINARY:
        case TOKEN___FUNC__:

        // Generic identifier
        case TOKEN_IDENTIFIER:
            return true;

        default:
            return false;
    }
}

bool token_is_literal(const Token* token)
{
    switch (token->type)
    {
        case TOKEN_NUMBER:
        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
        
        case TOKEN_PP_HEADER_NAME:
            return true;

        case TOKEN_UNKNOWN:
            return true; // SPECIAL CASE HERE

        default:
            return false;
    }
}

bool token_is_string(const Token* token)
{
    return (token->type == TOKEN_STRING || token->type == TOKEN_WIDE_STRING);
}

TokenData token_create_literal_node(String string)
{
    LiteralNode* node = xmalloc(sizeof(LiteralNode));
    *node = (LiteralNode)
    {
        .value = string,
    };

    TokenData data = { .literal = node };

    return data;   
}

bool token_has_opt_value(Token* tok)
{
    switch (tok->type)
    {
        /* intentional fallthrough all below here */
        case TOKEN_UNKNOWN:
        case TOKEN_IDENTIFIER:
        case TOKEN_NUMBER:
        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_WIDE_STRING:
        case TOKEN_PP_HEADER_NAME:
        case TOKEN_PP_MACRO_PARAMATER:
            return true;

        default:
            return false;
    }
}

void token_free_data(Token* token)
{
    if (token_is_literal(token))
    {
        LiteralNode* node = token->data.literal;

        string_free(&node->value);
        free(node);
    }
}

void token_free(Token* tok)
{
}

const char* token_type_get_name(TokenType type)
{
    // DO NOT ADD DEFAULT CASE HERE to trigger -Wswitch if anything added
    switch (type)
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
        case TOKEN__BOOL: return "_Bool";
        case TOKEN__COMPLEX: return "_Complex";
        case TOKEN__IMAGINARY: return "_Imaginary";
        case TOKEN___FUNC__: return "__func__";
        case TOKEN_IDENTIFIER: return "<identifier>";
        case TOKEN_NUMBER: return "<preprocessing-number>";
        case TOKEN_CHARACTER: return "<character-constant>";
        case TOKEN_WIDE_CHARACTER: return "<wide-character-constant>";
        case TOKEN_STRING: return "<string-literal>";
        case TOKEN_WIDE_STRING: return "<wide-string-literal>";
        case TOKEN_PP_HEADER_NAME: return "<<INTERNAL_HEADER_NAME>>"; 
        case TOKEN_PP_MACRO_PARAMATER: return "<<INTERNAL_MACRO_PARAM>>"; 
        case TOKEN_PP_EOD: return "<end-of-directive>";
        case TOKEN_LAST: return "<<INTERNAL_TOKEN_LAST>>";
        
        case TOKEN_PP_DEFINE: return "<<TOKEN_PP_DEFINE>>";
        case TOKEN_PP_UNDEF: return "<<TOKEN_PP_UNDEF>>";
        case TOKEN_PP_INCLUDE: return "<<TOKEN_PP_INCLUDE>>";
        case TOKEN_PP_IF: return "<<TOKEN_PP_IF>>";
        case TOKEN_PP_IFDEF: return "<<TOKEN_PP_IFDEF>>";
        case TOKEN_PP_IFNDEF: return "<<TOKEN_PP_IFNDEF>>";
        case TOKEN_PP_ELSE: return "<<TOKEN_PP_ELSE>>";
        case TOKEN_PP_ELIF: return "<<TOKEN_PP_ELIF>>";
        case TOKEN_PP_ENDIF: return "<<TOKEN_PP_ENDIF>>";
        case TOKEN_PP_LINE: return "<<TOKEN_PP_LINE>>";
        case TOKEN_PP_ERROR: return "<<TOKEN_PP_ERROR>>";
        case TOKEN_PP_PRAGMA: return "<<TOKEN_PP_PRAGMA>>";
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
    if (tok->type == TOKEN_PP_EOD)
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

    String* string;
    if (token_is_identifier(tok))
    {
        Identifier* node = tok->data.identifier;

        string = identifier_get_string(node);
    }
    else 
    {
        assert(token_is_literal(tok));

        LiteralNode* node = tok->data.literal;

        string = &node->value;
    }
    return string_get_ptr(string);
}

// Get the length of the token as represented within the source
size_t token_get_length(Token* tok)
{
    return (tok->end - tok->loc + 1);
}

static size_t token_get_real_length(const Token* token)
{
    assert(token_is_identifier(token) || token_is_literal(token));
    
    String* string;
    if (token_is_identifier(token))
    {
        Identifier* node = token->data.identifier;

        string = identifier_get_string(node);
    }
    else
    {
        assert(token_is_literal(token));

        LiteralNode* node = token->data.literal;

        string = &node->value;
    }
    return string_get_len(string);
}

bool token_equal_string(Token* tok, const char* str)
{
    assert(token_is_identifier(tok));

    const size_t str_length = strlen(str);
    const size_t token_len = token_get_real_length(tok);

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

bool token_string_cat(Token* tok1, Token* tok2, Token* dest);

TokenList token_list_allocate(void)
{
    TokenList tokens;

    tokens.used = 0;
    tokens.allocated = 1;
    tokens.tokens = xmalloc(sizeof(Token));

    return tokens;
}

void token_list_free(TokenList* list)
{
    for (size_t i = 0; i < list->used; i++)
    {
        Token* tok = &list->tokens[i];

        token_free(tok);
    }

    free(list->tokens);
}

Token* token_list_next(TokenList* list)
{
    if (list->used == list->allocated)
    {
        list->allocated *= 2;
        list->tokens = xrealloc(list->tokens, sizeof(Token) * list->allocated);
    }

    return &list->tokens[list->used++];
}

TokenStream token_list_to_stream(const TokenList* list)
{
    return (TokenStream) {.tokens = list->tokens, .count = list->used, .current_token = 0};
}
