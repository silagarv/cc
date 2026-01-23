#include "token.h"

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "util/panic.h"
#include "util/vec.h"
#include "util/xmalloc.h"
#include "util/str.h"

#include "lex/identifier_table.h"

vector_of_impl(Token, Token, token)

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
        case TOK_PP_define:
        case TOK_PP_undef:
        case TOK_PP_include:
        case TOK_PP_if:
        case TOK_PP_ifdef:
        case TOK_PP_ifndef:
        case TOK_PP_else:
        case TOK_PP_elif:
        case TOK_PP_endif:
        case TOK_PP_line:
        case TOK_PP_error:

        // Keywords
        case TOK_auto:
        case TOK_break:
        case TOK_case:
        case TOK_char:
        case TOK_const:
        case TOK_continue:
        case TOK_default:
        case TOK_do:
        case TOK_double:
        case TOK_else:
        case TOK_enum:
        case TOK_extern:
        case TOK_float:
        case TOK_for:
        case TOK_goto:
        case TOK_if:
        case TOK_inline:
        case TOK_int:
        case TOK_long:
        case TOK_register:
        case TOK_restrict:
        case TOK_return:
        case TOK_short:
        case TOK_signed:
        case TOK_sizeof:
        case TOK_static:
        case TOK_struct:
        case TOK_switch:
        case TOK_typedef:
        case TOK_union:
        case TOK_unsigned:
        case TOK_void:
        case TOK_volatile:
        case TOK_while:
        case TOK__Bool:
        case TOK__Complex:
        case TOK__Imaginary:
        case TOK___func__:

        // Generic identifier
        case TOK_IDENTIFIER:
            return true;

        default:
            return false;
    }
}

bool token_is_literal(const Token* token)
{
    switch (token->type)
    {
        case TOK_NUMBER:
        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
        case TOK_STRING:
        case TOK_WIDE_STRING:
        
        case TOK_PP_HEADER_NAME:
            return true;

        case TOK_UNKNOWN:
            return true; // SPECIAL CASE HERE

        default:
            return false;
    }
}

bool token_is_string(const Token* token)
{
    return (token->type == TOK_STRING || token->type == TOK_WIDE_STRING);
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
        case TOK_UNKNOWN:
        case TOK_IDENTIFIER:
        case TOK_NUMBER:
        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
        case TOK_STRING:
        case TOK_WIDE_STRING:
        case TOK_PP_HEADER_NAME:
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
        case TOK_UNKNOWN: return "<unknown-token>";
        case TOK_EOF: return "end-of-file";
        case TOK_LBRACKET: return "[";
        case TOK_RBRACKET: return "]";
        case TOK_LPAREN: return "(";
        case TOK_RPAREN: return ")";
        case TOK_LCURLY: return "{";
        case TOK_RCURLY: return "}";
        case TOK_DOT: return ".";
        case TOK_ARROW: return "->";
        case TOK_PLUS_PLUS: return "++";
        case TOK_MINUS_MINUS: return "--";
        case TOK_AND: return "&";
        case TOK_STAR: return "*";
        case TOK_PLUS: return "+";
        case TOK_MINUS: return "-";
        case TOK_TILDE: return "~";
        case TOK_NOT: return "!";
        case TOK_SLASH: return "/";
        case TOK_PERCENT: return "%";
        case TOK_LT_LT: return "<<";
        case TOK_GT_GT: return ">>";
        case TOK_LT: return "<";
        case TOK_GT: return ">";
        case TOK_LT_EQUAL: return "<=";
        case TOK_GT_EQUAL: return ">=";
        case TOK_EQUAL_EQUAL: return "==";
        case TOK_NOT_EQUAL: return "!=";
        case TOK_XOR: return "^";
        case TOK_OR: return "|";
        case TOK_AND_AND: return "&&";
        case TOK_OR_OR: return "||";
        case TOK_QUESTION: return "?";
        case TOK_COLON: return ":";
        case TOK_COLON_COLON: return "::";
        case TOK_SEMI: return ";";
        case TOK_ELIPSIS: return "...";
        case TOK_EQUAL: return "=";
        case TOK_STAR_EQUAL: return "*=";
        case TOK_SLASH_EQUAL: return "/=";
        case TOK_PERCENT_EQUAL: return "%=";
        case TOK_PLUS_EQUAL: return "+=";
        case TOK_MINUS_EQUAL: return "-=";
        case TOK_LT_LT_EQUAL: return "<<=";
        case TOK_GT_GT_EQUAL: return ">>=";
        case TOK_AND_EQUAL: return "&=";
        case TOK_XOR_EQUAL: return "^=";
        case TOK_OR_EQUAL: return "|=";
        case TOK_COMMA: return ",";
        case TOK_HASH: return "#";
        case TOK_HASH_HASH: return "##";
        case TOK_auto: return "auto";
        case TOK_break: return "break";
        case TOK_case: return "case";
        case TOK_char: return "char";
        case TOK_const: return "const";
        case TOK_continue: return "continue";
        case TOK_default: return "default";
        case TOK_do: return "do";
        case TOK_double: return "double";
        case TOK_else: return "else";
        case TOK_enum: return "enum";
        case TOK_extern: return "extern";
        case TOK_float: return "float";
        case TOK_for: return "for";
        case TOK_goto: return "goto";
        case TOK_if: return "if";
        case TOK_inline: return "inline";
        case TOK_int: return "int";
        case TOK_long: return "long";
        case TOK_register: return "register";
        case TOK_restrict: return "restrict";
        case TOK_return: return "return";
        case TOK_short: return "short";
        case TOK_signed: return "signed";
        case TOK_sizeof: return "sizeof";
        case TOK_static: return "static";
        case TOK_struct: return "struct";
        case TOK_switch: return "switch";
        case TOK_typedef: return "typedef";
        case TOK_union: return "union";
        case TOK_unsigned: return "unsigned";
        case TOK_void: return "void";
        case TOK_volatile: return "volatile";
        case TOK_while: return "while";
        case TOK__Bool: return "_Bool";
        case TOK__Complex: return "_Complex";
        case TOK__Imaginary: return "_Imaginary";
        case TOK___func__: return "__func__";
        case TOK_IDENTIFIER: return "identifier";
        case TOK_NUMBER: return "number";
        case TOK_CHARACTER: return "character literal";
        case TOK_WIDE_CHARACTER: return "wide character literal";
        case TOK_STRING: return "string literal";
        case TOK_WIDE_STRING: return "wide string literal";
        case TOK___attribute__: return "__attribute__";
        case TOK___extension__: return "__extension__";
        case TOK_asm: return "asm";

        case TOK_PP_HEADER_NAME: return "header name"; 
        case TOK_PP_EOD: return "end-of-directive";
        
        case TOK_PP_define: return "define";
        case TOK_PP_undef: return "undef";
        case TOK_PP_include: return "include";
        case TOK_PP_if: return "if";
        case TOK_PP_ifdef: return "ifdef";
        case TOK_PP_ifndef: return "ifndef";
        case TOK_PP_else: return "else";
        case TOK_PP_elif: return "elif";
        case TOK_PP_endif: return "endif";
        case TOK_PP_line: return "line";
        case TOK_PP_error: return "error";
        case TOK_PP_pragma: return "pragma";
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
    if (tok->type == TOK_PP_EOD)
    {
        return "\n"; /* special case here */
    }

    if (tok->type == TOK_EOF)
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
