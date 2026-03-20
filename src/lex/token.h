#ifndef TOKEN_H
#define TOKEN_H

#include <stddef.h>
#include <stdbool.h>

#include "util/arena.h"
#include "util/str.h"

#include "files/location.h"

struct Identifier;

// Our token types here
// Note that we use the pp definitions of tokens mainly which are then converted
// on demand by the parser. E.g. if its a number it gets converted, and properly
// checked there, and e.g. string literals are concatenated by parser
typedef enum TokenType {
    TOK_EOF,

    // Punctuators
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LCURLY,
    TOK_RCURLY,
    TOK_DOT,
    TOK_ARROW,
    TOK_PLUS_PLUS,
    TOK_MINUS_MINUS,
    TOK_AND,
    TOK_STAR,
    TOK_PLUS,
    TOK_MINUS,
    TOK_TILDE,
    TOK_NOT,
    TOK_SLASH,
    TOK_PERCENT,
    TOK_LT_LT,
    TOK_GT_GT,
    TOK_LT,
    TOK_GT,
    TOK_LT_EQUAL,
    TOK_GT_EQUAL,
    TOK_EQUAL_EQUAL,
    TOK_NOT_EQUAL,
    TOK_XOR,
    TOK_OR,
    TOK_AND_AND,
    TOK_OR_OR,
    TOK_QUESTION,
    TOK_COLON,
    TOK_COLON_COLON,
    TOK_SEMI,
    TOK_ELIPSIS,
    TOK_EQUAL,
    TOK_STAR_EQUAL,
    TOK_SLASH_EQUAL,
    TOK_PERCENT_EQUAL,
    TOK_PLUS_EQUAL,
    TOK_MINUS_EQUAL,
    TOK_LT_LT_EQUAL,
    TOK_GT_GT_EQUAL,
    TOK_AND_EQUAL,
    TOK_XOR_EQUAL,
    TOK_OR_EQUAL,
    TOK_COMMA,
    TOK_HASH,
    TOK_HASH_HASH,

    // Special tokens begin here
    TOK_IDENTIFIER,
    TOK_NUMBER,
    TOK_CHARACTER,
    TOK_WIDE_CHARACTER,
    TOK_UTF8_CHARACTER,
    TOK_UTF16_CHARACTER,
    TOK_UTF32_CHARACTER,
    TOK_STRING,
    TOK_WIDE_STRING,
    TOK_UTF8_STRING,
    TOK_UTF16_STRING,
    TOK_UTF32_STRING,

    // Keyword tokens
    TOK_alignas, // C23 Keyword
    TOK_alignof, // C23 Keyword
    TOK_auto,
    TOK_break,
    TOK_bool, // C23 keyword
    TOK_case,
    TOK_char,
    TOK_const,
    TOK_constexpr, // C23 Keyword
    TOK_continue,
    TOK_default,
    TOK_do,
    TOK_double,
    TOK_else,
    TOK_enum,
    TOK_extern,
    TOK_false, // C23 Keyword
    TOK_float,
    TOK_for,
    TOK_goto,
    TOK_if,
    TOK_inline, // C99 Keyword
    TOK_int,
    TOK_long,
    TOK_nullptr, // C23 Keyword
    TOK_register,
    TOK_restrict,
    TOK_return,
    TOK_short,
    TOK_signed,
    TOK_sizeof,
    TOK_static,
    TOK_static_assert, // C23 Keyword
    TOK_struct,
    TOK_switch,
    TOK_thread_local, // C23 Keyword
    TOK_true, // C23 Keyword
    TOK_typedef,
    TOK_typeof, // C23 Keyword
    TOK_typeof_unqual, // C23 Keyword
    TOK_union,
    TOK_unsigned,
    TOK_void,
    TOK_volatile,
    TOK_while,
    TOK__Alignas, // C11 Keyword
    TOK__Alignof, // C11 Keyword
    TOK__Atomic, // C11 Keyword
    TOK__Bitint, // C23 Keyword
    TOK__Bool, // C99 Keyword
    TOK__Complex, // C99 Keyword
    TOK__Decimal128, // C23 Keyword
    TOK__Decimal32, // C23 Keyword
    TOK__Decimal64, // C23 Keyword
    TOK__Generic, // C11 Keyword
    TOK__Imaginary, // C99 Keyword
    TOK__Noreturn, // C11 Keyword
    TOK__Static_assert, // C11 Keyword
    TOK__Thread_local, // C11 Keyword

    TOK___func__, // __func__

    // __attribute__ extension token for parsing and ignoring attributes
    TOK___attribute__,
    TOK___extension__,
    TOK_asm,

    // Some nice builtins we want to support
    TOK___builtin_va_arg,
    TOK___builtin_offsetof,

    // Support for gnu local label extension (unimplemented)
    TOK___label__,

    // Special preprocessing token names
    TOK_PP_define,
    TOK_PP_undef,
    TOK_PP_include,
    TOK_PP_embed, // C23
    TOK_PP_if,
    TOK_PP_ifdef,
    TOK_PP_ifndef,
    TOK_PP_elifndef, // C23
    TOK_PP_elifdef, // C23
    TOK_PP_else,
    TOK_PP_elif,
    TOK_PP_endif,
    TOK_PP_line,
    TOK_PP_error,
    TOK_PP_warning, // C23
    TOK_PP_pragma,
    TOK_PP_defined, 
    TOK_PP___has_include, // C23
    TOK_PP___has_embed, // C23
    TOK_PP___has_c_attribute, // C23
    
    TOK_PP_HEADER_NAME,

    TOK_PP_ARGEND, // Special token to mark the end of a argument preexpansion
    TOK_PP_EOD, // Special token to mark the end of directive

    TOK_UNKNOWN,

    /* the last token to get the number of tokens */
    TOK_LAST
} TokenType;

typedef struct LiteralNode {
    String value;
} LiteralNode;

// From TokenData we should always be able to retrieve the token spelling back
typedef union TokenData {
    struct Identifier* identifier; // The identifier this token corrosponds to
    char* raw; // The raw start of the token.
} TokenData;

// Different flags for our token to store
typedef enum TokenFlags {
    TOK_FLAG_NONE = 0, // Represents no flag
    TOK_FLAG_BOL = 1 << 0, // Beginning of line
    TOK_FLAG_WHITESPACE = 1 << 1, // leading space
    TOK_FLAG_DIGRAPH = 1 << 2, // Are we a digraph
    TOK_FLAG_NOEXPAND = 1 << 3 // Should we avoid expanding this token. 6.10.3
} TokenFlags;

// The structure of a token in order to capture all of the relavent information
typedef struct Token {
    Location loc; // the starting character in the token
    Location end; // the ending character within the token

    TokenType type; // The type of token
    
    TokenFlags flags; // the flags of the token

    TokenData data; // data the token needs
} Token;

typedef struct TokenListEntry TokenListEntry;

// TODO: I feel it would be nice to add a count of how many tokens are currently
// TODO: in the TokenList as it would somethimes be helpful for flattening the
// TODO: list.
typedef struct TokenList {
    Arena allocator;
    TokenListEntry* head;
    TokenListEntry* tail;
} TokenList;

// Basically a string view but for tokens. Really nice and simple structure 
// which should be relatively easy to work with.
typedef struct TokenStream {
    Token* tokens;
    size_t length;
    size_t cursor;
} TokenStream;

void token_set_flag(Token* token, TokenFlags flag);
void token_unset_flag(Token* token, TokenFlags flag);
bool token_has_flag(const Token* token, TokenFlags flag);
TokenType token_get_type(const Token* token);
void token_set_location(Token* token, Location location);
Location token_get_location(const Token* token);
Location token_get_end(const Token* token);
void token_set_end(Token* token, Location location);
bool token_is_type(const Token* token, TokenType type);
bool token_is_literal(const Token* token);
bool token_is_string(const Token* token);
bool token_is_character(const Token* token);
void token_set_type(Token* token, TokenType type);
struct Identifier* token_get_identifier(const Token* token);
void token_classify_identifier(Token* token);
void token_classify_pp_identifier(Token* token);
bool token_is_identifier_like(const Token* token);
bool token_is_directive_start(const Token* token);

String token_get_literal_node(const Token* token);
size_t token_get_length(Token* tok);

bool tokens_equal(const Token* tok1, const Token* tok2);

const char* token_type_get_name(TokenType type);

// Token list stuff
TokenListEntry* token_list_entry_next(const TokenListEntry* entry);
Token token_list_entry_token(const TokenListEntry* entry);

TokenList token_list(Arena arena);
void token_list_free(TokenList* list);
bool token_list_empty(const TokenList* list);
void token_list_push_front(TokenList* list, Token tok);
Token token_list_peek_front(const TokenList* list);
Token token_list_pop_front(TokenList* list);
Token token_list_pop_back(TokenList* list);
void token_list_push_back(TokenList* list, Token tok);
Token token_list_peek_back(const TokenList* list);
TokenListEntry* token_list_iter(const TokenList* list);

// Desctructively move all of the tokens in the list (should be the same size)
// as the count, into an array of allocated tokens (by arena) for the purposes
// of collecting macro arguments and other things.
Token* token_list_flatten(Arena* arena, TokenList* list, size_t count);

// Token stream functions are below. Note that our token stream structure does
// not actually own the memory used for the tokens so it does not have to do
// anything in order to deallocate the memory and can just be destroyed like
// any other stack memory.
TokenStream token_stream_create(Token* tokens, size_t len);
TokenStream token_stream_create_empty(void);

Token* token_stream_tokens(const TokenStream* stream);
size_t token_stream_length(const TokenStream* stream);
size_t token_stream_cursor(const TokenStream* stream);

bool token_stream_end(const TokenStream* stream);
Token token_stream_consume(TokenStream* stream);
Token token_stream_peek(const TokenStream* stream);
Token token_stream_peek_n(const TokenStream* stream, size_t n);

Token token_stream_get(const TokenStream* stream, size_t index);
Token token_stream_first(const TokenStream* stream);
Token token_stream_last(const TokenStream* stream);

#endif /* TOKEN_H */
