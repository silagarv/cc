#ifndef PARSE_PARSER_H
#define PARSE_PARSER_H

#include <stddef.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"

#include "files/filepath.h"
#include "files/location.h"
#include "files/source_manager.h"

#include "lex/identifier_table.h"
#include "lex/preprocessor.h"
#include "lex/token.h"

#include "ast/ast_fwd.h"

#include "parse/semantic.h"

#define countof(array) sizeof(array) / sizeof(array[0])

// Macros for checking the parsers current token
#define parser_is(parser, type) parser_has_match(parser, 1, \
        (TokenType[]) {type})
#define parser_has(parser, ...) parser_has_match(parser, \
        countof(((TokenType[]) { __VA_ARGS__ })), \
        ((TokenType[]) { __VA_ARGS__ }))

// Macros for checking the parsers next token
#define parser_is_next(parser, type) parser_has_match_next(parser, 1, \
        (TokenType[]) {type})
#define parser_has_next(parser, ...) parser_has_match_next(parser, \
        countof(((TokenType[]) { __VA_ARGS__ })), \
        ((TokenType[]) { __VA_ARGS__ }))

// Macros for doing parser recover
#define parser_recover(parser, type, flags) \
        parser_recover_many(parser, 1, (TokenType[]) {type}, flags)
#define parser_recover_two(parser, type1, type2, flags) \
        parser_recover_many(parser, 2, (TokenType[]) {type1, type2}, flags)
#define parser_recover_three(parser, type1, type2, type3, flags) \
        parser_recover_many(parser, 3, (TokenType[]) {type1, type2, type3}, \
        flags)

// An enum for the different contexts in which we can parse declarations in
typedef enum ParserDeclaratorContext {
    CONTEXT_FILE,
    CONTEXT_STRUCT,
    CONTEXT_PARAM,
    CONTEXT_BLOCK,
    CONTEXT_TYPE_NAME,
    CONTEXT_KNR
} ParserDeclaratorContext;

typedef enum ParserRecoverFlags {
    RECOVER_NONE = 0,
    RECOVER_EAT = 1 << 0,
    RECOVER_STOP_AT_SEMI = 1 << 1
} ParserRecoverFlags;

typedef struct Parser {
    DiagnosticManager* dm;

    LangOptions* lang;

    Preprocessor pp;

    SemanticChecker sc;

    Token prev_token;
    Token token;
    Token peek_token;

    size_t paren_count;
    size_t bracket_count;
    size_t brace_count;

    // Scope externs;
    // Scope top_level;

    Ast* ast;
} Parser;

bool parser_create(Parser* parser, DiagnosticManager* dm, LangOptions* opts,
        SourceManager* sm, Filepath* main_file, IdentifierTable* ids);
void parser_delete(Parser* parser);

Token* parser_current(Parser* parser);
Token* parser_next(Parser* parser);

TokenType parser_current_type(Parser* parser);
TokenType parser_next_type(Parser* parser);

Location parser_current_location(Parser* parser);

Location parser_consume(Parser* parser);
Location parser_consume_all(Parser* parser, TokenType type);
bool parser_try_consume(Parser* parser, TokenType type, Location* location);

bool parser_has_match(Parser* parser, size_t num, const TokenType* types);
bool parser_has_match_next(Parser* parser, size_t num, const TokenType* types);

void parser_recover_many(Parser* parser, size_t count, TokenType* types,
        ParserRecoverFlags flags);

Location parse_trailing_semi(Parser* parser, const char* context);

bool parser_is_typename(Parser* parser);
bool parser_is_next_typename(Parser* parser);
bool parser_is_expression(Parser* parser);
bool parser_is_statement(Parser* parser);
bool parser_is_initializer(Parser* parser);

DeclarationGroup parse_declaration(Parser* parser, ParserDeclaratorContext ctx,
        Location* semi);
QualifiedType parse_type_name(Parser* parser);

Statement* parse_statement(Parser* parser, bool decls);
Statement* parse_function_body(Parser* parser);
// TODO: expression statement...

Expression* parse_expression(Parser* parser);
Expression* parse_constant_expression(Parser* parser);

Initializer* parse_initializer(Parser* parser);

void parse_translation_unit(Parser* parser);

#endif /* PARSE_PARSER_H */
