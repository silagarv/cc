#include "parser_new.h"

#include <stddef.h>
#include <assert.h>

#include "files/location.h"
#include "lex/preprocessor.h"
#include "lex/token.h"

bool parser_create(Parser* parser, DiagnosticManager* dm, LangOptions* opts,
        SourceManager* sm, Filepath* main_file, IdentifierTable* ids);
void parser_delete(Parser* parser);

Token* parser_current(Parser* parser)
{
    return &parser->token;
}

Token* parser_next(Parser* parser)
{
    preprocessor_peek_token(&parser->pp, &parser->peek_token);
    return &parser->peek_token;
}

TokenType parser_current_type(Parser* parser)
{
    return token_get_type(parser_current(parser));
}

TokenType parser_next_type(Parser* parser)
{
    return token_get_type(parser_next(parser));
}

Location parser_current_location(Parser* parser)
{
    return token_get_location(parser_current(parser));
}

Location parser_consume(Parser* parser)
{
    Location loc = token_get_location(&parser->token);
    preprocessor_advance_token(&parser->pp, parser_current(parser));
    return loc;
}

Location parser_consume_all(Parser* parser, TokenType type)
{
    assert(parser_is(parser, type) && "isn't the type?");

    Location loc = token_get_location(&parser->token);
    while (parser_is(parser, type))
    {
        parser_consume(parser);
    }

    return loc;
}

bool parser_try_consume(Parser* parser, TokenType type, Location* location)
{
    *location = token_get_location(parser_current(parser));

    if (parser_is(parser, type))
    {
        parser_consume(parser);
        return true;
    }

    return false;
}

bool parser_has_match(Parser* parser, size_t num, const TokenType* types)
{
    for (size_t i = 0; i < num; i++)
    {
        if (token_is_type(&parser->token, types[i]))
        {
            return true;
        }
    }

    return false;
}

bool parser_has_match_next(Parser* parser, size_t num, const TokenType* types)
{
    Token* next = parser_next(parser);

    for (size_t i = 0; i < num; i++)
    {
        if (token_is_type(next, types[i]))
        {
            return true;
        }
    }

    return false;
}

void parser_recover_many(Parser* parser, size_t count, TokenType* types,
        ParserRecoverFlags flags)
{
    bool has_skipped = false;
    size_t paren_count = 0;
    size_t bracket_count = 0;
    size_t curly_count = 0;
    while (true)
    {
        // Check if we got to a token we wanted to stop at        
        if (parser_has_match(parser, count, types))
        {
            // If we're meant to eat it do so
            if (flags & RECOVER_EAT)
            {
                parser_consume(parser);
            }

            return;
        }

        TokenType current = token_get_type(parser_current(parser));
        switch (current)
        {
            // Don't want to accidentally consume this
            case TOK_EOF:
                return;

            // For each of our paren thesis types make sure we try to balance
            // then as best as possible
            case TOK_LPAREN:
                paren_count++;
                parser_consume(parser);
                parser_recover_many(parser, 1, (TokenType[1]) {TOK_RPAREN},
                        RECOVER_EAT);
                break;

            case TOK_LBRACKET:
                bracket_count++;
                parser_consume(parser);
                parser_recover_many(parser, 1, (TokenType[1]) {TOK_RBRACKET},
                        RECOVER_EAT);
                break;

            case TOK_LCURLY:
                curly_count++;
                parser_consume(parser);
                parser_recover_many(parser, 1, (TokenType[1]) {TOK_RCURLY},
                        RECOVER_EAT);
                break;

            // For our closing types we need to do something different
            case TOK_RPAREN:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (paren_count)
                {
                    paren_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                parser_consume(parser);
                break;

            case TOK_RBRACKET:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (bracket_count)
                {
                    bracket_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                parser_consume(parser);
                break;
                
            case TOK_RCURLY:
                // If we have parens and this is not the problem token. We can
                // assume we are done since the parse seems to want to finish
                // handling some other production.
                if (curly_count)
                {
                    curly_count--;
                    if (has_skipped)
                    {
                        return;
                    }
                }
                parser_consume(parser);
                break;

            // Check if we were meant to stop at a semi
            case TOK_SEMI:
                if (flags & RECOVER_STOP_AT_SEMI)
                {
                    return;
                }

            /* FALLTHROUGH */

            default:
                parser_consume(parser);
                break;
        }
        has_skipped = true;
    }
}

Location parse_trailing_semi(Parser* parser, const char* context)
{
    if (parser_is(parser, TOK_SEMI))
    {
        return parser_consume(parser);
    }

    // If not a semi, emit an error and attempt some recovery.
    diagnostic_error_at(parser->dm, parser_current_location(parser),
            "expected ';' after %s", context);
    parser_recover(parser, TOK_RCURLY, RECOVER_STOP_AT_SEMI);

    // If we actually stopped on a semi consume it
    if (parser_is(parser, TOK_SEMI))
    {
        parser_consume(parser);
    }

    return LOCATION_INVALID;
}

bool parser_token_is_typename(Parser* parser, Token* token)
{
    switch (token_get_type(token))
    {
        case TOK_void:
        case TOK_char:
        case TOK_short:
        case TOK_int:
        case TOK_long:
        case TOK_float:
        case TOK_double:
        case TOK_signed:
        case TOK_unsigned:
        case TOK__Bool:
        case TOK__Complex:
        case TOK__Imaginary:
        case TOK_struct:
        case TOK_union:
        case TOK_enum:
        case TOK_bool:
        case TOK__Decimal128:
        case TOK__Decimal32:
        case TOK__Decimal64:
        case TOK__Bitint:

        case TOK_typeof:
        case TOK_typeof_unqual:

        case TOK_const:
        case TOK_volatile:
        case TOK_restrict:
        case TOK__Atomic:

        case TOK_inline:
        case TOK__Noreturn:

        case TOK_typedef:
        case TOK_extern:
        case TOK_static:
        case TOK_register:
        case TOK_auto:
        case TOK__Thread_local:
        case TOK_thread_local:
        case TOK_constexpr:

        case TOK__Alignas:
        case TOK_alignas:
            return true;

        case TOK_IDENTIFIER:
        {
            return semantic_checker_identifier_is_typename(&parser->sc,
                    token_get_identifier(token), true);
        }

        case TOK_static_assert:
        case TOK__Static_assert:
            return true;

        default:
            return false;
    }
}

bool parser_is_typename(Parser* parser)
{
    return parser_token_is_typename(parser, parser_current(parser));
}

bool parser_is_next_typename(Parser* parser)
{
    return parser_token_is_typename(parser, parser_next(parser));
}

bool parser_is_expression(Parser* parser)
{
    switch (token_get_type(parser_current(parser)))
    {
        case TOK_NUMBER:
        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
        case TOK_UTF8_CHARACTER:
        case TOK_UTF16_CHARACTER:
        case TOK_UTF32_CHARACTER:
        case TOK_STRING:
        case TOK_WIDE_STRING:
        case TOK_UTF8_STRING:
        case TOK_UTF16_STRING:
        case TOK_UTF32_STRING:
        case TOK_LPAREN:
        case TOK_PLUS_PLUS:
        case TOK_MINUS_MINUS:
        case TOK_AND:
        case TOK_STAR:
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_NOT:
        case TOK_TILDE:
        case TOK_sizeof:
        case TOK_IDENTIFIER:
        case TOK__Alignof:
        case TOK_alignof:
        case TOK_false:
        case TOK_true:
        case TOK_nullptr:
        case TOK__Generic:
        case TOK___func__:
        case TOK___builtin_va_arg:
        case TOK___builtin_offsetof:
            return true;

        default:
            return false;
    }
}

bool parser_is_statement(Parser* parser)
{
    switch (token_get_type(parser_current(parser)))
    {
        case TOK_LCURLY:
        case TOK_SEMI:

        case TOK_case:
        case TOK_default:
        case TOK_if:
        case TOK_switch:
        case TOK_while:
        case TOK_do:
        case TOK_for:
        case TOK_goto:
        case TOK_continue:
        case TOK_break:
        case TOK_return:
        case TOK_asm:
            return true;

        default:
            return parser_is_expression(parser) || parser_is_typename(parser);
    }
}

bool parser_is_initializer(Parser* parser)
{
    return parser_is(parser, TOK_LCURLY) || parser_is_expression(parser);
}

void parse_external_declaration(Parser* parser)
{
    Location semi;
    Declaration* decl = parse_declaration(parser, &semi);
    // TODO: add the declaration to the AST??????????????
}

void parse_top_level(Parser* parser)
{
    assert(!parser_is(parser, TOK_EOF) && "cannot parse EOF");

    switch (token_get_type(parser_current(parser)))
    {
        case TOK_SEMI:
        {
            Location semis = parser_consume_all(parser, TOK_SEMI);
            diagnostic_warning_at(parser->dm, semis, Wextra_semi,
                    "extra ';' outside of a function");
            return;
        }

        case TOK_RCURLY:
        {
            diagnostic_error_at(parser->dm, parser_consume(parser),
                    "extraneous closing brace ('}')");
            return;
        }

        default:
            parse_external_declaration(parser);
            return;
    }
}

void parse_translation_unit_internal(Parser* parser)
{
    if (parser_is(parser, TOK_EOF))
    {
        diagnostic_warning_at(parser->dm, parser_current_location(parser),
                Wempty_translation_unit, "ISO C requires a translation unit to "
                "contain at least one declaration");
    }

    while (!parser_is(parser, TOK_EOF))
    {
        parse_top_level(parser);
    }
}

void parse_translation_unit(Parser* parser)
{
    
    parser_consume(parser);
    parse_translation_unit_internal(parser);
}
