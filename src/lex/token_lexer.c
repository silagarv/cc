#include "token_lexer.h"

#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#include "util/panic.h"
#include "util/str_view.h"

#include "lex/location_map.h"
#include "lex/source_line.h"
#include "lex/source_stream.h"

// TODO: will need to somehow implement digraph support in the lexer
// TODO: add support for lexing angled bracket header files

static bool is_identifier(char c)
{
    if ('a' <= c && c <= 'z')
    {
        return true;
    }

    if ('A' <= c && c <= 'Z')
    {
        return true;
    }

    if ('0' <= c && c <= '9')
    {
        return true;
    }

    if (c == '_')
    {
        return true;
    }

    return false;
}

static bool is_numeric(char c)
{
    if ('0' <= c && c <= '9')
    {
        return true;
    }

    return false;
}

static bool is_horizontal_whitespace(char c)
{
    // TODO: is '\0' considered whitespace???
    switch (c)
    {
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            return true;

        default:
            return false;
    }
}

TokenLexer token_lexer_create(SourceStream stream, LineRun* run)
{
    TokenLexer lexer = (TokenLexer) {
        .stream = stream,
        .line_run = run,

        .has_line = false,
        .line = (SourceLine) {0},

        .bol = 0,

        .line_ptr = NULL,
        .line_len = 0,
        .line_pos = 0,

        .start_of_line = false
    };

    return lexer;
}

void token_lexer_close(TokenLexer* lexer)
{
    source_stream_close(&lexer->stream);
}

static bool at_eof(TokenLexer* lexer)
{   
    // If we don't currently have a line so check if there are more available
    if (!lexer->has_line)
    {
        return source_stream_at_eof(&lexer->stream);
    }

    // If we have a line are we at the end?
    if (lexer->line_pos < lexer->line_len)
    {
        return false;
    }

    assert(lexer->line_pos == lexer->line_len);

    // If we're at the end we need to have no more lines to get
    return source_stream_at_eof(&lexer->stream);
}

static bool get_next_line(TokenLexer* lexer)
{
    if (source_stream_at_eof(&lexer->stream))
    {
        return false;
    }

    // Get the line and set the current state
    lexer->line = source_stream_read_line(&lexer->stream);
    lexer->bol = line_run_add_line(lexer->line_run, lexer->line);

    // Set up our flags properly now
    lexer->line_ptr = lexer->line.string.ptr;
    lexer->line_len = lexer->line.string.len;
    lexer->line_pos = 0;

    lexer->has_line = true;
    lexer->start_of_line = true;

    return true;
}

static void remove_line(TokenLexer* lexer)
{
    lexer->has_line = false;

    lexer->line_ptr = NULL;
    lexer->line_pos = 0;
    lexer->line_len = 0;
}

static char next_char(TokenLexer* lexer)
{
retry:
    // This will get a line if we don't have it
    if (!lexer->has_line && !get_next_line(lexer))
    {
        return '\0';
    }

    // If we're at the end of the line we just pretend it doesn't exist
    // and got and get the next one
    if (lexer->line_pos == lexer->line_len)
    {
        remove_line(lexer);

        goto retry;
    }

    return lexer->line_ptr[lexer->line_pos++];
}

static char curr_char(TokenLexer* lexer)
{
    assert(lexer->has_line);

    return lexer->line_ptr[lexer->line_pos];
}

static char peek_char(TokenLexer* lexer)
{
    assert(lexer->has_line);

    return lexer->line_ptr[lexer->line_pos + 1];
}

static Location get_current_location(TokenLexer* lexer)
{
    return lexer->bol + (Location) lexer->line_pos;
}

static void skip_block_comment(TokenLexer* lexer)
{
    while (true)
    {
        char c = next_char(lexer);

        // Check if we encounter eof at all
        if (c == '\0' && at_eof(lexer))
        {
            break;
        }

        if (c == '*' && curr_char(lexer) == '/')
        {
            next_char(lexer);
            break;
        }
    }
}

static void skip_line_comment(TokenLexer* lexer)
{
    // Note since we always append a newline to the end of the file here
    // we do not have to check if we get end of file while reading a line
    // comment
    char c;
    do
    {
        c = next_char(lexer);
    } while (c != '\n');
}

static void lex_preprocessing_number(TokenLexer* lexer, Token* tok)
{
    while (true)
    {
        char c = curr_char(lexer);
        if (is_identifier(c) || c == '.')
        {
            next_char(lexer);
            tok->opt_value.len++;
            if (c == 'e' || c == 'E' || c == 'p' || c == 'P')
            {
                c = curr_char(lexer);
                if (c == '-' || c == '+')
                {
                    tok->opt_value.len++;
                    next_char(lexer);
                }
            }
            continue;
        }
        break;
    }
}

static uint32_t lex_universal_character(TokenLexer* lexer, Token* tok);

static void determine_identifier_keyword(Token* tok)
{
    assert(tok->type == TOKEN_IDENTIFIER);
    assert(tok->opt_value.len > 0);

    switch (string_view_get(&tok->opt_value, 0))
    {
        case 'a':
            if (string_view_equals(&tok->opt_value, "auto"))
            {
                tok->type = TOKEN_AUTO;
            }
            break;
        
        case 'b':
            if (string_view_equals(&tok->opt_value, "break"))
            {
                tok->type = TOKEN_BREAK;
            }
            break;

        case 'c':
            if (string_view_equals(&tok->opt_value, "char"))
            {
                tok->type = TOKEN_CHAR;
            }
            else if (string_view_equals(&tok->opt_value, "const"))
            {
                tok->type = TOKEN_CONST;
            }
            else if (string_view_equals(&tok->opt_value, "continue"))
            {
                tok->type = TOKEN_CONTINUE;
            }
            break;

        case 'd':
            if (string_view_equals(&tok->opt_value, "default"))
            {
                tok->type = TOKEN_DEFAULT;
            }
            else if (string_view_equals(&tok->opt_value, "do"))
            {
                tok->type = TOKEN_DO;
            }
            else if (string_view_equals(&tok->opt_value, "double"))
            {
                tok->type = TOKEN_DOUBLE;
            }
            break;

        case 'e':
            if (string_view_equals(&tok->opt_value, "else"))
            {
                tok->type = TOKEN_ELSE;
            }
            else if (string_view_equals(&tok->opt_value, "enum"))
            {
                tok->type = TOKEN_ENUM;
            }
            else if (string_view_equals(&tok->opt_value, "extern"))
            {
                tok->type = TOKEN_EXTERN;
            }
            break;

        case 'f':
            if (string_view_equals(&tok->opt_value, "float"))
            {
                tok->type = TOKEN_FLOAT;
            }
            else if (string_view_equals(&tok->opt_value, "for"))
            {
                tok->type = TOKEN_FOR;
            }
            break;

        case 'g':
            if (string_view_equals(&tok->opt_value, "goto"))
            {
                tok->type = TOKEN_GOTO;
            }
            break;

        case 'i':
            if (string_view_equals(&tok->opt_value, "if"))
            {
                tok->type = TOKEN_IF;
            }
            else if (string_view_equals(&tok->opt_value, "inline"))
            {
                tok->type = TOKEN_INLINE;
            }
            else if (string_view_equals(&tok->opt_value, "int"))
            {
                tok->type = TOKEN_INT;
            }
            break;

        case 'l':
            if (string_view_equals(&tok->opt_value, "long"))
            {
                tok->type = TOKEN_LONG;
            }
            break;

        case 'r':
            if (string_view_equals(&tok->opt_value, "register"))
            {
                tok->type = TOKEN_REGISTER;
            }
            else if (string_view_equals(&tok->opt_value, "restrict"))
            {
                tok->type = TOKEN_RESTRICT;
            }
            else if (string_view_equals(&tok->opt_value, "return"))
            {
                tok->type = TOKEN_RETURN;
            }
            break;

        case 's':
            if (string_view_equals(&tok->opt_value, "short"))
            {
                tok->type = TOKEN_SHORT;
            }
            else if (string_view_equals(&tok->opt_value, "signed"))
            {
                tok->type = TOKEN_SIGNED;
            }
            else if (string_view_equals(&tok->opt_value, "sizeof"))
            {
                tok->type = TOKEN_SIZEOF;
            }            
            else if (string_view_equals(&tok->opt_value, "static"))
            {
                tok->type = TOKEN_STATIC;
            }
            else if (string_view_equals(&tok->opt_value, "struct"))
            {
                tok->type = TOKEN_STRUCT;
            }
            else if (string_view_equals(&tok->opt_value, "switch"))
            {
                tok->type = TOKEN_SWITCH;
            }
            break;

        case 't':
            if (string_view_equals(&tok->opt_value, "typedef"))
            {
                tok->type = TOKEN_TYPEDEF;
            }
            break;

        case 'u':
            if (string_view_equals(&tok->opt_value, "union"))
            {
                tok->type = TOKEN_UNION;
            }
            else if (string_view_equals(&tok->opt_value, "unsigned"))
            {
                tok->type = TOKEN_UNSIGNED;
            }
            break;

        case 'v':
            if (string_view_equals(&tok->opt_value, "volatile"))
            {
                tok->type = TOKEN_VOLATILE;
            }
            else if (string_view_equals(&tok->opt_value, "void"))
            {
                tok->type = TOKEN_VOID;
            }
            break;

        case 'w':
            if (string_view_equals(&tok->opt_value, "while"))
            {
                tok->type = TOKEN_WHILE;
            }
            break;

        case '_':
            if (string_view_equals(&tok->opt_value, "_Bool"))
            {
                tok->type = TOKEN__BOOL;
            }
            else if (string_view_equals(&tok->opt_value, "_Complex"))
            {
                tok->type = TOKEN__COMPLEX;
            }
            else if (string_view_equals(&tok->opt_value, "_Imaginary"))
            {
                tok->type = TOKEN__IMAGINARY;
            }
            break;

        default:
            break;
    }
}

static void lex_identifier(TokenLexer* lexer, Token* tok)
{
    // TODO: add ucn bullshit...
    while (is_identifier(curr_char(lexer)))
    {
        tok->opt_value.len++;
        next_char(lexer);
    }

    determine_identifier_keyword(tok);
}

static void lex_string_literal(TokenLexer* lexer, Token* tok)
{
    // In here we already have got the starting '"' so there is no need to get
    // this again
    while (true)
    {
        char c = next_char(lexer);

        if (c == '\n')
        {
            break;
        }

        tok->opt_value.len++;

        if (c == '\\')
        {
            next_char(lexer);
            tok->opt_value.len++;
            continue;
        }

        if (c == '"')
        {
            break;
        }
    }
}

static void lex_character_literal(TokenLexer* lexer, Token* tok)
{
    // In here we already have got the starting '\'' so there is no need to get
    // this again so just go until we see the next non-escaped one
    while (true)
    {
        char c = next_char(lexer);
        tok->opt_value.len++;

        if (c == '\\')
        {
            next_char(lexer);
            tok->opt_value.len++;

            continue;
        }

        if (c == '\'')
        {
            break;
        }
    }
}

static Token lex_next(TokenLexer* lexer)
{
restart:;
    bool whitespace = false;

    char* starting_ptr = &lexer->line_ptr[lexer->line_pos];
    size_t len = 1;

    // Set up the token
    Token tok = (Token)
    {
        .type = TOKEN_UNKNOWN,
        .loc = get_current_location(lexer),

        .opt_value = (StringView) {.start = starting_ptr, .len = len},

        .leading_space = whitespace,
        .start_of_line = lexer->start_of_line,
        .disable_expand = false,
    };

    char c = next_char(lexer);
    switch (c)
    {
        // If we hit these we're either at the end or got a null character
        // if we got a null character treat it as whitespace and just continue
        // that is likely an error
        case '\0':
            if (at_eof(lexer))
            {
                tok.type = TOKEN_EOF;
                break;
            }
            else
            {
                next_char(lexer);
                whitespace = true;
                goto restart;
            }
            break;

        // Our special whitespace tokens skip all of the white space that we can
        // and go back to the start but ensure that we set the whitespace flag
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            while (is_horizontal_whitespace(curr_char(lexer)))
            {
                next_char(lexer);
            }
            whitespace = true;
            goto restart;

        // Note that the lexer's start of line flag will automatically be set
        // when we get the next line from it so there is no need to do anything
        // additional here
        case '\n':
            // Is this the best way to deal with the bug of not having any text
            // for a value token when we don't get a line
            get_next_line(lexer);
            goto restart;

        // Number cases
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            tok.type = TOKEN_NUMBER;
            lex_preprocessing_number(lexer, &tok);
            break;

        // Case of wide string literal
        case 'L':
            c = curr_char(lexer);
            if (c == '"')
            {
                tok.type = TOKEN_WIDE_STRING;
                next_char(lexer);
                tok.opt_value.len++;
                lex_string_literal(lexer, &tok);
                break;
            }
            else if (c == '\'')
            {
                tok.type = TOKEN_WIDE_CHARACTER;
                next_char(lexer);
                tok.opt_value.len++;
                lex_character_literal(lexer, &tok);
                break;
            }

        // Identifier cases excluding 'L' since that is used for wide string
        // and character literals in C99
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
        case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case '_':
            tok.type = TOKEN_IDENTIFIER;
            lex_identifier(lexer, &tok);
            break;

        case '\'':
            tok.type = TOKEN_CHARACTER;
            lex_character_literal(lexer, &tok);
            break;

        case '"':
            tok.type = TOKEN_STRING;
            lex_string_literal(lexer, &tok);
            break;

        case '.':
            c = curr_char(lexer);
            if (is_numeric(c))
            {
                tok.type = TOKEN_NUMBER;
                lex_preprocessing_number(lexer, &tok);
            }
            else if (c == '.' && peek_char(lexer) == '.')
            {
                tok.type = TOKEN_ELIPSIS;
                next_char(lexer);
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_DOT;
            }
            break;

        case '/':
            c = curr_char(lexer);
            if (c == '*')
            {
                next_char(lexer);
                skip_block_comment(lexer);
                goto restart;
            }
            else if (c == '/')
            {
                next_char(lexer);
                skip_line_comment(lexer);
                goto restart;
            }
            else if (c == '=')
            {
                tok.type = TOKEN_SLASH_EQUAL;
                next_char(lexer);
            }
            else {
                tok.type = TOKEN_SLASH;
            }
            break;

        case '*':
            c = curr_char(lexer);
            if (c == '=')
            {
                tok.type = TOKEN_STAR_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_STAR;
            }
            break;
        
        case '%':
            c = curr_char(lexer);
            if (c == '=')
            {
                tok.type = TOKEN_PERCENT_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_PERCENT;
            }
            break;

        case '+':
            c = curr_char(lexer);
            if (c == '+')
            {
                tok.type = TOKEN_PLUS_PLUS;
                next_char(lexer);
            }
            else if (c == '=')
            {
                tok.type = TOKEN_PLUS_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_PLUS;
            }
            break;

        case '-':
            c = curr_char(lexer);
            if (c == '-')
            {
                tok.type = TOKEN_MINUS_MINUS;
                next_char(lexer);
            }
            else if (c == '>')
            {
                tok.type = TOKEN_ARROW;
                next_char(lexer);
            }
            else if (c == '=')
            {
                tok.type = TOKEN_MINUS_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_MINUS;
            }
            break;

        case '|':
            c = curr_char(lexer);
            if (c == '|')
            {
                tok.type = TOKEN_OR_OR;
                next_char(lexer);
            }
            else if (c == '=')
            {
                tok.type = TOKEN_OR_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_OR;
            }
            break;

        case '&':
            c = curr_char(lexer);
            if (c == '&')
            {
                tok.type = TOKEN_AND_AND;
                next_char(lexer);
            }
            else if (c == '=')
            {
                tok.type = TOKEN_AND_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_AND;
            }
            break;

        case '^':
            c = curr_char(lexer);
            if (c == '=')
            {
                tok.type = TOKEN_XOR_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_XOR;
            }
            break;

        case '=':
            c = curr_char(lexer);
            if (c == '=')
            {
                tok.type = TOKEN_EQUAL_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_EQUAL;
            }
            break;

        case '!':
            c = curr_char(lexer);
            if (c == '=')
            {
                tok.type = TOKEN_NOT_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_NOT;
            }
            break;

        case '#':
            c = curr_char(lexer);
            if (c == '#')
            {
                tok.type = TOKEN_HASH_HASH;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_HASH;
            }
            break;
        
        case '<':
            c = curr_char(lexer);
            if (c == '<')
            {
                next_char(lexer);
                c = curr_char(lexer);
                if (c == '=')
                {
                    tok.type = TOKEN_LT_LT_EQUAL;
                    next_char(lexer);
                }
                else
                {
                    tok.type = TOKEN_LT_LT;
                }
            }
            else if (c == '=')
            {
                tok.type = TOKEN_LT_EQUAL;
            }
            else
            {
                tok.type = TOKEN_LT;
            }
            break;

        case '>':
            c = curr_char(lexer);
            if (c == '>')
            {
                next_char(lexer);
                c = curr_char(lexer);
                if (c == '=')
                {
                    tok.type = TOKEN_GT_GT_EQUAL;
                    next_char(lexer);
                }
                else
                {
                    tok.type = TOKEN_GT_GT;
                }
            }
            else if (c == '=')
            {
                tok.type = TOKEN_GT_EQUAL;
                next_char(lexer);
            }
            else
            {
                tok.type = TOKEN_GT;
            }
            break;

        case '[': tok.type = TOKEN_LBRACKET; break;
        case ']': tok.type = TOKEN_RBRACKET; break;
        case '(': tok.type = TOKEN_LPAREN; break;
        case ')': tok.type = TOKEN_RPAREN; break;
        case '{': tok.type = TOKEN_LCURLY; break;
        case '}': tok.type = TOKEN_RCURLY; break;
        case '?': tok.type = TOKEN_QUESTION; break;
        case ';': tok.type = TOKEN_SEMI; break;
        case ',': tok.type = TOKEN_COMMA; break;
        case '~': tok.type = TOKEN_TILDE; break;
        case ':': tok.type = TOKEN_COLON; break;

        default:           
            // TODO: nothing to do here yet until we add in unicode support
            // since we will need to do other things once we add that in
            break;
    }

    lexer->start_of_line = false;

    return tok;
}

Token token_lexer_get_next(TokenLexer* lexer)
{
    if (!lexer->has_line && !get_next_line(lexer))
    {
        return (Token) {0};
    }

    return lex_next(lexer);
}

