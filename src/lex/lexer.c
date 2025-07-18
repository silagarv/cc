#include "lexer.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "lex/token.h"
#include "util/panic.h"
#include "util/panic.h"
#include "util/buffer.h"

#include "lex/char_type.h"
#include "lex/location.h"
#include "util/str.h"

#define IDENTIFIER_START_SIZE (10)
#define NUMBER_START_SIZE (10)
#define STRING_START_SIZE (10)

Lexer lexer(const char* buffer_start, const char* buffer_end, Location start_loc)
{
    assert(*buffer_end == '\0');

    Lexer lexer;
    lexer.buffer_start = buffer_start;
    lexer.buffer_end = buffer_end;

    lexer.current_ptr = (char*) buffer_start;

    lexer.start_loc = start_loc;

    lexer.start_of_line = true;
    lexer.lexing_directive = false;
    lexer.can_lex_header = false;

    return lexer;
}

// Modify the lexer's position
static void seek(Lexer* lexer, size_t pos)
{
    lexer->current_ptr += pos;
}

// Get the lexers current position
static char* get_position(const Lexer* lexer)
{
    return lexer->current_ptr;
}

// Set the lexers position
static void set_position(Lexer* lexer, char* pos)
{
    assert(pos >= lexer->buffer_start && pos <= lexer->buffer_end);

    lexer->current_ptr = pos;
}

static char get_trigraph(char c)
{
    switch (c)
    {
        case '(': return '[';
        case ')': return ']';
        case '<': return '{';
        case '>': return '}';
        case '=': return '#';
        case '/': return '\\';
        case '\'': return '^';
        case '!': return '|';
        case '-': return '~';
        default: return '\0';
    }
}

static bool is_simple_char(char c)
{
    if (c == '?' || c == '\\')
    {
        return false;
    }

    return true;   
}

static char get_char_and_size_slow(const Lexer* lexer, size_t* peek, char curr);
static char get_char_and_size(const Lexer* lexer, size_t* peek);

// Get a the next character and the number of char's it took to get to it 
static char get_char_and_size(const Lexer* lexer, size_t* peek)
{
    const char current = lexer->current_ptr[*peek];

    if (is_simple_char(current))
    {
        *peek += 1;
        return current;
    }
    
    return get_char_and_size_slow(lexer, peek, current);
}

// Note this is strict on the rules in the c standard. Maybe we don't want
// to be this strict in the future. So this method could be subject to change.
static char get_char_and_size_slow(const Lexer* lexer, size_t* peek, char curr)
{
    assert(!is_simple_char(curr));

    const char next = lexer->current_ptr[*peek + 1];

    if (curr == '\\' && next == '\n')
    {
        *peek += 2;

        // We need to check here if we are at the end of the buffer. Could also
        // possibly add a warning here in the future. This is necessary to
        // prevent peek increasing by another and then the possible later call
        // to function 'seek' failing.
        if (lexer->current_ptr + *peek == lexer->buffer_end)
        {
            return '\0';
        }

        return get_char_and_size(lexer, peek);
    }

    if (curr == '?' && next == '?')
    {
        const char trigraph = get_trigraph(lexer->current_ptr[*peek + 2]);
        const bool has_trigraph = (trigraph != '\0');

        if (!has_trigraph)
        {
            *peek += 1;
            return '?';
        }

        // In the future we may want to diagnose the use of a trigraph in souce
        // but for now we will just leave this here.

        if (trigraph == '\\' && lexer->current_ptr[*peek + 3] == '\n') 
        {
            *peek += 4;
            return get_char_and_size(lexer, peek);
        } 
        else 
        {
            *peek += 3;
            return trigraph;
        }
    }

    // Not a trigraph or bs newline
    *peek += 1;
    return curr;
}

// Get the current raw character from the stream designed to be as fast as
// possible. Could easily be inlined too
static char get_curr_char_raw(Lexer* lexer)
{
    return lexer->current_ptr[0];
}

// Quickly consume the current raw character
static void consume_curr_char_raw(Lexer* lexer)
{
    lexer->current_ptr++;
}

// Get the current char but don't advance the lexer
static char get_curr_char(Lexer* lexer)
{
    size_t len = 0;
    return get_char_and_size(lexer, &len);
}

// Get the next char and advance the lexer
static char get_next_char(Lexer* lexer)
{
    size_t len = 0;
    const char current = get_char_and_size(lexer, &len);
    seek(lexer, len);
    return current;
}

// Consume the current char
static void consume_char(Lexer* lexer)
{
    size_t len = 0;
    (void) get_char_and_size(lexer, &len);
    seek(lexer, len);
}

// A hacky way to peek a char. Will leave this here since it is only used in one
// spot test if we got an elipsis
static char peek_char(Lexer* lexer)
{
    // Get the save point
    char* pos = get_position(lexer);

    // Consume a char
    consume_char(lexer);

    // Get the 'current' char i.e. really the next one
    char peek = get_curr_char(lexer);

    // Reset the position
    set_position(lexer, pos);

    // Return the char
    return peek;
}

// Are we at eof
static bool at_eof(Lexer* lexer)
{
    return lexer->current_ptr >= lexer->buffer_end;
}

// Get the current position
static Location get_curr_location(Lexer* lexer)
{
    const Location offset = lexer->current_ptr - lexer->buffer_start;
    return (lexer->start_loc + offset);
}

static void reset_token(Token* token)
{
    *token = (Token) {0};
}

static void skip_line_comment(Lexer* lexer)
{
    do
    {
        char current = get_curr_char(lexer);
        if (current == '\r' || current == '\n')
        {
            break;
        }

        if (current == '\0' && at_eof(lexer))
        {
            // TODO: maybe issue warning about comment ending the thing
            break;
        }

        consume_char(lexer);
    } while (true);
}

static void skip_block_comment(Lexer* lexer)
{
    do {
        char current = get_curr_char(lexer);

        if (current == '*')
        {
            consume_char(lexer);

            current = get_curr_char(lexer);

            if (current == '/')
            {
                consume_char(lexer);

                break;
            }
        }

        if (current == '\0' && at_eof(lexer))
        {
            /* TODO: issue warning about unterminated comment */
            break;
        }

        consume_char(lexer);
    } while (true);
}

static bool lex_number(Lexer* lexer, Token* token, char* start)
{
    token->type = TOKEN_NUMBER;

    // Reset the position to the start and create a buffer for us to build
    set_position(lexer, start);

    Buffer number = buffer_new_size(NUMBER_START_SIZE);

    // TODO: note that numbers can contain ucn's so will need to handle that
    // eventually
    while (true)
    {
        char current = get_curr_char(lexer);

        // We know that the first character will always be a number or a dot
        // and then subsequent characters can be identifiers or dots
        if (!(is_identifier(current) || current == '.'))
        {
            break;
        }

        buffer_add_char(&number, current);
        consume_char(lexer);

        // If we get e+ or E- make sure that this is lexed correctly
        if (current == 'e' || current == 'E' || current == 'p' || current == 'P')
        {
            current = get_curr_char(lexer);
            if (current == '-' || current == '+')
            {
                buffer_add_char(&number, current);
                consume_char(lexer);
            }
        }
    }

    // Finish the number construction
    buffer_make_cstr(&number);

    token->opt_value = string_from_buffer(&number);

    return true;
}

// TODO: I would like to make this more modular to better support older / newer
// language standards eventually...
static void classify_identifier(Token* token)
{
    assert(token->type == TOKEN_IDENTIFIER);
    assert(token->opt_value.len > 0);

    switch (string_get(&token->opt_value, 0))
    {
        case 'a':
            if (string_equal(&token->opt_value, "auto"))
            {
                token->type = TOKEN_AUTO;
            }
            break;
        
        case 'b':
            if (string_equal(&token->opt_value, "break"))
            {
                token->type = TOKEN_BREAK;
            }
            break;

        case 'c':
            if (string_equal(&token->opt_value, "case"))
            {
                token->type = TOKEN_CASE;
            }
            else if (string_equal(&token->opt_value, "char"))
            {
                token->type = TOKEN_CHAR;
            }
            else if (string_equal(&token->opt_value, "const"))
            {
                token->type = TOKEN_CONST;
            }
            else if (string_equal(&token->opt_value, "continue"))
            {
                token->type = TOKEN_CONTINUE;
            }
            break;

        case 'd':
            if (string_equal(&token->opt_value, "default"))
            {
                token->type = TOKEN_DEFAULT;
            }
            else if (string_equal(&token->opt_value, "do"))
            {
                token->type = TOKEN_DO;
            }
            else if (string_equal(&token->opt_value, "double"))
            {
                token->type = TOKEN_DOUBLE;
            }
            break;

        case 'e':
            if (string_equal(&token->opt_value, "else"))
            {
                token->type = TOKEN_ELSE;
            }
            else if (string_equal(&token->opt_value, "enum"))
            {
                token->type = TOKEN_ENUM;
            }
            else if (string_equal(&token->opt_value, "extern"))
            {
                token->type = TOKEN_EXTERN;
            }
            break;

        case 'f':
            if (string_equal(&token->opt_value, "float"))
            {
                token->type = TOKEN_FLOAT;
            }
            else if (string_equal(&token->opt_value, "for"))
            {
                token->type = TOKEN_FOR;
            }
            break;

        case 'g':
            if (string_equal(&token->opt_value, "goto"))
            {
                token->type = TOKEN_GOTO;
            }
            break;

        case 'i':
            if (string_equal(&token->opt_value, "if"))
            {
                token->type = TOKEN_IF;
            }
            else if (string_equal(&token->opt_value, "inline"))
            {
                token->type = TOKEN_INLINE;
            }
            else if (string_equal(&token->opt_value, "int"))
            {
                token->type = TOKEN_INT;
            }
            break;

        case 'l':
            if (string_equal(&token->opt_value, "long"))
            {
                token->type = TOKEN_LONG;
            }
            break;

        case 'r':
            if (string_equal(&token->opt_value, "register"))
            {
                token->type = TOKEN_REGISTER;
            }
            else if (string_equal(&token->opt_value, "restrict"))
            {
                token->type = TOKEN_RESTRICT;
            }
            else if (string_equal(&token->opt_value, "return"))
            {
                token->type = TOKEN_RETURN;
            }
            break;

        case 's':
            if (string_equal(&token->opt_value, "short"))
            {
                token->type = TOKEN_SHORT;
            }
            else if (string_equal(&token->opt_value, "signed"))
            {
                token->type = TOKEN_SIGNED;
            }
            else if (string_equal(&token->opt_value, "sizeof"))
            {
                token->type = TOKEN_SIZEOF;
            }            
            else if (string_equal(&token->opt_value, "static"))
            {
                token->type = TOKEN_STATIC;
            }
            else if (string_equal(&token->opt_value, "struct"))
            {
                token->type = TOKEN_STRUCT;
            }
            else if (string_equal(&token->opt_value, "switch"))
            {
                token->type = TOKEN_SWITCH;
            }
            break;

        case 't':
            if (string_equal(&token->opt_value, "typedef"))
            {
                token->type = TOKEN_TYPEDEF;
            }
            break;

        case 'u':
            if (string_equal(&token->opt_value, "union"))
            {
                token->type = TOKEN_UNION;
            }
            else if (string_equal(&token->opt_value, "unsigned"))
            {
                token->type = TOKEN_UNSIGNED;
            }
            break;

        case 'v':
            if (string_equal(&token->opt_value, "volatile"))
            {
                token->type = TOKEN_VOLATILE;
            }
            else if (string_equal(&token->opt_value, "void"))
            {
                token->type = TOKEN_VOID;
            }
            break;

        case 'w':
            if (string_equal(&token->opt_value, "while"))
            {
                token->type = TOKEN_WHILE;
            }
            break;

        case '_':
            if (string_equal(&token->opt_value, "_Bool"))
            {
                token->type = TOKEN__BOOL;
            }
            else if (string_equal(&token->opt_value, "_Complex"))
            {
                token->type = TOKEN__COMPLEX;
            }
            else if (string_equal(&token->opt_value, "_Imaginary"))
            {
                token->type = TOKEN__IMAGINARY;
            }
            else if (string_equal(&token->opt_value, "__func__"))
            {
                token->type = TOKEN___FUNC__;
            }
            break;

        default:
            break;
    }

    // We also want to remove the allocated memory if any so we don't leak
    if (token->type != TOKEN_IDENTIFIER)
    {
        token_free_data(token);
    }
}

static bool lex_identifier(Lexer* lexer, Token* token, char* start)
{
    token->type = TOKEN_IDENTIFIER;

    // Reset the position to the start and create a buffer for us to build
    set_position(lexer, start);

    Buffer identifier = buffer_new_size(IDENTIFIER_START_SIZE);

    // TODO: handle universal characters
    while (true)
    {
        const char current = get_curr_char(lexer);

        if (!is_identifier(current))
        {
            break;
        }

        buffer_add_char(&identifier, current);
        consume_char(lexer);
    }

    // Finish building the identifier
    buffer_make_cstr(&identifier);

    token->opt_value = string_from_buffer(&identifier);

    // TODO: classify the identifier into catagories
    classify_identifier(token);

    return true;
}

static char get_starting_delimiter(TokenType type)
{
    switch (type)
    {
        case TOKEN_WIDE_STRING:
        case TOKEN_STRING:
            return '"';

        case TOKEN_WIDE_CHARACTER:
        case TOKEN_CHARACTER:
            return '\'';

        case TOKEN_PP_HEADER_NAME:
            return '<';

        default:
            panic("unreachable");
            return '\0';
    }
}

static char get_ending_delimiter(TokenType type)
{
    switch (type)
    {
        case TOKEN_WIDE_STRING:
        case TOKEN_STRING:
            return '"';

        case TOKEN_WIDE_CHARACTER:
        case TOKEN_CHARACTER:
            return '\'';

        case TOKEN_PP_HEADER_NAME:
            return '>';

        default:
            panic("unreachable");
            return '\0';
    }
}

static bool is_wide(TokenType type)
{
    switch (type)
    {
        case TOKEN_WIDE_CHARACTER:
        case TOKEN_WIDE_STRING:
            return true;

        default:
            return false;
    }
}

static bool lex_string_like_literal(Lexer* lexer, Token* token, TokenType type)
{
    token->type = type;

    const char ending_delim = get_ending_delimiter(type);

    Buffer string = buffer_new_size(STRING_START_SIZE);
    if (is_wide(type))
    {
        buffer_add_char(&string, 'L');
    }
    buffer_add_char(&string, get_starting_delimiter(type));

    char current = get_next_char(lexer);
    while (current != ending_delim)
    {
        // Escaped character get the next character but dont add just yet...
        if (current == '\\')
        {
            buffer_add_char(&string, '\\');

            current = get_next_char(lexer);
        }

        // Below are two bad cases. The first one is that we got a newline in
        // the string. I.e. an unterminated string. We just end the token here
        // the next one is that we got end of file during the string.
        if (current == '\r' || current == '\n')
        {
            token->type = TOKEN_UNKNOWN;
            goto finish_string;
        } 
        else if (current == '\0' && at_eof(lexer))
        {
            token->type = TOKEN_UNKNOWN;
            goto finish_string;
        }

        // Finally add the char and get the next one
        buffer_add_char(&string, current);

        current = get_next_char(lexer);
    }

    // This should only be added when we get a well formed string literal
    buffer_add_char(&string, ending_delim);

    // Finish building the string
finish_string:
    buffer_make_cstr(&string);

    // Invalid character constants since they have nothing in them...
    if (is_wide(type) && buffer_get_len(&string) == 3)
    {
        token->type = TOKEN_UNKNOWN;
    }
    else if (buffer_get_len(&string) == 2)
    {
        token->type = TOKEN_UNKNOWN;
    }

    token->opt_value = string_from_buffer(&string);

    return true;
}

static bool lex_string_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOKEN_STRING);
}

static bool lex_wide_string_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOKEN_WIDE_STRING);
}

static bool lex_character_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOKEN_CHARACTER);
}

static bool lex_wide_character_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOKEN_WIDE_CHARACTER);
}

static bool lex_header_name(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOKEN_PP_HEADER_NAME);
}

static bool lex_internal(Lexer* lexer, Token* token)
{
    bool whitespace = false;

retry_lexing:;
    // Eat any leading horizonal whitespace (quick version)
    if (is_horizontal_whitespace(get_curr_char_raw(lexer)))
    {
        whitespace = true;
        do 
        {
            consume_curr_char_raw(lexer);
        } while (is_horizontal_whitespace(get_curr_char_raw(lexer)));
    }

    // Set up the token here
    char* token_start = get_position(lexer);

    Location token_location = get_curr_location(lexer);

    token->type = TOKEN_UNKNOWN;
    token->loc = token_location;
    token->opt_value = (String) {0};
    token->leading_space = whitespace;
    token->start_of_line = lexer->start_of_line;
    token->disable_expand = false;

    // Set it to false even if it was before
    lexer->start_of_line = false;

    // Here we will do our actual lexing
    char curr = get_next_char(lexer);

    switch (curr)
    {
        case '\0':
            if (at_eof(lexer))
            {
                token->type = TOKEN_EOF;

                return false;
            }
            
            whitespace = true;

            goto retry_lexing;

        // Our special whitespace tokens skip all of the white space that we can
        // and go back to the start but ensure that we set the whitespace flag
        case ' ':
        case '\t':
        case '\f':
        case '\v':
            while (is_horizontal_whitespace(get_curr_char_raw(lexer)))
            {
                consume_curr_char_raw(lexer);
            }
            whitespace = true;

            goto retry_lexing;
        
        case '\r':
            // \r\n line endings, simply consume the char if a '\n' is next and
            // then fallthrough to the newline code. Also works for '\r' only
            // line endings
            if (get_curr_char_raw(lexer) == '\n')
            {
                consume_curr_char_raw(lexer);
            }

            /* FALLTHROUGH */

        case '\n':
            // If we are in a PP directive finish it up
            if (lexer->lexing_directive)
            {
                // Reset our lexing flags and set the token type
                lexer->lexing_directive = false;
                lexer->start_of_line = true;

                token->type = TOKEN_PP_EOD;

                break;
            }

            // Reset the start of line flag
            lexer->start_of_line = true;

            // No whitespace before token now
            whitespace = false;

            // now retry this all
            goto retry_lexing;

        // Number cases
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return lex_number(lexer, token, token_start);

        case 'L':
            curr = get_curr_char(lexer);
            if (curr == '"')
            {
                consume_char(lexer);

                return lex_wide_string_literal(lexer, token);
            }
            else if (curr == '\'')
            {
                consume_char(lexer);

                return lex_wide_character_literal(lexer, token);
            }

            /* FALLTHROUGH */
        
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
        case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case '_':
            return lex_identifier(lexer, token, token_start);

        // String and character literals
        case '"':
            return lex_string_literal(lexer, token);

        case '\'':
            return lex_character_literal(lexer, token);
        
        case '.':
            curr = get_curr_char(lexer);

            if (is_numeric(curr))
            {
                return lex_number(lexer, token, token_start);
            }
            else if (curr == '.' && peek_char(lexer) == '.')
            {
                token->type = TOKEN_ELIPSIS;

                consume_char(lexer);
                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_DOT;
            }
            break;

        case '/':
            curr = get_curr_char(lexer);

            if (curr == '/')
            {
                consume_char(lexer);

                skip_line_comment(lexer);

                // Make sure to unset whitespace
                whitespace = false;

                goto retry_lexing;
            }
            else if (curr == '*')
            {
                consume_char(lexer);

                skip_block_comment(lexer);

                whitespace = true;

                goto retry_lexing;
            }
            else if (curr == '=')
            {
                token->type = TOKEN_SLASH_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_SLASH;
            }
            break;

            case '*':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOKEN_STAR_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_STAR;
            }
            break;
        
        case '%':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOKEN_PERCENT_EQUAL;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOKEN_RBRACKET;

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                /* Here we either got %: or %:%: */

                consume_char(lexer);

                if (get_curr_char(lexer) == '%' && peek_char(lexer) == ':')
                {
                    token->type = TOKEN_HASH_HASH;

                    consume_char(lexer);
                    consume_char(lexer);
                }
                else
                {
                    token->type = TOKEN_HASH;
                }
            }
            else
            {
                token->type = TOKEN_PERCENT;
            }
            break;

        case '+':
            curr = get_curr_char(lexer);
            if (curr == '+')
            {
                token->type = TOKEN_PLUS_PLUS;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOKEN_PLUS_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_PLUS;
            }
            break;

        case '-':
            curr = get_curr_char(lexer);
            if (curr == '-')
            {
                token->type = TOKEN_MINUS_MINUS;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOKEN_ARROW;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOKEN_MINUS_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_MINUS;
            }
            break;

        case '|':
            curr = get_curr_char(lexer);
            if (curr == '|')
            {
                token->type = TOKEN_OR_OR;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOKEN_OR_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_OR;
            }
            break;

        case '&':
            curr = get_curr_char(lexer);
            if (curr == '&')
            {
                token->type = TOKEN_AND_AND;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOKEN_AND_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_AND;
            }
            break;

        case '^':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOKEN_XOR_EQUAL;
                
                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_XOR;
            }
            break;

        case '=':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOKEN_EQUAL_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_EQUAL;
            }
            break;

        case '!':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOKEN_NOT_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_NOT;
            }
            break;

        case '#':
            curr = get_curr_char(lexer);
            if (curr == '#')
            {
                token->type = TOKEN_HASH_HASH;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_HASH;
            }
            break;
        
        case '<':
            if (lexer->can_lex_header)
            {
                return lex_header_name(lexer, token);
            }

            curr = get_curr_char(lexer);
            if (curr == '<')
            {
                consume_char(lexer);

                curr = get_curr_char(lexer);
                if (curr == '=')
                {
                    token->type = TOKEN_LT_LT_EQUAL;

                    consume_char(lexer);
                }
                else
                {
                    token->type = TOKEN_LT_LT;
                }
            }
            else if (curr == '=')
            {
                token->type = TOKEN_LT_EQUAL;

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                token->type = TOKEN_LBRACKET;

                consume_char(lexer);
            }
            else if (curr == '%')
            {
                token->type = TOKEN_LBRACKET;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_LT;
            }
            break;

        case '>':
            curr = get_curr_char(lexer);
            if (curr == '>')
            {
                consume_char(lexer);

                curr = get_curr_char(lexer);
                if (curr == '=')
                {
                    token->type = TOKEN_GT_GT_EQUAL;

                    consume_char(lexer);
                }
                else
                {
                    token->type = TOKEN_GT_GT;
                }
            }
            else if (curr == '=')
            {
                token->type = TOKEN_GT_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOKEN_GT;
            }
            break;

        case ':': 
            curr = get_curr_char(lexer);
            if (curr == '>')
            {
                token->type = TOKEN_RBRACKET;

                consume_char(lexer);
            } 
            else
            {
                token->type = TOKEN_COLON;
            }
            break;

        // All single character tokens in c99
        case '[': token->type = TOKEN_LBRACKET; break;
        case ']': token->type = TOKEN_RBRACKET; break;
        case '(': token->type = TOKEN_LPAREN; break;
        case ')': token->type = TOKEN_RPAREN; break;
        case '{': token->type = TOKEN_LCURLY; break;
        case '}': token->type = TOKEN_RCURLY; break;
        case '?': token->type = TOKEN_QUESTION; break;
        case ';': token->type = TOKEN_SEMI; break;
        case ',': token->type = TOKEN_COMMA; break;
        case '~': token->type = TOKEN_TILDE; break;

        default: // Currently just create an unknown token and die
        {
            Buffer unknown = buffer_new_size(2);
            
            buffer_add_char(&unknown, curr);
            buffer_make_cstr(&unknown);

            token->opt_value = string_from_buffer(&unknown);

            // TODO: maybe warn of an unknown token?
        }
        break;
    }

    return true;
}

bool lexer_get_next(Lexer* lexer, Token* token)
{
    reset_token(token);

    return lex_internal(lexer, token);
}
