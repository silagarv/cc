#include "lexer.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "util/panic.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"

#include "files/location.h"

#include "lex/char_help.h"
#include "lex/unicode.h"
#include "lex/token.h"

#define MAX_UCN_LENGTH (8)

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

static char get_char_fast(Lexer* lexer)
{
    return *lexer->current_ptr;
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

// note that these functions are nice enough that hopefully they can be
// inlined into the non-raw versions. Also note that these are here to enable
// us to get the spelling of the token
static char get_char_and_size_slow_raw(const char* curr_ptr, const char* end_ptr, 
        size_t* peek, char curr);
static char get_char_and_size_raw(const char* curr_ptr, const char* end_ptr,
        size_t* peek);

static char get_char_and_size_raw(const char* curr_ptr, const char* end_ptr,
        size_t* peek)
{
    const char current = curr_ptr[*peek];

    if (is_simple_char(current))
    {
        *peek += 1;
        return current;
    }
    
    return get_char_and_size_slow_raw(curr_ptr, end_ptr, peek, current);
}

static char get_char_and_size_slow_raw(const char* curr_ptr, const char* end_ptr, 
        size_t* peek, char curr)
{
    assert(!is_simple_char(curr));

    const char next = curr_ptr[*peek + 1];

    if (curr == '\\' && next == '\n')
    {
        *peek += 2;

        // We need to check here if we are at the end of the buffer. Could also
        // possibly add a warning here in the future. This is necessary to
        // prevent peek increasing by another and then the possible later call
        // to function 'seek' failing.
        if (curr_ptr + *peek == end_ptr)
        {
            return '\0';
        }

        return get_char_and_size_raw(curr_ptr, end_ptr, peek);
    }

    if (curr == '?' && next == '?')
    {
        const char trigraph = get_trigraph(curr_ptr[*peek + 2]);
        const bool has_trigraph = (trigraph != '\0');

        if (!has_trigraph)
        {
            *peek += 1;
            return '?';
        }

        // In the future we may want to diagnose the use of a trigraph in souce
        // but for now we will just leave this here.

        if (trigraph == '\\' && curr_ptr[*peek + 3] == '\n') 
        {
            *peek += 4;
            return get_char_and_size_raw(curr_ptr, end_ptr, peek);
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

static char get_char_and_size_slow(const Lexer* lexer, size_t* peek, char curr);
static char get_char_and_size(const Lexer* lexer, size_t* peek);

// Get a the next character and the number of char's it took to get to it 
static char get_char_and_size(const Lexer* lexer, size_t* peek)
{
    return get_char_and_size_raw(lexer->current_ptr, lexer->buffer_end, peek);
}

// Note this is strict on the rules in the c standard. Maybe we don't want
// to be this strict in the future. So this method could be subject to change.
static char get_char_and_size_slow(const Lexer* lexer, size_t* peek, char curr)
{
    return get_char_and_size_slow_raw(lexer->current_ptr, lexer->buffer_end, 
            peek, curr);
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

// Peek a char by calling get_char_and_size twice whilst retaining the peek in
// between the calls. A much better solution then before
static char peek_char(Lexer* lexer)
{
    size_t peek = 0;
    (void) get_char_and_size(lexer, &peek);
    return get_char_and_size(lexer, &peek);
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

static Location get_ending_location(Lexer* lexer)
{
    const Location offset = lexer->current_ptr - lexer->buffer_start;
    return (lexer->start_loc + offset - 1);
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

static bool try_lex_ucn(Lexer* lexer, Token* token, Buffer* buffer, utf32* value)
{
    assert(get_curr_char(lexer) == '\\');

    // First save the current point in the case the ucn is invalid
    char* save_point = get_position(lexer);

    // Skip over the '\\'
    consume_char(lexer);

    // Now get the next character and check if it is a u
    const char ucn_type = get_next_char(lexer);

    // We didn't get a ucn. Restore the save point and continue
    if (ucn_type != 'U' && ucn_type != 'u')
    {
        set_position(lexer, save_point);

        return false;
    }

    // Now we know we want to lex a ucn so try and store the result in a temp
    // buffer until we can confirm it is valid
    const size_t required_digits = (ucn_type == 'U') ? 8 : 4;
    char ucn_buffer[MAX_UCN_LENGTH];

    size_t num_digits;
    for (num_digits = 0; num_digits < required_digits; num_digits++)
    {
        char current = get_next_char(lexer);

        // If we didn't get a hex digit we can just return and leave early
        if (!is_hexadecimal(current))
        {
            set_position(lexer, save_point);

            return false;
        }

        ucn_buffer[num_digits] = current;
    }

    assert(num_digits == required_digits);

    // TODO: we actually want to convert to utf8 and add the the buffer instead
    // TODO: we also need to make the lexing of identifiers a bit different
    // since we are going to do this as well

    // If were here we know we got the required number of digits so let's add
    // them to our buffer along with the leading '\u' or '\U' :)
    buffer_add_char(buffer, '\\');
    buffer_add_char(buffer, ucn_type);  
    for (size_t i = 0; i < required_digits; i++)
    {
        buffer_add_char(buffer, ucn_buffer[i]);
    }

    // Now we also need to calculate the value of the universal character to
    // ensure that we get a valid ucn within an identifier
    *value = 0;
    for (size_t i = 0; i < required_digits; i++)
    {
        *value *= 16;
        *value += convert_hexadecimal(ucn_buffer[i]);
    }

    return true;
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

        if (is_identifier(current) || current == '.')
        {
            buffer_add_char(&number, current);
            consume_char(lexer);

            if (current == 'e' || current == 'E' 
                    || current == 'p' || current == 'P')
            {
                current = get_curr_char(lexer);
                if (current == '-' || current == '+')
                {
                    buffer_add_char(&number, current);
                    consume_char(lexer);
                }
            }

            continue;
        }

        if (current == '\\')
        {
            uint32_t value;
            if (!try_lex_ucn(lexer, token, &number, &value))
            {
                break;
            }

            // If we got a correct ammount of numbers then we consider it well
            // formed even if it fails the next check of the range being valid
            // TODO: check ucn range
            if (!is_valid_ucn(value))
            {
                // TODO: implement error on invalid ucn value...

                // panic("invalid ucn value");
            }
            continue;
        }

        break;
    }

    // Finish the number construction
    buffer_make_cstr(&number);
    token->data = token_create_literal_node(string_from_buffer(&number));

    return true;
}

static void classify_pp_identifier(Token* token)
{
    assert(token_is_identifier(token));

    const String* string = &token->data.identifier->value;
    switch (string_get(string, 0))
    {
        case 'd':
            if (token_equal_string(token, "define"))
            {
                token->type = TOKEN_PP_DEFINE;
            }
            break;

        case 'e':
            if (token_equal_string(token, "elif"))
            {
                token->type= TOKEN_PP_ELIF;
            }
            else if (token_equal_string(token, "else"))
            {
                token->type= TOKEN_PP_ELSE;
            }
            else if (token_equal_string(token, "endif"))
            {
                token->type= TOKEN_PP_ENDIF;
            }
            else if (token_equal_string(token, "error"))
            {
                token->type= TOKEN_PP_ERROR;
            }
            break;

        case 'i':
            if (token_equal_string(token, "if"))
            {
                token->type= TOKEN_PP_IF;
            }
            else if (token_equal_string(token, "ifdef"))
            {
                token->type= TOKEN_PP_IFDEF;
            }
            else if (token_equal_string(token, "ifndef"))
            {
                token->type= TOKEN_PP_IFNDEF;
            }
            else if (token_equal_string(token, "include"))
            {
                token->type= TOKEN_PP_INCLUDE;
            }
            break;

        case 'l':
            if (token_equal_string(token, "line"))
            {
                token->type= TOKEN_PP_LINE;
            }
            break;
        
        case 'p':
            if (token_equal_string(token, "pragma"))
            {
                token->type= TOKEN_PP_PRAGMA;
            }
            break;

        case 'u':
            if (token_equal_string(token, "undef"))
            {
                token->type= TOKEN_PP_UNDEF;
            }
            break;
        
        default:
            break;
    }

}

// TODO: I would like to make this more modular to better support older / newer
// language standards eventually...
static void classify_identifier(Token* token)
{
    assert(token_is_identifier(token));

    const String* string = &token->data.identifier->value;
    switch (string_get(string, 0))
    {
        case 'a':
            if (token_equal_string(token, "auto"))
            {
                token->type = TOKEN_AUTO;
            }
            break;
        
        case 'b':
            if (token_equal_string(token, "break"))
            {
                token->type = TOKEN_BREAK;
            }
            break;

        case 'c':
            if (token_equal_string(token, "case"))
            {
                token->type = TOKEN_CASE;
            }
            else if (token_equal_string(token, "char"))
            {
                token->type = TOKEN_CHAR;
            }
            else if (token_equal_string(token, "const"))
            {
                token->type = TOKEN_CONST;
            }
            else if (token_equal_string(token, "continue"))
            {
                token->type = TOKEN_CONTINUE;
            }
            break;

        case 'd':
            if (token_equal_string(token, "default"))
            {
                token->type = TOKEN_DEFAULT;
            }
            else if (token_equal_string(token, "do"))
            {
                token->type = TOKEN_DO;
            }
            else if (token_equal_string(token, "double"))
            {
                token->type = TOKEN_DOUBLE;
            }
            break;

        case 'e':
            if (token_equal_string(token, "else"))
            {
                token->type = TOKEN_ELSE;
            }
            else if (token_equal_string(token, "enum"))
            {
                token->type = TOKEN_ENUM;
            }
            else if (token_equal_string(token, "extern"))
            {
                token->type = TOKEN_EXTERN;
            }
            break;

        case 'f':
            if (token_equal_string(token, "float"))
            {
                token->type = TOKEN_FLOAT;
            }
            else if (token_equal_string(token, "for"))
            {
                token->type = TOKEN_FOR;
            }
            break;

        case 'g':
            if (token_equal_string(token, "goto"))
            {
                token->type = TOKEN_GOTO;
            }
            break;

        case 'i':
            if (token_equal_string(token, "if"))
            {
                token->type = TOKEN_IF;
            }
            else if (token_equal_string(token, "inline"))
            {
                token->type = TOKEN_INLINE;
            }
            else if (token_equal_string(token, "int"))
            {
                token->type = TOKEN_INT;
            }
            break;

        case 'l':
            if (token_equal_string(token, "long"))
            {
                token->type = TOKEN_LONG;
            }
            break;

        case 'r':
            if (token_equal_string(token, "register"))
            {
                token->type = TOKEN_REGISTER;
            }
            else if (token_equal_string(token, "restrict"))
            {
                token->type = TOKEN_RESTRICT;
            }
            else if (token_equal_string(token, "return"))
            {
                token->type = TOKEN_RETURN;
            }
            break;

        case 's':
            if (token_equal_string(token, "short"))
            {
                token->type = TOKEN_SHORT;
            }
            else if (token_equal_string(token, "signed"))
            {
                token->type = TOKEN_SIGNED;
            }
            else if (token_equal_string(token, "sizeof"))
            {
                token->type = TOKEN_SIZEOF;
            }            
            else if (token_equal_string(token, "static"))
            {
                token->type = TOKEN_STATIC;
            }
            else if (token_equal_string(token, "struct"))
            {
                token->type = TOKEN_STRUCT;
            }
            else if (token_equal_string(token, "switch"))
            {
                token->type = TOKEN_SWITCH;
            }
            break;

        case 't':
            if (token_equal_string(token, "typedef"))
            {
                token->type = TOKEN_TYPEDEF;
            }
            break;

        case 'u':
            if (token_equal_string(token, "union"))
            {
                token->type = TOKEN_UNION;
            }
            else if (token_equal_string(token, "unsigned"))
            {
                token->type = TOKEN_UNSIGNED;
            }
            break;

        case 'v':
            if (token_equal_string(token, "volatile"))
            {
                token->type = TOKEN_VOLATILE;
            }
            else if (token_equal_string(token, "void"))
            {
                token->type = TOKEN_VOID;
            }
            break;

        case 'w':
            if (token_equal_string(token, "while"))
            {
                token->type = TOKEN_WHILE;
            }
            break;

        case '_':
            if (token_equal_string(token, "_Bool"))
            {
                token->type = TOKEN__BOOL;
            }
            else if (token_equal_string(token, "_Complex"))
            {
                token->type = TOKEN__COMPLEX;
            }
            else if (token_equal_string(token, "_Imaginary"))
            {
                token->type = TOKEN__IMAGINARY;
            }
            else if (token_equal_string(token, "__func__"))
            {
                token->type = TOKEN___FUNC__;
            }
            break;

        default:
            break;
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
        char current = get_curr_char(lexer);
        
        // Simple identifier like character
        if (is_identifier(current))
        {
            buffer_add_char(&identifier, current);
            consume_char(lexer);
            continue;
        }

        // Possible universal character but we need to check for it and if it is
        // then we can continue
        if (current == '\\')
        {
            utf32 value;
            if (!try_lex_ucn(lexer, token, &identifier, &value))
            {
                break;
            }

            // If we got a correct ammount of numbers then we consider it well
            // formed even if it fails the next check of the range being valid
            // TODO: check ucn range
            if (!is_valid_ucn(value))
            {
                // TODO: implement error on invalid ucn value...

                // panic("invalid ucn value");
            }
            continue;
        }

        break;
    }

    // Finish building the identifier and create the token data
    buffer_make_cstr(&identifier);
    token->data = token_create_identifier_node(string_from_buffer(&identifier));

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

static bool is_character_like(TokenType type)
{
    switch (type) 
    {
        case TOKEN_CHARACTER:
        case TOKEN_WIDE_CHARACTER:
            return true;

        default:
            return false;
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
    if (is_character_like(type))
    {
        if (is_wide(type) && buffer_get_len(&string) == 3)
        {
            token->type = TOKEN_UNKNOWN;
        }
        else if (buffer_get_len(&string) == 2)
        {
            token->type = TOKEN_UNKNOWN;
        }
    }

    token->data = token_create_literal_node(string_from_buffer(&string));

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

    // Set up the token here...
    char* token_start = get_position(lexer);

    Location token_location = get_curr_location(lexer);

    token->loc = token_location;
    token->end = token_location;

    token->type = TOKEN_UNKNOWN;

    // Make sure our token flags are correctly set up
    token_set_flag(token, whitespace ? TOKEN_FLAG_WHITESPACE : TOKEN_FLAG_NONE);
    token_set_flag(token, lexer->start_of_line ? TOKEN_FLAG_BOL: TOKEN_FLAG_NONE);
    token_unset_flag(token, TOKEN_FLAG_DISABLE_EXPAND);

    token->data = (TokenData) {0};

    // Set it to false even if it was true before before
    lexer->start_of_line = false;

    // TODO: we should convert all get_next_char / get_curr_char calls into
    // get_char_and_size calls, then we might have a faster lexer??? since
    // currently we are forcing the lexer to redo alot of the work which might
    // not be noticeable on small files but on longer files it might definitely
    // add up

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
            lex_number(lexer, token, token_start);

            break;

        case 'L':
            curr = get_curr_char(lexer);
            if (curr == '"')
            {
                consume_char(lexer);

                lex_wide_string_literal(lexer, token);

                break;
            }
            else if (curr == '\'')
            {
                consume_char(lexer);

                lex_wide_character_literal(lexer, token);

                break;
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
            lex_identifier(lexer, token, token_start);

            break;

        // String and character literals
        case '"':
            lex_string_literal(lexer, token);

            break;

        case '\'':
            lex_character_literal(lexer, token);

            break;
        
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
                token->type = TOKEN_RCURLY;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                /* Here we either got %: or %:%: */

                consume_char(lexer);

                if (get_curr_char(lexer) == '%' && peek_char(lexer) == ':')
                {
                    token->type = TOKEN_HASH_HASH;

                    token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                    consume_char(lexer);
                    consume_char(lexer);
                }
                else
                {
                    token->type = TOKEN_HASH;

                    token_set_flag(token, TOKEN_FLAG_DIGRAPH);
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

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == '%')
            {
                token->type = TOKEN_LCURLY;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

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

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

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

        case '\\': // TODO: ucn starting an identifier
            curr = get_curr_char(lexer);

            if (curr == 'u')
            {
                panic("currently an identifier cannot start a ucn");
            }

            // RESET TO '\\' FOR NOW ONLY TO CREATE AN UNKNOWN TOKEN TYPE
            curr = '\\';

            /* FALLTHROUGH */

        default: // Create an unknown token
            if (!is_ascii(curr))
            {
                utf32 value;
                bool conversion_success = utf8_to_utf32((unsigned char**) &token_start, 
                        (unsigned char*) lexer->buffer_end, &value);
                
                // TODO: figure out what to do with the value once we have it
                // probably use the c99 status of if a codepoint is a valid
                // identifier an go from there

                panic("Non ascii character encountered");
            }

            token->type = TOKEN_UNKNOWN;

            Buffer unknown = buffer_from_format("%c", curr);
            token->data = token_create_literal_node(string_from_buffer(&unknown));
            break;
    }

    // Here we need to get the final location of the token
    token->end = get_ending_location(lexer);

    return true;
}

bool lexer_get_next(Lexer* lexer, Token* token)
{
    reset_token(token);

    return lex_internal(lexer, token);
}

void token_get_spelling(const Token* token, Buffer buff)
{
    return;
}

void token_stringify(const Token* token, Buffer buffer)
{
    return;
}
