#include "lexer.h"

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "driver/diagnostic.h"
#include "driver/lang.h"
#include "files/file_manager.h"
#include "files/source_manager.h"
#include "util/arena.h"
#include "util/panic.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"

#include "files/location.h"

#include "lex/char_help.h"
#include "lex/unicode.h"
#include "lex/token.h"
#include "lex/identifier_table.h"

#define MAX_UCN_LENGTH (8)

#define IDENTIFIER_START_SIZE (10)
#define NUMBER_START_SIZE (10)
#define STRING_START_SIZE (10)

void lexer_create(Lexer* lexer, DiagnosticManager* dm, LangOptions* opts,
        Arena* literal_arena, IdentifierTable* identifiers, SourceFile* source)
{
    const FileBuffer* fb = source_file_get_buffer(source);
    *lexer = (Lexer)
    {
        .dm = dm,
        .lang = opts,
        .literal_arena = literal_arena,
        .identifiers = identifiers,
        .buffer_start = file_buffer_get_start(fb),
        .buffer_end = file_buffer_get_end(fb),
        .current_ptr = (char*) file_buffer_get_start(fb),
        .start_loc = source_file_get_start_location(source),
        .start_of_line = true,
        .lexing_directive = false,
        .can_lex_header = false
    };
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
    char current = curr_ptr[*peek];

    if (is_simple_char(current))
    {
        *peek += 1;

        return current;
    }
    
    return get_char_and_size_slow_raw(curr_ptr, end_ptr, peek, current);
}

static char get_char_and_size_slow_raw(const char* curr_ptr,
        const char* end_ptr, size_t* peek, char curr)
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
    char current = get_char_and_size(lexer, &len);
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
static Location get_location_from(const Lexer* lexer, const char* position)
{
    return lexer->start_loc + position - lexer->buffer_start;
}

static Location get_curr_location(const Lexer* lexer)
{
    return get_location_from(lexer, lexer->current_ptr);
}

static Location get_ending_location(const Lexer* lexer)
{
    return get_location_from(lexer, lexer->current_ptr) - 1;
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

        // Check for line comment terminated by end of file
        if (current == '\0' && at_eof(lexer))
        {
            diagnostic_warning_at(lexer->dm, get_curr_location(lexer),
                    "no newline at end of file");
            break;
        }

        consume_char(lexer);
    }
    while (true);
}

static void skip_block_comment(Lexer* lexer, Location start_loc)
{
    do {
        char current = get_curr_char(lexer);
        if (current == '\0' && at_eof(lexer))
        {
            diagnostic_error_at(lexer->dm, start_loc,
                    "unterminated /* comment");
            break;
        }

        // Check for nested block comments
        if (current == '/' && peek_char(lexer) == '*')
        {
            diagnostic_warning_at(lexer->dm, get_curr_location(lexer),
                    "'/*' within block comment");
        }

        // Check for end of comment
        if (current == '*' && peek_char(lexer) == '/')
        {
            consume_char(lexer);
            consume_char(lexer);
            break;
        }

        consume_char(lexer);
    }
    while (true);
}

static TokenData lexer_create_literal_node(Lexer* lexer, Buffer* buffer)
{
    // Mode the buffers data into the memory and delete the buffer
    LiteralNode* node = arena_allocate_size(lexer->literal_arena,
            sizeof(LiteralNode));
    node->value.ptr = arena_allocate_size(lexer->literal_arena,
            buffer_get_len(buffer) + 1);
    memcpy(node->value.ptr, buffer_get_ptr(buffer), buffer_get_len(buffer) + 1);
    node->value.len = buffer_get_len(buffer);
    
    buffer_free(buffer);

    TokenData data = { .literal = node };

    return data;
}

// Try to lex a UCN, we assume the calling functions already saw a '\' so we 
// are then doing the next steps. Then we only advance the lexer some steps
// forward if we determine that we actually have a UCN, AND if we have a buffer
// to add the universal character name into.
static bool try_lex_ucn(Lexer* lexer, Location slash_loc, Buffer* buffer,
        bool identifier, utf32* value)
{
    // Now we attempt to try and read the ucn
    size_t size = 0;
    char current = get_char_and_size(lexer, &size);
    if (current != 'u' && current != 'U')
    {
        return false;
    }

    // Now we should also check if we are in c89 mode and reject the potential
    // ucn. Note that clang doesn't actually check the ammount of digits is
    // what it should be
    if (!lang_opts_c99(lexer->lang))
    {
        // TODO: this diagnostic can trigger twice in C90 mode for some reason
        if (buffer == NULL)
        {
            diagnostic_warning_at(lexer->dm, slash_loc, "universal "
                    "character names are only valid in C99; treating as '\\' "
                    "followed by identifier");
        }
        return false;
    }

    // Save the ucn type
    char ucn_type = current;

    // Now setup our buffers for stuff
    size_t required_digits = (current == 'U') ? 8 : 4;
    char ucn_buffer[MAX_UCN_LENGTH] = {0};

    for (size_t num_digits = 0; num_digits < required_digits; num_digits++)
    {
        current = get_char_and_size(lexer, &size);

        // If we didn't get a hex digit we can just return and leave early
        if (!is_hexadecimal(current))
        {
            // Again only warn when we aren't trying to put it into an 
            // identifier since it will do it twice
            if (buffer == NULL)
            {
                diagnostic_warning_at(lexer->dm, slash_loc, "incomplete "
                        "universal character name; treating as '\\' followed "
                        "by identifier");
            }
            return false;
        }

        ucn_buffer[num_digits] = current;
    }

    // Now we also need to calculate the value of the universal character to
    // ensure that we get a valid ucn within an identifier
    *value = 0;
    for (size_t i = 0; i < required_digits; i++)
    {
        *value *= 16;
        *value += convert_hexadecimal(ucn_buffer[i]);
    }

    // If were here we know we got the required number of digits so let's add
    // them to our buffer along with the leading '\u' or '\U' :)
    // Also, don't forget to eat the UCN if we are here and only if we are 
    // adding to the buffer as well. Since otherwise we will eat it if we are
    // simply trying to test for the present of a UCN
    if (buffer != NULL)
    {
        // Check for invalid UCN's here but allow invalid in numbers since we
        // will not convert then anyways
        if (identifier && !is_valid_ucn(*value))
        {
            diagnostic_error_at(lexer->dm, slash_loc, "character <U+%0*X> "
                    "not allowed in an identifier", required_digits, *value);
        }

        // Non-identifiers should be added this was as well as any invalid 
        // unicode instead of being ignored
        if (!identifier || !is_valid_utf32(*value))
        {
            // For numbers simply add the UCN to the end of the buffer 
            // unconverted since that is what GCC and Clang do and do the same
            // for invalid utf32 since that seems reasonable.
            buffer_add_char(buffer, '\\');
            buffer_add_char(buffer, ucn_type);  
            for (size_t i = 0; i < required_digits; i++)
            {
                buffer_add_char(buffer, ucn_buffer[i]);
            }
        }
        else
        {
            ucn_add_to_buffer(*value, buffer);
        }   

        seek(lexer, size);
    }

    return true;
}

static bool lex_number(Lexer* lexer, Token* token, char* start)
{
    token->type = TOK_NUMBER;

    // Reset the position to the start and create a buffer for us to build
    set_position(lexer, start);

    Buffer number = buffer_new_size(NUMBER_START_SIZE);

    // TODO: note that numbers can contain ucn's so will need to handle that
    // eventually
    while (true)
    {
        char* save_pos = get_position(lexer);
        char current = get_next_char(lexer);

        if (is_identifier(current) || current == '.')
        {
            buffer_add_char(&number, current);

            if (current == 'e' || current == 'E' || current == 'p' 
                    || current == 'P')
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
            Location slash_loc = get_location_from(lexer, save_pos);
            utf32 value;
            if (!try_lex_ucn(lexer, slash_loc, &number, false, &value))
            {
                set_position(lexer, save_pos);
                break;
            }

            continue;
        }

        // Restore the lexers save position if we get to the end of the number
        set_position(lexer, save_pos);
        break;
    }

    // Finish the number construction
    buffer_make_cstr(&number);
    token->data = lexer_create_literal_node(lexer, &number);

    return true;
}

// TODO: I would like to make this more modular to better support older / newer
// language standards eventually...
static void classify_identifier(Token* token)
{
    assert(token_is_identifier(token));

    if (!identifier_is_keyword(token->data.identifier))
    {
        return;
    }

    token->type = identifier_get_keyword(token->data.identifier);
}

static bool lex_identifier(Lexer* lexer, Token* token, char* start)
{
    token->type = TOK_IDENTIFIER;

    // Reset the position to the start and create a buffer for us to build
    set_position(lexer, start);

    Buffer identifier = buffer_new_size(IDENTIFIER_START_SIZE);

    // TODO: handle universal characters
    while (true)
    {
        char* save_pos = get_position(lexer);
        char current = get_next_char(lexer);
        
        // Simple identifier like character
        if (is_identifier(current))
        {
            buffer_add_char(&identifier, current);
            continue;
        }

        // Possible universal character but we need to check for it and if it is
        // then we can continue
        if (current == '\\')
        {
            utf32 value;
            Location slash_loc = get_location_from(lexer, save_pos);
            if (!try_lex_ucn(lexer, slash_loc, &identifier, true, &value))
            {
                set_position(lexer, save_pos);
                break;
            }

            continue;
        }

        // Reset the position of the lexer in order to rewind the fact that we
        // got that character
        set_position(lexer, save_pos);
        break;
    }

    // Finish building the identifier and create the token data.
    // TODO: we wan't to improve this to not allocate / deallocate alot so we
    // may in the future reuse this buffer would be nice
    buffer_make_cstr(&identifier);

    String string = string_from_buffer(&identifier);
    token->data.identifier = identifier_table_lookup(lexer->identifiers,
            &string);
    string_free(&string);

    classify_identifier(token);

    return true;
}

static char get_starting_delimiter(TokenType type)
{
    switch (type)
    {
        case TOK_WIDE_STRING:
        case TOK_STRING:
            return '"';

        case TOK_WIDE_CHARACTER:
        case TOK_CHARACTER:
            return '\'';

        case TOK_PP_HEADER_NAME:
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
        case TOK_WIDE_STRING:
        case TOK_STRING:
            return '"';

        case TOK_WIDE_CHARACTER:
        case TOK_CHARACTER:
            return '\'';

        case TOK_PP_HEADER_NAME:
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
        case TOK_CHARACTER:
        case TOK_WIDE_CHARACTER:
            return true;

        default:
            return false;
    }
}

static bool is_wide(TokenType type)
{
    switch (type)
    {
        case TOK_WIDE_CHARACTER:
        case TOK_WIDE_STRING:
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
        if (current == '\r' || current == '\n'
                || (current == '\0' && at_eof(lexer)))
        {
            token->type = TOK_UNKNOWN;

            diagnostic_warning_at(lexer->dm, token->loc,
                    "missing terminating '%c' character", ending_delim);
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
    if (is_character_like(type) 
            && ((is_wide(type) && buffer_get_len(&string) == 3) 
            || (buffer_get_len(&string) == 2)))
    {
        diagnostic_warning_at(lexer->dm, token->loc,
                "empty character constant");
        token->type = TOK_UNKNOWN;
    }

    token->data = lexer_create_literal_node(lexer, &string);

    return true;
}

static bool lex_string_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOK_STRING);
}

static bool lex_wide_string_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOK_WIDE_STRING);
}

static bool lex_character_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOK_CHARACTER);
}

static bool lex_wide_character_literal(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOK_WIDE_CHARACTER);
}

static bool lex_header_name(Lexer* lexer, Token* token)
{
    return lex_string_like_literal(lexer, token, TOK_PP_HEADER_NAME);
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
        }
        while (is_horizontal_whitespace(get_curr_char_raw(lexer)));
    }

    // Set up the token here...
    char* token_start = get_position(lexer);

    Location token_location = get_curr_location(lexer);

    token->loc = token_location;
    token->end = token_location;

    token->type = TOK_UNKNOWN;

    // Make sure our token flags are correctly set up
    token_set_flag(token, whitespace ? TOKEN_FLAG_WHITESPACE : TOKEN_FLAG_NONE);
    token_set_flag(token,
            lexer->start_of_line ? TOKEN_FLAG_BOL : TOKEN_FLAG_NONE);
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
                // Warn about no newline at eof
                if (!(token->flags & TOKEN_FLAG_BOL))
                {
                    // TODO: fix this, will crash if unterminated token at eof
                    diagnostic_warning_at(lexer->dm, token->loc,
                            "no newline at end of file");
                }

                token->type = TOK_EOF;
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
            // Test to see if we are at the end of the file. If here don't warn
            // about no newlines at eof since we abviously have them.
            if (get_curr_char(lexer) == '\0' && at_eof(lexer))
            {
                token->type = TOK_EOF;

                return false;
            }

            // If we are in a PP directive finish it up
            if (lexer->lexing_directive)
            {
                // Reset our lexing flags and set the token type
                lexer->lexing_directive = false;
                lexer->start_of_line = true;

                token->type = TOK_PP_EOD;

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
                token->type = TOK_ELIPSIS;

                consume_char(lexer);
                consume_char(lexer);
            }
            else
            {
                token->type = TOK_DOT;
            }
            break;

        case '/':
            curr = get_curr_char(lexer);

            if (curr == '/')
            {
                consume_char(lexer);

                // TODO: add mechanism to not report this multiple time, might
                // TODO: have to wait until proper warning classes though
                if (!lang_opts_c99(lexer->lang))
                {
                    diagnostic_warning_at(lexer->dm, token->loc, "// comments "
                            "are not allowed in ISO C90");
                }

                skip_line_comment(lexer);

                // Make sure to unset whitespace
                whitespace = false;

                goto retry_lexing;
            }
            else if (curr == '*')
            {
                consume_char(lexer);

                skip_block_comment(lexer, token->loc);

                whitespace = true;

                goto retry_lexing;
            }
            else if (curr == '=')
            {
                token->type = TOK_SLASH_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_SLASH;
            }
            break;

        case '*':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOK_STAR_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_STAR;
            }
            break;
        
        case '%':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOK_PERCENT_EQUAL;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOK_RCURLY;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                /* Here we either got %: or %:%: */

                consume_char(lexer);

                if (get_curr_char(lexer) == '%' && peek_char(lexer) == ':')
                {
                    token->type = TOK_HASH_HASH;

                    token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                    consume_char(lexer);
                    consume_char(lexer);
                }
                else
                {
                    token->type = TOK_HASH;

                    token_set_flag(token, TOKEN_FLAG_DIGRAPH);
                }
            }
            else
            {
                token->type = TOK_PERCENT;
            }
            break;

        case '+':
            curr = get_curr_char(lexer);
            if (curr == '+')
            {
                token->type = TOK_PLUS_PLUS;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOK_PLUS_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_PLUS;
            }
            break;

        case '-':
            curr = get_curr_char(lexer);
            if (curr == '-')
            {
                token->type = TOK_MINUS_MINUS;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOK_ARROW;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOK_MINUS_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_MINUS;
            }
            break;

        case '|':
            curr = get_curr_char(lexer);
            if (curr == '|')
            {
                token->type = TOK_OR_OR;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOK_OR_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_OR;
            }
            break;

        case '&':
            curr = get_curr_char(lexer);
            if (curr == '&')
            {
                token->type = TOK_AND_AND;

                consume_char(lexer);
            }
            else if (curr == '=')
            {
                token->type = TOK_AND_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_AND;
            }
            break;

        case '^':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOK_XOR_EQUAL;
                
                consume_char(lexer);
            }
            else
            {
                token->type = TOK_XOR;
            }
            break;

        case '=':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOK_EQUAL_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_EQUAL;
            }
            break;

        case '!':
            curr = get_curr_char(lexer);
            if (curr == '=')
            {
                token->type = TOK_NOT_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_NOT;
            }
            break;

        case '#':
            curr = get_curr_char(lexer);
            if (curr == '#')
            {
                token->type = TOK_HASH_HASH;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_HASH;
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
                    token->type = TOK_LT_LT_EQUAL;

                    consume_char(lexer);
                }
                else
                {
                    token->type = TOK_LT_LT;
                }
            }
            else if (curr == '=')
            {
                token->type = TOK_LT_EQUAL;

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                token->type = TOK_LBRACKET;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == '%')
            {
                token->type = TOK_LCURLY;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_LT;
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
                    token->type = TOK_GT_GT_EQUAL;

                    consume_char(lexer);
                }
                else
                {
                    token->type = TOK_GT_GT;
                }
            }
            else if (curr == '=')
            {
                token->type = TOK_GT_EQUAL;

                consume_char(lexer);
            }
            else
            {
                token->type = TOK_GT;
            }
            break;

        case ':': 
            curr = get_curr_char(lexer);
            // Only accept the '::' punctuator in c23 mode
            if (curr == ':' && lang_opts_c23(lexer->lang))
            {
                token->type = TOK_COLON_COLON;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOK_RBRACKET;

                token_set_flag(token, TOKEN_FLAG_DIGRAPH);

                consume_char(lexer);
            } 
            else
            {
                token->type = TOK_COLON;
            }
            break;

        // All single character tokens in c99
        case '[': token->type = TOK_LBRACKET; break;
        case ']': token->type = TOK_RBRACKET; break;
        case '(': token->type = TOK_LPAREN; break;
        case ')': token->type = TOK_RPAREN; break;
        case '{': token->type = TOK_LCURLY; break;
        case '}': token->type = TOK_RCURLY; break;
        case '?': token->type = TOK_QUESTION; break;
        case ';': token->type = TOK_SEMI; break;
        case ',': token->type = TOK_COMMA; break;
        case '~': token->type = TOK_TILDE; break;

        case '\\':
        {
            utf32 value;
            Location slash_loc = get_location_from(lexer, token_start);
            if (try_lex_ucn(lexer, slash_loc, NULL, true, &value))
            {
                lex_identifier(lexer, token, token_start);
                break;
            }

            // RESET TO '\\' FOR NOW ONLY TO CREATE AN UNKNOWN TOKEN TYPE
            curr = '\\';
        }

            /* FALLTHROUGH */

        default: // Create an unknown token
            if (!is_ascii(curr))
            {
                utf32 value;
                bool conversion_success = utf8_to_utf32(
                        (unsigned char**) &token_start, 
                        (unsigned char*) lexer->buffer_end, &value);
                
                // TODO: figure out what to do with the value once we have it
                // probably use the c99 status of if a codepoint is a valid
                // identifier an go from there

                panic("Non ascii character encountered");
            }

            token->type = TOK_UNKNOWN;

            Buffer unknown = buffer_from_format("%c", curr);
            token->data = lexer_create_literal_node(lexer, &unknown);

            // Finally, since we got an unknown token produce a warning about it
            // diagnostic_warning_at(lexer->dm, token->loc, "unknown token '%c' "
            //         "encountered", curr);
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

bool lexer_peek(Lexer* lexer, Token* token)
{
    char* position = get_position(lexer);

    reset_token(token);

    bool status = lex_internal(lexer, token);

    set_position(lexer, position);

    return status;
}

TokenType lexer_get_next_next_type(Lexer* lexer)
{
    // TODO: save flags???
    char* original_position = get_position(lexer);

    // No need to free and data since any literal is stored in the pp's literal
    // arena!
    Token next;
    lexer_get_next(lexer, &next);

    set_position(lexer, original_position);

    return next.type;
}
