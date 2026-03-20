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
#include "driver/warning.h"
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
        IdentifierTable* identifiers, SourceFile* source)
{
    const FileBuffer* fb = source_file_get_buffer(source);
    *lexer = (Lexer)
    {
        .dm = dm,
        .lang = opts,
        .identifiers = identifiers,
        .buffer_start = file_buffer_get_start(fb),
        .buffer_end = file_buffer_get_end(fb),
        .current_ptr = (char*) file_buffer_get_start(fb),
        .start_loc = source_file_get_start_location(source),
        .err_line_comment = false,
        .start_of_line = true,
        .lexing_directive = false,
        .can_lex_header = false
    };
}

static bool diagnose(const Lexer* lexer)
{
    return lexer->dm != NULL;
}

void lexer_set_directive(Lexer* lexer)
{
    lexer->lexing_directive = true;
}

void lexer_set_header(Lexer* lexer)
{
    lexer->can_lex_header = true;
}

void lexer_diable_diagnostics(Lexer* lexer)
{
    lexer->dm = NULL;
}

void lexer_enable_diagnostics(Lexer* lexer, DiagnosticManager* dm)
{
    assert(!diagnose(lexer) && "should only be reenabled when diabled");
    lexer->dm = dm;
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
    assert(pos >= lexer->buffer_start);
    assert(pos <= lexer->buffer_end);

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
    assert(!is_simple_char(curr) && "simple char?");

    char next = curr_ptr[*peek + 1];
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
        char trigraph = get_trigraph(curr_ptr[*peek + 2]);

        if (!trigraph)
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

static bool would_be_eof(Lexer* lexer, size_t peek)
{
    return lexer->current_ptr + peek >= lexer->buffer_end;
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

static void seek_to_location(Lexer* lexer, Location location)
{
    assert(location >= lexer->start_loc && "not in lexer?");

    Location diff = location - lexer->start_loc;
    lexer->current_ptr += diff;
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
        if (current == '\0' && at_eof(lexer) && diagnose(lexer))
        {
            diagnostic_warning_at(lexer->dm, get_curr_location(lexer),
                    Wnewline_eof, "no newline at end of file");
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
        if (current == '\0' && at_eof(lexer) && diagnose(lexer))
        {
            diagnostic_error_at(lexer->dm, start_loc,
                    "unterminated /* comment");
            break;
        }

        // Check for nested block comments
        if (current == '/' && peek_char(lexer) == '*' && diagnose(lexer))
        {
            diagnostic_warning_at(lexer->dm, get_curr_location(lexer),
                    Wcomment, "'/*' within block comment");
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
        if (buffer == NULL && diagnose(lexer))
        {
            // TODO: right warning opt?
            diagnostic_warning_at(lexer->dm, slash_loc, Wunicode, "universal "
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
            if (buffer == NULL && diagnose(lexer))
            {
                diagnostic_warning_at(lexer->dm, slash_loc, Wunicode,
                        "incomplete universal character name; treating as '\\' "
                        "followed by identifier");
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
        if (identifier && !is_valid_ucn(*value) && diagnose(lexer))
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

bool lexer_try_get_ucn(Lexer* lexer, utf32* value, bool commit, bool no_warn)
{
    assert(get_curr_char(lexer) == '\\' && "not a slash?");

    size_t peek = 0;
    get_char_and_size(lexer, &peek); // Skip the slash.

    char current = get_char_and_size(lexer, &peek);

    if (current != 'u' && current != 'U')
    {
        return false;
    }

     // Now we should also check if we are in c89 mode and reject the potential
    // ucn. Note that clang doesn't actually check the ammount of digits is
    // what it should be
    if (!lang_opts_c99(lexer->lang))
    {
        if (!no_warn && diagnose(lexer))
        {
            diagnostic_warning_at(lexer->dm, get_curr_location(lexer), Wunicode,
                    "universal character names are only valid in C99; treating "
                    "as '\\' followed by identifier");
        }
        return false;
    }

    // Now get the required digits and try to see if we have that many also keep
    // track of the current value of the UCN
    utf32 tmp_value = 0;
    size_t required = current == 'U' ? 8 : 4;
    for (size_t i = 0; i < required; i++)
    {
        current = get_char_and_size(lexer, &peek);
        if (!is_hexadecimal(current))
        {
            if (!no_warn && diagnose(lexer))
            {
                diagnostic_warning_at(lexer->dm, get_curr_location(lexer),
                        Wunicode, "incomplete universal character name; "
                        "treating as '\\' followed by identifier");
            }
            return false;
        }

        tmp_value *= 16;
        tmp_value += convert_hexadecimal(current);
    }

    // Okay we have got to this point and know that we have a valid ucn. If we
    // should commit do so now and if we should put the value in the buffer also
    // do so now
    if (commit)
    {
        seek(lexer, peek);
    }

    if (value)
    {
        *value = tmp_value;
    }

    return true;
}

bool lexer_number(Lexer* lexer, Token* token)
{
    while (true)
    {
        size_t peek = 0;
        char current = get_char_and_size(lexer, &peek);

        // Handle any identifier chars or dots here whilst also handling 
        // exponentiation stuff.
        if (is_identifier(current) || current == '.')
        {
            // We we get an 'E' or 'P' then check if the next character is a 
            // plus or minus. If it is, then commit to the plus or minus.
            if (current == 'e' || current == 'E' || current == 'p'
                    || current == 'P')
            {
                size_t new_peek = peek;
                current = get_char_and_size(lexer, &new_peek);
                if (current == '-' || current == '+')
                {
                    peek = new_peek;
                }
            }

            seek(lexer, peek);
            continue;            
        }

        // Handle a possible ucn in a number. We should definitely commit to it
        // here if we get it too. Note that we don't really care about the
        // value of the UCN anyways.
        if (current == '\\')
        {
            if (lexer_try_get_ucn(lexer, /*value=*/NULL, /*commit=*/true,
                    /*no_warn=*/true))
            {
                continue;
            }
        }

        // Wasn't an identifier char or a UCN
        break;
    }

    // Finally, after we are done setup some important information in the token
    token_set_type(token, TOK_NUMBER);
    token_set_end(token, get_ending_location(lexer));

    // Then we will want to get the Identifier* for this token. Then don't 
    // forget to classify the token.
    return true;
}

void lexer_get_identifier_spelling(Lexer* lexer, Token* token, char* buffer)
{
    // Save the current position of the lexer.
    char* save_pos = get_position(lexer);

    // Since the token is currently storing the raw pointer to it's start
    set_position(lexer, token->data.raw);

    // Then keep getting the characters until we get to the end.
    size_t pos = 0;
    while (get_curr_location(lexer) <= token_get_end(token))
    {
        buffer[pos++] = get_next_char(lexer);
    }
    assert(get_position(lexer) == save_pos && "didn't restore position?");

    // Then finally terminate the buffer.
    buffer[pos] = '\0';
}

Identifier* lexer_lookup_identifier(Lexer* lexer, Token* token)
{
    // Start by getting the length of the token
    size_t length = token_get_length(token);

    // FIXME: should we create some kind of small buffer type for this. I think
    // FIXME: it would be good since we could easily reuse in on our numbers
    // FIXME: and other stuff to reduce memory allocations there too!
    // Create a union which will hold our data based on the length we're given
    union {
        char stack[64];
        Buffer heap;
    } buffer;

    // >= to since we need to store the null terminator.
    if (length >= 64)
    {
        buffer.heap = buffer_new_size(length + 1); // + 1 for null terminator
    }

    // Then get the pointer we are going to get the spelling of the identifier
    // through.
    char* ptr = length < 64 ? buffer.stack : buffer_get_ptr(&buffer.heap);
    lexer_get_identifier_spelling(lexer, token, ptr);

    // Then all we need to do is a simple lookup using our identifier table.
    Identifier* id = identifier_table_get(lexer->identifiers, ptr);

    // Then don't forget to free the heap buffer if we ended up using that to
    // get our identifier.
    if (length >= 64)
    {
        buffer_free(&buffer.heap);
    }

    // Finally, return the identifier that we found.
    return id;    
}

bool lexer_identifier(Lexer* lexer, Token* token)
{
    // Keep going until we reach the end of the identifier
    while (true)
    {
        size_t size = 0;
        char current = get_char_and_size(lexer, &size);

        // ASCII identifiers
        if (is_identifier(current))
        {
            seek(lexer, size);
            continue;
        }

        // If it's not an ASCII identifier and we have a backslash try to get a
        // UCN in that case.
        if (current == '\\')
        {
            utf32 value;
            if (lexer_try_get_ucn(lexer, &value, /*commit*/true,
                    /*no_warn*/true))
            {
                continue;
            }
        }

        if (!is_ascii(current))
        {
            // TODO: unicode identifiers?
        }
        
        // Did not get an idnetifier char or a UCN so we are definitely done.
        break;
    }

    // Finally, after we are done setup some important information in the token
    token_set_type(token, TOK_IDENTIFIER);
    token_set_end(token, get_ending_location(lexer));

    // Then we will want to get the Identifier* for this token. Then don't 
    // forget to classify the token.
    token->data.identifier = lexer_lookup_identifier(lexer, token);
    token_classify_identifier(token);
    return true;
}

bool lexer_header_name(Lexer* lexer, Token* token)
{
    assert(lexer->can_lex_header && "can't lex a header right now?");

    // Here it is a bit simple since we don't need to escape characters here.
    while (true)
    {
        size_t peek = 0;
        char current = get_char_and_size(lexer, &peek);

        if ((current == '\r' || current == '\n')
                || (current == '\0' && at_eof(lexer)))
        {
            if (diagnose(lexer))
            {
                diagnostic_error_at(lexer->dm, get_curr_location(lexer),
                        "expected '>'");
            }
            break;
        }

        seek(lexer, peek);

        // If we had the closing delimited we are done.
        if (current == '>')
        {
            break;
        }
    }

    // Set the token type and get the ending location for the token
    token_set_type(token, TOK_PP_HEADER_NAME);
    token_set_end(token, get_ending_location(lexer));

    // Also make sure that we can't lex any more header
    lexer->can_lex_header = false;
    return true;
}

bool lexer_string_literal(Lexer* lexer, Token* token, TokenType type)
{
    bool had_char = false;
    bool error = false;

    // Keep going until we reach the end of the character literal
    while (true)
    {
        char current = get_next_char(lexer);

        // We have reached the end of the character literal so end.
        if (current == '"')
        {
            break;
        }

        // If we get an escape character take the opportunity to escape it.
        if (current == '\\')
        {
            current = get_next_char(lexer);
        }

        // Check if we are at the end of the line or at the end of the file then
        // we are in an error state. This is not good.
        if ((current == '\r' || current == '\n')
                || (current == '\0' && at_eof(lexer)))
        {
            if (diagnose(lexer))
            {
                diagnostic_warning_at(lexer->dm, token->loc, Winvalid_pp_token,
                        "missing terminating '\"' character");
            }
            error = true;
            break;
        }

        // Make sure to set if we've had a character or not.
        had_char = true;
    }

    // Finally, after we are done setup some important information in the token
    token_set_type(token, !error ? type : TOK_UNKNOWN);
    token_set_end(token, get_ending_location(lexer));

    return true;
}

bool lexer_char_literal(Lexer* lexer, Token* token, TokenType type)
{
    bool had_char = false;
    bool error = false;

    // Keep going until we reach the end of the character literal
    while (true)
    {
        char current = get_next_char(lexer);

        // We have reached the end of the character literal so end.
        if (current == '\'')
        {
            break;
        }

        // If we get an escape character take the opportunity to escape it.
        if (current == '\\')
        {
            current = get_next_char(lexer);
        }

        // Check if we are at the end of the line or at the end of the file then
        // we are in an error state. This is not good.
        if ((current == '\r' || current == '\n')
                || (current == '\0' && at_eof(lexer)))
        {
            if (diagnose(lexer))
            {
                diagnostic_warning_at(lexer->dm, token->loc, Winvalid_pp_token,
                        "missing terminating ' character");
            }
            error = true;
            break;
        }

        // Make sure to set if we've had a character or not.
        had_char = true;
    }

    // Before finishing off our token check for any final errors that we may
    // have gotten which for here is just checking that we don't have an empty
    // character constant.
    if (!error && !had_char && diagnose(lexer))
    {
        diagnostic_warning_at(lexer->dm, token_get_location(token),
                Winvalid_pp_token, "empty character constant");
        error = true;
    }

    // Finally, after we are done setup some important information in the token
    token_set_type(token, !error ? type : TOK_UNKNOWN);
    token_set_end(token, get_ending_location(lexer));

    return true;
}

static bool lex_end_of_file(Lexer* lexer, Token* token, char* tok_start)
{
    // Check if we are in directive lexing mode and adjust accordingly. If we
    // are then we should return an EOD token and then revert the lexers 
    // position so that we can relex the end of file
    if (lexer->lexing_directive)    
    {
        // Reset our lexing flags and set the token type
        lexer->lexing_directive = false;
        lexer->start_of_line = false;

        token_set_type(token, TOK_PP_EOD);

        // Reset the lexer position so we can go again at the same spot and get 
        // an EOF token.
        set_position(lexer, tok_start);
        return true;
    }

    // Warn about no newline at eof since we know we have definitely hit the
    // end of file. Also issue a warning on a completely empty translation unit
    if ((lexer->buffer_start == lexer->buffer_end 
            || (*(lexer->buffer_end - 1) != '\n' 
            && *(lexer->buffer_end - 1) != '\r')) && diagnose(lexer))
    {
        diagnostic_warning_at(lexer->dm, token->loc, Wnewline_eof,
                "no newline at end of file");
    }

    // Set the token type to be the EOF token.
    token_set_type(token, TOK_EOF);
    
    // Reset the lexer position back to the end of the bufer to allow for 
    // multiple eof tokens to be sent. This helps our skipping lexing be faster.
    assert(lexer->current_ptr >= lexer->buffer_end
            && *lexer->buffer_end == '\0');
    lexer->current_ptr = (char*) lexer->buffer_end;

    return false;
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
    assert(token_start <= lexer->buffer_end && "currently off buffer end!");

    Location token_location = get_curr_location(lexer);

    token->loc = token_location;
    token->end = token_location;

    token->type = TOK_UNKNOWN;

    // Make sure our token flags are correctly set up
    token_set_flag(token, whitespace ? TOK_FLAG_WHITESPACE : TOK_FLAG_NONE);
    token_set_flag(token,
            lexer->start_of_line ? TOK_FLAG_BOL : TOK_FLAG_NONE);

    token->data.raw = token_start;

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
                return lex_end_of_file(lexer, token, token_start);
            }

            // TODO: warn on null characters?
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
            // If we are in a PP directive finish it up. This should take 
            // priority over being in the end of the file. Otherwise it is 
            // possible for an EOF token to be sent before and EOD token.
            if (lexer->lexing_directive)
            {
                // Reset our lexing flags and set the token type
                lexer->lexing_directive = false;
                lexer->can_lex_header = false; // In the event of not getting it
                lexer->start_of_line = true;
                token->type = TOK_PP_EOD;
                break;
            }

            // Test to see if we are at the end of the file. If here don't warn
            // about no newlines at eof since we abviously have them.
            if (get_curr_char(lexer) == '\0' && at_eof(lexer))
            {
                token->type = TOK_EOF;
                return false;
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
            return lexer_number(lexer, token);
        
        // Look for the 'L' prefix for string and character literals
        case 'L':
            curr = get_curr_char(lexer);
            if (curr == '"')
            {
                consume_char(lexer);
                return lexer_string_literal(lexer, token, TOK_WIDE_STRING);
            }
            else if (curr == '\'')
            {
                consume_char(lexer);
                return lexer_char_literal(lexer, token, TOK_WIDE_CHARACTER);
            }

            return lexer_identifier(lexer, token);

        // Look for the 'U' prefix but only if we are in the correct language
        // modes.
        case 'U':
            if (!lang_opts_c11(lexer->lang))
            {
                return lexer_identifier(lexer, token);
            }

            curr = get_curr_char(lexer);
            if (curr == '"')
            {
                consume_char(lexer);
                return lexer_string_literal(lexer, token, TOK_UTF32_STRING);
            }
            else if (curr == '\'')
            {
                consume_char(lexer);
                return lexer_char_literal(lexer, token, TOK_UTF32_CHARACTER);
            }

            return lexer_identifier(lexer, token);
            
        // Look for the 'u' or 'u8' encoding prefix but only if we are in the
        // correct language modes that allow for this. Note that u8 characters
        // were only introduced in C23 so we will need to check for this.
        case 'u':
        {
            if (!lang_opts_c11(lexer->lang))
            {
                return lexer_identifier(lexer, token);
            }

            bool u8 = false;
            curr = get_curr_char(lexer);

            // First check for the possibility of having a u8
            if (curr == '8')
            {
                u8 = true;

                consume_char(lexer);
                curr = get_curr_char(lexer);
            }

            if (curr == '"')
            {
                consume_char(lexer);
                return lexer_string_literal(lexer, token,
                        u8 ? TOK_UTF8_STRING : TOK_UTF16_STRING);
            }
            else if (curr == '\''
                    && (!u8 || (u8 && lang_opts_c23(lexer->lang))))
            {
                consume_char(lexer);
                return lexer_char_literal(lexer, token,
                        u8 ? TOK_UTF8_CHARACTER : TOK_UTF16_CHARACTER);
            }
        }

        /* FALLTHROUGH */
        
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
        case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':    /*'U'*/
        case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
        case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
        case 'o': case 'p': case 'q': case 'r': case 's': case 't':    /*'u'*/
        case 'v': case 'w': case 'x': case 'y': case 'z':
        case '_':
            return lexer_identifier(lexer, token);

        // String and character literals
        case '"':
            return lexer_string_literal(lexer, token, TOK_STRING);

        case '\'':
            return lexer_char_literal(lexer, token, TOK_CHARACTER);
        
        case '.':
            curr = get_curr_char(lexer);

            if (is_numeric(curr))
            {
                return lexer_number(lexer, token);
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

                if (!lang_opts_c99(lexer->lang) && !lexer->err_line_comment
                        && diagnose(lexer))
                {
                    lexer->err_line_comment = true;
                    diagnostic_warning_at(lexer->dm, token->loc, Wcomment,
                            "// comments are not allowed in ISO C90");
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

                token_set_flag(token, TOK_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == ':')
            {
                /* Here we either got %: or %:%: */

                consume_char(lexer);

                if (get_curr_char(lexer) == '%' && peek_char(lexer) == ':')
                {
                    token->type = TOK_HASH_HASH;

                    token_set_flag(token, TOK_FLAG_DIGRAPH);

                    consume_char(lexer);
                    consume_char(lexer);
                }
                else
                {
                    token->type = TOK_HASH;

                    token_set_flag(token, TOK_FLAG_DIGRAPH);
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
                return lexer_header_name(lexer, token);
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

                token_set_flag(token, TOK_FLAG_DIGRAPH);

                consume_char(lexer);
            }
            else if (curr == '%')
            {
                token->type = TOK_LCURLY;

                token_set_flag(token, TOK_FLAG_DIGRAPH);

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

            if (curr == ':')
            {
                token->type = TOK_COLON_COLON;

                consume_char(lexer);
            }
            else if (curr == '>')
            {
                token->type = TOK_RBRACKET;

                token_set_flag(token, TOK_FLAG_DIGRAPH);

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
            // TODO: enable us to start an identifier with a UCN
            // utf32 value;
            // Location slash_loc = get_location_from(lexer, token_start);
            // if (try_lex_ucn(lexer, slash_loc, NULL, true, &value))
            // {
            //     lex_identifier(lexer, token);
            //     break;
            // }

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

                panic("non ascii characters are currenlty not implemented");
            }

            // Otherwise we just have a purely unknown token.
            token->type = TOK_UNKNOWN;
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

bool lexer_peek_next(Lexer* lexer, Token* token)
{
    // Save our lexer's state and disable diagnostics
    DiagnosticManager* dm = lexer->dm; // Save the diagnostic manager
    lexer->dm = NULL; // Make sure to turn the diagnostics off completely by
                      // removing the dm from the lexer.
                      // TODO: is this an appropriate thing to do?
    
    char* current_ptr = lexer->current_ptr;
    /* no need for err_line_comment as diagnostics are diable at the moment */
    bool start_of_line =  lexer->start_of_line;
    bool lexing_directive = lexer->lexing_directive;
    bool can_lex_header = lexer->can_lex_header;

    reset_token(token);
    bool ret = lex_internal(lexer, token);

    // Restore the lexer's state to before the token was lexed.
    lexer->current_ptr = current_ptr;
    lexer->start_of_line = start_of_line;
    lexer->lexing_directive = lexing_directive;
    lexer->can_lex_header = can_lex_header;
    lexer->dm = dm; // Restore the diagnotic manager
    
    // Finally, properly return the token to the user.
    return ret;
}

void lexer_skip_to_end_of_line(Lexer* lexer)
{
    // FIXME: for now we won't seek past newlines.
    while (true)
    {
        size_t peek = 0;
        char curr = get_char_and_size(lexer, &peek);

        // If we are at the end of the line then break.
        if (curr == '\r' || curr == '\n')
        {
            return;
        }
        
        // Also if we are at the end of the file then break at well.
        if (curr == '\0' && would_be_eof(lexer, peek))
        {
            return;
        }

        // Otherwise 'eat' the char that we would have gotten.
        seek(lexer, peek);
    }
}

void lexer_read_diagnostic_string(Lexer* lexer, Buffer* buffer)
{
    // consume leading whitespace 
    while (is_horizontal_whitespace(get_curr_char(lexer)))
    {
        consume_char(lexer);
    }

    while (true)
    {
        char curr;
        switch ((curr = get_curr_char(lexer)))
        {
            // We got the end of the line
            case '\r':
            case '\n':
                buffer_make_cstr(buffer);
                return;

            // We either got EOF or just some random null char
            case '\0':
                if (at_eof(lexer))
                {
                    buffer_make_cstr(buffer);
                    return;
                }

                /* FALLTHROUGH */

            default:
                buffer_add_char(buffer, curr);
                break;
        }
        consume_char(lexer);
    }
}

// FIXME: this function and the one below will eventually fail for tokens which
// FIXME: are macros. This is not good.
void lexer_token_spelling(SourceManager* sm, LangOptions* opts, Token token,
        Buffer* buffer)
{
    Location location = token_get_location(&token);
    SourceFile* source = source_manager_from_location(sm, location);

    Lexer lexer;
    lexer_create(&lexer, NULL, opts, NULL, source);

    seek_to_location(&lexer, location);
    assert(get_curr_location(&lexer) == location && "not at location?");

    Location end_location = token_get_end(&token);
    while (get_curr_location(&lexer) <= end_location)
    {
        // Get the next char from the lexer.Then we just add to the buffer.
        char c = get_next_char(&lexer);
        buffer_add_char(buffer, c);   
    }
}

// FIXME: This should not escape a '\' if it is not coming from a string?
void lexer_token_stringify(SourceManager* sm, LangOptions* opts, Token token,
        Buffer* buffer)
{   
    Location location = token_get_location(&token);
    SourceFile* source = source_manager_from_location(sm, location);

    Lexer lexer;
    lexer_create(&lexer, /*dm*/NULL, opts, /*idents*/NULL, source);

    seek_to_location(&lexer, location);
    assert(get_curr_location(&lexer) == location && "not at location?");

    Location end_location = token_get_end(&token);
    while (get_curr_location(&lexer) <= end_location)
    {
        // Get the next char from the lexer. If it as '\\' or a '"' then we 
        // should escape it. Then we just add the char to the buffer.
        char c = get_next_char(&lexer);

        if (c == '\\' || c == '"')
        {
            buffer_add_char(buffer, '\\');
        }
        buffer_add_char(buffer, c);   
    }
}
