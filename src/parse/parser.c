#include "parser.h"

#include <stdbool.h>
#include <stddef.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "util/panic.h"
#include "util/xmalloc.h"

#include "driver/diagnostic.h"

#include "lex/token.h"
#include "lex/location_map.h"

#include "parse/expression.h"
#include "parse/declaration.h"
#include "parse/statement.h"

static const TokenType storage_classes[] = 
{
    TOKEN_EXTERN,
    TOKEN_TYPEDEF,
    TOKEN_STATIC,
    TOKEN_AUTO,
    TOKEN_REGISTER
};

static const TokenType type_qualifiers[] =
{
    TOKEN_CONST,
    TOKEN_RESTRICT,
    TOKEN_VOLATILE,
    TOKEN_INLINE
};

static const TokenType statement_start_set[] = 
{
    TOKEN_FOR
};

static const size_t statement_start_set_size = 
        sizeof(statement_start_set) / sizeof(statement_start_set[0]);

static bool is_valid_stream_position(TokenStream* stream)
{
    return (stream->current_token < stream->count);
}

// Below are basic token matching functions for us to use while parsing

static TokenType curr_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token].type;
}

static TokenType next_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token + 1].type;
}

static TokenType next_next_type(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return stream->tokens[stream->current_token + 2].type;
}

static Token* curr(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return &stream->tokens[stream->current_token];
}

static Token* next(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    return &stream->tokens[stream->current_token + 1];
}

static void consume(TokenStream* stream)
{
    assert(is_valid_stream_position(stream));

    // Make sure we only eat tokens when it is okay to do so, otherwise we might
    // go over how many we can actually eat
    if (stream->current_token < stream->count)
    {
        stream->current_token++;
    }
}

static void parse_error(Parser* parser, const char* fmt, ...)
{
    Token* tok = curr(parser->stream);   

    ResolvedLocation loc = line_map_resolve_location(parser->map, tok->loc);
    fprintf(stderr, "%s:%u:%u\n", loc.name->path, loc.line, loc.col);
    va_list args;
    va_start(args, fmt);
    diag_verror(fmt, args);
    va_end(args);
}

static bool match(Parser* parser, TokenType type)
{
    TokenStream* stream = parser->stream;

    if (curr_type(stream) == type)
    {
        consume(stream);

        return true;
    }

    parse_error(parser, "expected '%s' but got '%s'", 
            token_type_get_name(type), 
            token_type_get_name(curr_type(stream))
        );

    panic("error");

    return false;
}

static bool is_match(Parser* parser, TokenType type)
{
    return (curr_type(parser->stream) == type);
}

static bool has_match(Parser* parser, const TokenType* types, size_t count)
{
    const TokenType current = curr_type(parser->stream);

    for (size_t i = 0; i < count; i++)
    {
        if (current == types[i])
        {
            return true;
        }
    }

    return false;
}

// Functions to help with error recovery eventually we will have more 
// intelligent error recovery functions but this will do for now...
static void consume_until_and(Parser* parser, TokenType type)
{   
    TokenStream* stream = parser->stream;

    while (curr_type(stream) != type)
    {
        if (curr_type(stream) == TOKEN_EOF)
        {
            return;
        }

        consume(stream);
    }

    consume(stream);
}

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_integer_constant(Parser* parser);
static Expression* parse_floating_constant(Parser* parser);
static Expression* parse_enumeration_constant(Parser* parser);
static Expression* parse_character_constant(Parser* parser);
static Expression* parse_constant(Parser* parser);

// All of our functions for parsing expressions
static Expression* parse_primary_expression(Parser* parser);
static Expression* parse_postfix_expression(Parser* parser);
static Expression* parse_argument_expression_list(Parser* parser);
static Expression* parse_unary_expression(Parser* parser);
static Expression* parse_cast_expression(Parser* parser);
static Expression* parse_multiplicative_expression(Parser* parser);
static Expression* parse_additive_expression(Parser* parser);
static Expression* parse_shift_expression(Parser* parser);
static Expression* parse_relational_expression(Parser* parser);
static Expression* parse_equality_expression(Parser* parser);
static Expression* parse_and_expression(Parser* parser);
static Expression* parse_exclusive_or_expression(Parser* parser);
static Expression* parse_inclusive_or_expression(Parser* parser);
static Expression* parse_logical_and_expression(Parser* parser);
static Expression* parse_logical_or_expression(Parser* parser);
static Expression* parse_conditional_expression(Parser* parser);
static Expression* parse_assignment_expression(Parser* parser);
static Expression* parse_constant_expression(Parser* parser);
static Expression* parse_expression(Parser* parser);

// All of our functions for parsing statements
static Statement* parse_labeled_statement(Parser* parser);
static Statement* parse_compound_statement(Parser* parser);
static Statement* parse_expression_statement(Parser* parser);
static Statement* parse_selection_statement(Parser* parser);
static Statement* parse_iteration_statement(Parser* parser);
static Statement* parse_jump_statement(Parser* parser);
static Statement* parse_statement(Parser* parser);

// All of our functions for parsing declarations / definitions
static Declaration* parse_designation(Parser* parser);
static Declaration* parse_designator_list(Parser* parser);
static Declaration* parse_designator(Parser* parser);
static Declaration* parse_initializer(Parser* parser);
static Declaration* parse_initializer_list(Parser* parser);
static Declaration* parse_declarator(Parser* parser);
static Declaration* parse_init_declarator(Parser* parser);
static Declaration* parse_init_declarator_list(Parser* parser);
static Declaration* parse_function_specificer(Parser* parser);
static Declaration* parse_typedef_name(Parser* parser);
static Declaration* parse_enumerator_list(Parser* parser);
static Declaration* parse_enum_specificer(Parser* parser);
static Declaration* parse_declarator(Parser* parser);
static Declaration* parse_direct_declarator(Parser* parser);
static Declaration* parse_direct_abstract_declarator(Parser* parser);
static Declaration* parse_abstract_declarator(Parser* parser);
static Declaration* parse_pointer(Parser* parser);
static Declaration* parse_identifier_list(Parser* parser);
static Declaration* parse_paramater_declaration(Parser* parser);
static Declaration* parse_paramater_list(Parser* parser);
static Declaration* parse_paramater_type_list(Parser* parser);
static Declaration* parse_type_specificer(Parser* parser);
static Declaration* parse_type_qualifier(Parser* parser);
static Declaration* parse_type_qualifier_list(Parser* parser);
static Declaration* parse_specifier_qualifier_list(Parser* parser);
static Declaration* parse_struct_declarator(Parser* parser);
static Declaration* parse_struct_declarator_list(Parser* parser);
static Declaration* parse_struct_declaration(Parser* parser);
static Declaration* parse_struct_declaration_list(Parser* parser);
static Declaration* parse_struct_or_union(Parser* parser);
static Declaration* parse_struct_or_union_specifier(Parser* parser);
static Declaration* parse_type_specifier(Parser* parser);
static Declaration* parse_storage_class_specifier(Parser* parser);
static Declaration* parse_declaration_specifiers(Parser* parser);
static Declaration* parse_declaration(Parser* parser);

// The definitions of the functions we will use for pasing

// Some functions for creating errors


















void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};

    while (curr_type(stream) != TOKEN_EOF)
    {
        parse_statement(&parser);
    }

    return;
}

static Expression* parse_integer_constant(Parser *parser);
static Expression* parse_floating_constant(Parser *parser);
static Expression* parse_enumeration_constant(Parser *parser);
static Expression* parse_character_constant(Parser *parser);

static Expression* parse_constant(Parser *parser)
{
    switch (curr_type(parser->stream))
    {


        default:
            panic("not a valid constant type");

    }

    return NULL;
}

static Expression* parse_primary_expression(Parser* parser)
{
    // TODO: make sure we handle wide strings, stirngs, wide chars, and chars
    // TODO: propeperly

    /* for now just match numbers */
    if (is_match(parser, TOKEN_LPAREN))
    {
        match(parser, TOKEN_LPAREN);

        parse_expression(parser);

        match(parser, TOKEN_RPAREN);
    }
    else if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);
    }
    else if (is_match(parser, TOKEN_NUMBER))
    {
        match(parser, TOKEN_NUMBER);
    }
    else if (is_match(parser, TOKEN_STRING))
    {
        match(parser, TOKEN_STRING);
    }
    else if (is_match(parser, TOKEN_CHARACTER))
    {
        match(parser, TOKEN_CHARACTER);
    }
    else
    {
        panic("parse_primary_expression");
    }

    return NULL;
}

static Expression* parse_postfix_expression(Parser* parser)
{
    /* for now we will ignore the typename followed by initialiser list */
    /* ( type-name ) { initializer-list } */
    /* in fact for now we will just parse a primary expression as this would 
     * be quite a bit of effort to implement at the moment
     */

    parse_primary_expression(parser);
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_LPAREN,
            TOKEN_DOT, TOKEN_ARROW, TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS}, 6))
    {
        switch (curr_type(parser->stream))
        {
            case TOKEN_LBRACKET:
                match(parser, TOKEN_LBRACKET);
                parse_expression(parser);
                match(parser, TOKEN_RBRACKET);
                break;

            case TOKEN_LPAREN:
                match(parser, TOKEN_LPAREN);
                if (curr_type(parser->stream) != TOKEN_RPAREN)
                {
                    parse_argument_expression_list(parser);
                }
                match(parser, TOKEN_RPAREN);
                break;

            case TOKEN_DOT:
                match(parser, TOKEN_DOT);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_ARROW:
                match(parser, TOKEN_ARROW);
                match(parser, TOKEN_IDENTIFIER);
                break;

            case TOKEN_PLUS_PLUS:
                match(parser, TOKEN_PLUS_PLUS);
                break;

            case TOKEN_MINUS_MINUS:
                match(parser, TOKEN_MINUS_MINUS);
                break;

            default:
                panic("unreachable");
                break;
        }
    }

    return NULL;
}

static Expression* parse_argument_expression_list(Parser* parser)
{
    parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        parse_assignment_expression(parser);
    }

    return NULL;
}

static Expression* parse_unary_expression(Parser* parser)
{
    switch (curr_type(parser->stream))
    {
        case TOKEN_PLUS_PLUS:
            match(parser, TOKEN_PLUS_PLUS);
            parse_unary_expression(parser);
            break;

        case TOKEN_MINUS_MINUS:
            match(parser, TOKEN_MINUS_MINUS);
            parse_unary_expression(parser);
            break;

        case TOKEN_AND:
            match(parser, TOKEN_AND);
            parse_cast_expression(parser);
            break;

        case TOKEN_STAR:
            match(parser, TOKEN_STAR);
            parse_cast_expression(parser);
            break;

        case TOKEN_PLUS:
            match(parser, TOKEN_PLUS);
            parse_cast_expression(parser);
            break;

        case TOKEN_MINUS:
            match(parser, TOKEN_MINUS);
            parse_cast_expression(parser);
            break;

        case TOKEN_TILDE:
            match(parser, TOKEN_TILDE);
            parse_cast_expression(parser);
            break;

        case TOKEN_NOT:
            match(parser, TOKEN_NOT);
            parse_cast_expression(parser);
            break;

        case TOKEN_SIZEOF:
            match(parser, TOKEN_SIZEOF);
            /* for now we will ignore the case where we have a type name */
            parse_unary_expression(parser);
            break;

        default:
            parse_postfix_expression(parser);
            break;
    }

    return NULL;
}

static Expression* parse_cast_expression(Parser* parser)
{
    /* for now we will just do a unary expression and ignore the part below */

    /* this is because they share some of the same start set and we don't want
     * anyhing to do with that just yet 
     */

    /* ( type-name ) cast-expression*/

    parse_unary_expression(parser);

    return NULL;
}

static Expression* parse_multiplicative_expression(Parser* parser)
{
    parse_cast_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT}, 3))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_STAR: match(parser, TOKEN_STAR); break;
            case TOKEN_SLASH: match(parser, TOKEN_SLASH); break;
            case TOKEN_PERCENT: match(parser, TOKEN_PERCENT); break;

            default:
                panic("unreachable");
                break;
        }

        parse_cast_expression(parser);
    }

    return NULL;
}

static Expression* parse_additive_expression(Parser* parser)
{
    parse_multiplicative_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_PLUS, TOKEN_MINUS}, 2))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_PLUS: match(parser, TOKEN_PLUS); break;
            case TOKEN_MINUS: match(parser, TOKEN_MINUS); break;

            default:
                panic("unreachable");
                break;
        }

        parse_multiplicative_expression(parser);
    }

    return NULL;
}

static Expression* parse_shift_expression(Parser* parser)
{
    parse_additive_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LT_LT, TOKEN_GT_GT}, 2))
    {
        switch (curr_type(parser->stream)) 
        {
            case TOKEN_LT_LT: match(parser, TOKEN_LT_LT); break;
            case TOKEN_GT_GT: match(parser, TOKEN_GT_GT); break;

            default:
                panic("unreachable");
                break;
        }
        
        parse_additive_expression(parser);
    }

    return NULL;
}

static Expression* parse_relational_expression(Parser* parser)
{
    parse_shift_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_LT, TOKEN_GT, TOKEN_LT_EQUAL, TOKEN_GT_EQUAL}, 4))
    {
        switch (curr_type(parser->stream))
        {
            case TOKEN_LT: match(parser, TOKEN_LT); break;
            case TOKEN_GT: match(parser, TOKEN_GT); break;
            case TOKEN_LT_EQUAL: match(parser, TOKEN_LT_EQUAL); break;
            case TOKEN_GT_EQUAL: match(parser, TOKEN_GT_EQUAL); break;

            default:
                panic("unreachable");
                break;
        }

        parse_shift_expression(parser);
    }

    return NULL;
}

static Expression* parse_equality_expression(Parser* parser)
{
    parse_relational_expression(parser);

    while (has_match(parser, (TokenType[]) {TOKEN_EQUAL_EQUAL, TOKEN_NOT_EQUAL}, 2))
    {
        switch (curr_type(parser->stream))
        {
            case TOKEN_EQUAL_EQUAL: match(parser, TOKEN_EQUAL_EQUAL); break;
            case TOKEN_NOT_EQUAL: match(parser, TOKEN_NOT_EQUAL); break;

            default:
                panic("unreachable");
                break;
        }

        parse_relational_expression(parser);
    }

    return NULL;
}

static Expression* parse_and_expression(Parser* parser)
{
    parse_equality_expression(parser);

    while (is_match(parser, TOKEN_AND))
    {
        match(parser, TOKEN_AND);

        parse_equality_expression(parser);
    }

    return NULL;
}

static Expression* parse_exclusive_or_expression(Parser* parser)
{
    parse_and_expression(parser);

    while (is_match(parser, TOKEN_XOR))
    {
        match(parser, TOKEN_XOR);

        parse_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_inclusive_or_expression(Parser* parser)
{
    parse_exclusive_or_expression(parser);

    while (is_match(parser, TOKEN_OR))
    {
        match(parser, TOKEN_OR);

        parse_exclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_and_expression(Parser* parser)
{
    parse_inclusive_or_expression(parser);

    while (is_match(parser, TOKEN_AND_AND))
    {
        match(parser, TOKEN_AND_AND);

        parse_inclusive_or_expression(parser);
    }

    return NULL;
}

static Expression* parse_logical_or_expression(Parser* parser)
{
    parse_logical_and_expression(parser);

    while (is_match(parser, TOKEN_OR_OR))
    {
        match(parser, TOKEN_OR_OR);

        parse_logical_and_expression(parser);
    }

    return NULL;
}

static Expression* parse_conditional_expression(Parser* parser)
{
    parse_logical_or_expression(parser);

    if (is_match(parser, TOKEN_QUESTION))
    {
        match(parser, TOKEN_QUESTION);

        parse_expression(parser);

        match(parser, TOKEN_COLON);

        parse_conditional_expression(parser);
    }

    return NULL;
}

static Expression* parse_assignment_expression(Parser* parser)
{
    static const TokenType assignment_operators[] = {
        TOKEN_EQUAL, TOKEN_STAR_EQUAL, TOKEN_SLASH_EQUAL, TOKEN_PERCENT_EQUAL,
        TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_LT_LT_EQUAL, TOKEN_GT_GT_EQUAL,
        TOKEN_AND_EQUAL, TOKEN_XOR_EQUAL, TOKEN_OR_EQUAL
    };
    static const size_t num_operators = 
            sizeof(assignment_operators) / sizeof(assignment_operators[0]);

    /* 
     * well need to add in the other actual part but for now we'll just
     * do the top half of this production
     *
     * also how are we supposed to know the difference before even trying to
     * parse this shit... wtf do I do here????
     * 
     * unary-expression assignment-operator assignment-expression
     */

    // Some c compilers seem to just parse a conditional here so this is what
    // I will do for now until I find a better solution
    parse_conditional_expression(parser);

    if (has_match(parser, assignment_operators, num_operators))
    {
        // TODO: this is naught since we will eventually want to know the
        // TODO: type that we matched since that information is needed later
        TokenType current_type = curr_type(parser->stream);
        match(parser, current_type);
        parse_assignment_expression(parser);
    }

    return NULL;
}

static Expression* parse_constant_expression(Parser* parser)
{
    parse_conditional_expression(parser);

    return NULL;
}

static Expression* parse_expression(Parser* parser)
{
    parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        parse_assignment_expression(parser);
    }

    return NULL;
}

// For parsing statements

// Allocate and setup the current statement
static Statement* statement_allocate(Parser* parser, StatementType type)
{
    Statement* stmt = xmalloc(sizeof(Statement));

    stmt->base.type = type;
    stmt->base.loc = curr(parser->stream)->loc;
    stmt->base.parent = NULL; // TODO: change this from NULL to proper stmt

    return stmt;
}

static Statement* parse_labeled_statement(Parser* parser)
{
    switch (curr_type(parser->stream)) 
    {
        case TOKEN_IDENTIFIER:
            match(parser, TOKEN_IDENTIFIER);
            match(parser, TOKEN_COLON);
            parse_statement(parser);
            break;

        case TOKEN_CASE:
            match(parser, TOKEN_CASE);
            match(parser, TOKEN_COLON);
            parse_statement(parser);
            break;

        case TOKEN_DEFAULT:
            match(parser, TOKEN_DEFAULT);
            match(parser, TOKEN_COLON);
            parse_statement(parser);
            break;
        
        default:
            panic("bad labelled statement");
            break;
    }

    return NULL;
}
static Statement* parse_compound_statement(Parser* parser)
{
    match(parser, TOKEN_LCURLY);

    // TODO: can either be a declarations or a statement
    // TODO: so we will need some way to easily differentiate between the two
    // TODO: idk maybe statement start set or declarations start set

    // for now we will just parse a statement

    // TODO: this may not work later so we will need to change this to
    // TODO: check for statements startings...

    while (!is_match(parser, TOKEN_RCURLY))
    {
        parse_statement(parser);
    }

    match(parser, TOKEN_RCURLY);

    return NULL;
}

static Statement* parse_expression_statement(Parser* parser)
{
    // Need some way to check if we can start and expression
    // like idk might need some kind of expression start set

    if (is_match(parser, TOKEN_SEMI))
    {
        match(parser, TOKEN_SEMI);
    }
    else
    {
        parse_expression(parser);
        match(parser, TOKEN_SEMI);
    }

    return NULL;
}
static Statement* parse_selection_statement(Parser* parser)
{
    switch (curr_type(parser->stream))
    {
        case TOKEN_IF:
            match(parser, TOKEN_IF);
            match(parser, TOKEN_LPAREN);
            parse_expression(parser);
            match(parser, TOKEN_RPAREN);
            parse_statement(parser);
            // This will take care of matching the else to the closest if...
            if (is_match(parser, TOKEN_ELSE))
            {
                match(parser, TOKEN_ELSE);
                parse_statement(parser);
            }
            break;

        case TOKEN_SWITCH:
            match(parser, TOKEN_SWITCH);
            match(parser, TOKEN_LPAREN);
            parse_expression(parser);
            match(parser, TOKEN_RPAREN);
            parse_statement(parser);
            break;

        default:
            panic("bad selection statement start");
            break;
    }

    return NULL;
}
static Statement* parse_iteration_statement(Parser* parser)
{
    switch (curr_type(parser->stream))
    {
        case TOKEN_WHILE:
            match(parser, TOKEN_WHILE);
            match(parser, TOKEN_LPAREN);
            parse_expression(parser);
            match(parser, TOKEN_RPAREN);
            parse_statement(parser);
            break;

        case TOKEN_DO:
            match(parser, TOKEN_DO);
            parse_statement(parser);
            match(parser, TOKEN_WHILE);
            match(parser, TOKEN_LPAREN);
            parse_expression(parser);
            match(parser, TOKEN_RPAREN);
            match(parser, TOKEN_SEMI);
            break;

        case TOKEN_FOR:
            match(parser, TOKEN_FOR);
            match(parser, TOKEN_LPAREN);

            // TODO: for now we will assume that we have 3 expressions just
            // TODO: because I can but this will need to be redone later

            // TODO: additionally like below some distinction of declaration
            // vs expression start is needed

            // TODO: this first one should also handle a declaration
            // TODO: but im not sure how to do that just yet
            if (!is_match(parser, TOKEN_SEMI))
            {
                parse_expression(parser);
            }

            match(parser, TOKEN_SEMI);

            if (!is_match(parser, TOKEN_SEMI))
            {
                parse_expression(parser);
            }
            match(parser, TOKEN_SEMI);

            if (!is_match(parser, TOKEN_RPAREN))
            {
                parse_expression(parser);
            }
            match(parser, TOKEN_RPAREN);

            parse_statement(parser);
            break;

        default:
            panic("bad iteration statement value");
            break;
    }

    return NULL;
}
static Statement* parse_jump_statement(Parser* parser)
{
    switch (curr_type(parser->stream))
    {
        case TOKEN_GOTO:
            match(parser, TOKEN_GOTO);
            match(parser, TOKEN_IDENTIFIER);
            match(parser, TOKEN_SEMI);
            break;

        case TOKEN_CONTINUE:
            match(parser, TOKEN_CONTINUE);
            match(parser, TOKEN_SEMI);
            break;

        case TOKEN_BREAK:
            match(parser, TOKEN_BREAK);
            match(parser, TOKEN_SEMI);
            break;

        case TOKEN_RETURN:
            match(parser, TOKEN_RETURN);
            // TODO: this is hacky way of doing this would be much cleaner
            // TODO: to check if we're about to start an expression instead
            if (is_match(parser, TOKEN_SEMI))
            {
                // DO NOTHING
            }
            else
            {
                parse_expression(parser);
            }
            match(parser, TOKEN_SEMI);
            break;

        default:
            panic("bad jump statement token");
            break;
    }

    return NULL;
}

static Statement* parse_statement(Parser* parser)
{
    switch(curr_type(parser->stream))
    {
        // Here we are specifically looking for a label
        // e.g. fail: ...
        case TOKEN_IDENTIFIER:
            if (next_type(parser->stream) != TOKEN_COLON)
            {
                goto case_expression_statement;
            }
        case TOKEN_CASE:
        case TOKEN_DEFAULT:
            parse_labeled_statement(parser);
            break;

        case TOKEN_LCURLY:
            parse_compound_statement(parser);
            break;
        
        /* note that if we get a ';' we're just going to match an empty 
         * expression statement so this can easily just be reduced to match a
         * semi colon later but will leave this here for now
         */
        case TOKEN_SEMI:
            parse_expression_statement(parser);
            break;

        case TOKEN_IF:
        case TOKEN_SWITCH:
            parse_selection_statement(parser);
            break;

        case TOKEN_WHILE:
        case TOKEN_DO:
        case TOKEN_FOR:
            parse_iteration_statement(parser);
            break;

        case TOKEN_GOTO:
        case TOKEN_CONTINUE:
        case TOKEN_BREAK:
        case TOKEN_RETURN:
            parse_jump_statement(parser);
            break;

        default:
            // For now we should just try to parse an expression statement
            // even if that might not be right
            
            // TODO: eventually we want to change this to make an error statement
            // TODO: and will move the logic of parsing an expression to elsewhere

            // TODO: improve the logic here to handle this case better
case_expression_statement:
            parse_expression_statement(parser);    

            // panic("bad statement start");
            break;
    }

    return NULL;
}



