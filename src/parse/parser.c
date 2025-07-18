#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
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

// Special thanks to the below
// https://recc.robertelder.org/ll-c-grammar.txt

#define countof(array) (sizeof(array) / sizeof(array[0]))

// Below are the start set for some of the specifiers / qualifiers
static const TokenType storage_class[] = {
    TOKEN_TYPEDEF, 
    TOKEN_EXTERN, 
    TOKEN_STATIC,
    TOKEN_AUTO,
    TOKEN_REGISTER
};

static const TokenType type_specifier[] = {
    TOKEN_VOID,
    TOKEN_CHAR,
    TOKEN_SHORT,
    TOKEN_INT,
    TOKEN_LONG,
    TOKEN_FLOAT,
    TOKEN_DOUBLE,
    TOKEN_SIGNED,
    TOKEN_UNSIGNED,
    TOKEN__BOOL,
    TOKEN__COMPLEX,
    TOKEN__IMAGINARY,
    TOKEN_STRUCT,
    TOKEN_UNION,
    TOKEN_ENUM
};

static const TokenType type_qualifier[] = {
    TOKEN_CONST,
    TOKEN_RESTRICT,
    TOKEN_VOLATILE
};

static const TokenType function_specificer[] = {
    TOKEN_INLINE
};

static const size_t storage_class_count = countof(storage_class);
static const size_t type_specifier_count = countof(type_specifier);
static const size_t type_qualifier_count = countof(type_qualifier);
static const size_t function_specificer_count = countof(function_specificer);

static bool is_valid_stream_position(TokenStream* stream)
{
    return (stream->current_token < stream->count);
}

// Return the type of the current token in a token stream
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
    // go over how many we can actually eat. In the case there is none to eat
    // we just panic
    if (stream->current_token < stream->count)
    {
        stream->current_token++;
    }
    else
    {
        panic("trying to comsume a token we shouldn't...");
    }
}

// Now we are below the token stream methods we have the parser methods. These
// are the actual onces which should be called and acted upon

// Anchor token methods
static void add_recover_token(Parser* parser, TokenType type);
static void remove_recover_token(Parser* parser, TokenType type);
static void add_recover_tokens(Parser* parser, const TokenType* types, size_t count);
static void remove_recover_tokens(Parser* parser, const TokenType* types, size_t count);
static bool is_in_recover_set(Parser* parser, TokenType type);

// Get the current token from the parser
static Token* get_curr_token(Parser* parser);
static Token* get_next_token(Parser* parser);

// Parsing helper methods for matching
static void require(Parser* parser, TokenType type);
static void match(Parser* parser, TokenType type);
static bool is_match(Parser* parser, TokenType type);
static bool is_next_match(Parser* parser, TokenType type);
static bool has_match(Parser* parser, const TokenType* types, size_t count);

static void parse_error(Parser* parser, const char* fmt, ...)
{
    Token* tok = get_curr_token(parser);   

    ResolvedLocation loc = line_map_resolve_location(parser->map, tok->loc);
    fprintf(stderr, "%s:%u:%u\n", loc.name->path, loc.line, loc.col);
    va_list args;
    va_start(args, fmt);
    diag_verror(fmt, args);
    va_end(args);
}

static void add_recover_token(Parser* parser, TokenType type)
{
    assert(type && type < TOKEN_LAST);
    assert(parser->recover_set[type] != SIZE_MAX);
    // If above assertion is triggered, then there we basically have infinite
    // memory, so realistically it should never happen...

    parser->recover_set[type]++;
}

static void remove_recover_token(Parser* parser, TokenType type)
{
    assert(type && type < TOKEN_LAST);
    assert(parser->recover_set[type] != 0);

    parser->recover_set[type]--;
}

static void add_recover_tokens(Parser* parser, const TokenType* types, size_t count)
{
    for (size_t i = 0; i < count; i++)
    {
        add_recover_token(parser, types[i]);
    }
}

static void remove_recover_tokens(Parser* parser, const TokenType* types, size_t count)
{
    for (size_t i = 0; i < count; i++)
    {
        remove_recover_token(parser, types[i]);
    }
}

static bool is_in_recover_set(Parser* parser, TokenType type)
{
    return (parser->recover_set[type] != 0);
}

static Token* get_curr_token(Parser* parser)
{
    return curr(parser->stream);
}

static Token* get_next_token(Parser* parser)
{
    return next(parser->stream);
}

// The require function requires a token to be of that type. I.e. we should
// have already checked we got that type and to get any other type would mean
// that something has gone quite wrong. In this case I think it is more than 
// okay that we just give up all together
static void require(Parser* parser, TokenType type)
{
    assert(is_match(parser, type));
    consume(parser->stream);
}

static void eat_until(Parser* parser, TokenType type)
{
    // Can't have a match for the type when this function is called
    assert(!is_match(parser, type));

    while (!is_match(parser, type) && !is_match(parser, TOKEN_EOF))
    {
        consume(parser->stream);
    }
}

static void eat_until_recover(Parser* parser)
{
    while (true)
    {
        TokenType curr = curr_type(parser->stream);

        if (is_in_recover_set(parser, curr))
        {
            break;
        }

        consume(parser->stream);
    }
}

// A parsing method to synchronise the stream of tokens in the event of an error
// will go until we hit the a token in the recovery set. This is using panic mode
// recovery and this should work pretty well in c...
static void recover(Parser* parser, TokenType type)
{
    eat_until(parser, type);

    if (!is_match(parser, TOKEN_EOF))
    {
        assert(is_match(parser, type));
        consume(parser->stream);
    }

    // TODO: change this to basic recover set stuff once we are parsing everything
    // TODO: also might also consider adding statement level recovery?

    // while (true)
    // {
    //     TokenType curr = curr_type(parser->stream);
    //     if (parser->recover_set[curr] != 0)
    //     {
    //         break;
    //     }

    //     // TODO: maybe add some things in here handle braces / parens / brackets
    //     // TODO: better

    //     consume(parser->stream);
    // }
}

static void match(Parser* parser, TokenType type)
{
    TokenStream* stream = parser->stream;

    if (curr_type(stream) == type)
    {
        consume(stream);

        return;
    }

    parse_error(parser, "expected '%s' but got '%s'", 
            token_type_get_name(type), 
            token_type_get_name(curr_type(stream))
        );

    recover(parser, type);    

    return;
}

static bool is_match(Parser* parser, TokenType type)
{
    return (curr_type(parser->stream) == type);
}

static bool is_next_match(Parser* parser, TokenType type)
{
    return (next_type(parser->stream) == type);
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

// TODO: we definitely want to add methods like for example the below
static bool is_typename_start(Parser* parser, const Token* tok);
static bool is_expression_start(Parser* parser, const Token* tok);
// TODO: however I think we will need a symbol table and stuff for the next part
// TODO: for sure though

// TODO: implement below so we can do important stuff
static bool is_declorator_start(Parser* parser, const Token* tok);
static bool is_abstract_declarator_start(Parser* parser, const Token* tok);

// Functions for parsing our constants which include integer, floating point
// enumeration and character constants
static Expression* parse_integer_constant(Parser* parser);
static Expression* parse_floating_constant(Parser* parser);
static Expression* parse_enumeration_constant(Parser* parser);
static Expression* parse_character_constant(Parser* parser);
static Expression* parse_constant(Parser* parser);

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
static Statement* parse_label_statement(Parser* parser);
static Statement* parse_case_statement(Parser* parser);
static Statement* parse_default_statement(Parser* parser);
static Statement* parse_compound_statement(Parser* parser);
static Statement* parse_expression_statement(Parser* parser);
static Statement* parse_selection_statement(Parser* parser);
static Statement* parse_iteration_statement(Parser* parser);
static Statement* parse_jump_statement(Parser* parser);
static Statement* parse_statement(Parser* parser);

// All of our functions for parsing declarations / definitions
// TODO: maybe all of these don't need to return a declaration type???
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

static void* parse_type_name(Parser* parser);

// The definitions of the functions we will use for pasing

// Some functions for creating errors

// TODO: add things for error recovery and whatnot to make parsing better

static bool is_typename_start(Parser* parser, const Token* tok)
{
    const TokenType type = tok->type;
    switch (type)
    {
        case TOKEN_VOID:
        case TOKEN_CHAR:
        case TOKEN_SHORT:
        case TOKEN_INT:
        case TOKEN_LONG:
        case TOKEN_FLOAT:
        case TOKEN_DOUBLE:
        case TOKEN_SIGNED:
        case TOKEN_UNSIGNED:
        case TOKEN__BOOL:
        case TOKEN__COMPLEX:
        case TOKEN__IMAGINARY:
        case TOKEN_STRUCT:
        case TOKEN_UNION:
        case TOKEN_ENUM:
            return true;

        case TOKEN_IDENTIFIER: // TODO: once we make a symbol table and stuff
            return false;

        default:
            return false;
    }
}

static bool is_expression_start(Parser* parser, const Token* tok)
{
    const TokenType type = tok->type;

    switch (type)
    {
        case TOKEN_NUMBER:
        case TOKEN_WIDE_CHARACTER:
        case TOKEN_WIDE_STRING:
        case TOKEN_CHARACTER:
        case TOKEN_STRING:
        case TOKEN_LPAREN:
        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS:
        case TOKEN_AND:
        case TOKEN_STAR:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_NOT:
        case TOKEN_TILDE:
        case TOKEN_SIZEOF:
            return true;

        case TOKEN_IDENTIFIER:
            return true; // TODO: fix once we have a proper symbol table
        
        default:
            return false;
    }
}

static Expression* parse_integer_constant(Parser *parser);
static Expression* parse_floating_constant(Parser *parser);
static Expression* parse_number_constant(Parser* parser);
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
        // TODO: here when we create the expression make sure it is set to
        // TODO: be in parens

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
        panic("expected expression");
    }

    return NULL;
}

static Expression* parse_postfix_expression(Parser* parser)
{
    /* For postfix expression we ignore the following rule:
     * ( type-name ) { initializer-list } 
     * this rule is actually handled in the function parse_cast_expression. See
     * that function for details
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
                if (!is_match(parser, TOKEN_RPAREN))
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
            require(parser, TOKEN_SIZEOF);
            if (is_match(parser, TOKEN_LPAREN) && 
                    is_typename_start(parser, get_next_token(parser)))
            {
                match(parser, TOKEN_LPAREN);
                parse_type_name(parser);
                match(parser, TOKEN_RPAREN);
            }
            else
            {
                parse_unary_expression(parser);
            }
            break;

        default:
            parse_postfix_expression(parser);
            break;
    }

    return NULL;
}

static Expression* parse_cast_expression(Parser* parser)
{
    /* ( type-name ) cast-expression */
    while (is_match(parser, TOKEN_LPAREN) && 
            is_typename_start(parser, get_next_token(parser)))
    {
        require(parser, TOKEN_LPAREN);


        Declaration* decl = parse_type_name(parser);
        
        
        match(parser, TOKEN_RPAREN);

        /* ( type-name ) { initializer-list }
         * ( type-name ) { initializer-list , }
         * 
         * Although this is technically a postfix expression we cannot handle it
         * there is we eat all of the typenames so handle it here.
         */
        if (is_match(parser, TOKEN_LCURLY))
        {
            require(parser, TOKEN_LCURLY);
            
            parse_initializer_list(parser);

            if (is_match(parser, TOKEN_COMMA))
            {
                require(parser, TOKEN_COMMA);
            }

            match(parser, TOKEN_RCURLY);

            return NULL;
        }
    }

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
    static const TokenType additive_operators[] = {TOKEN_PLUS, TOKEN_MINUS};
    static const size_t num_operators = countof(additive_operators);

    add_recover_tokens(parser, additive_operators, num_operators);

    Expression* expr = parse_multiplicative_expression(parser);

    while (has_match(parser, additive_operators, num_operators))
    {
        /* Need to create a new binary expression and allocate it that way and
         * then set its type to match the corrosponding operator
         */
        
        ExpressionType type = EXPRESSION_ERROR;
        switch (curr_type(parser->stream))
        {
            case TOKEN_PLUS: 
                require(parser, TOKEN_PLUS);
                type = EXPRESSION_BINARY_ADD; 
                break;

            case TOKEN_MINUS: 
                require(parser, TOKEN_MINUS);
                type = EXPRESSION_BINARY_SUBTRACT; 
                break;

            default: 
                panic("unreachable"); 
                break;
        }

        Expression* rhs = parse_multiplicative_expression(parser);

        // TODO: get new binary expression tmp with type type
        Expression* tmp = NULL; 
        // TODO: assign tmp's lhs to expr, and tmp's rhs to rhs
        
        expr = tmp;

        // To stop warning about unused for now
        (void) type;
    }

    remove_recover_tokens(parser, additive_operators, num_operators);

    return expr;
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
    Expression* expr = parse_logical_or_expression(parser);

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
    return parse_conditional_expression(parser);
}

static Expression* parse_expression(Parser* parser)
{
    add_recover_token(parser, TOKEN_COMMA);

    Expression* expr = parse_assignment_expression(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        require(parser, TOKEN_COMMA);

        Expression* rhs = parse_assignment_expression(parser);

        // TODO: build the expression here
    }

    remove_recover_token(parser, TOKEN_COMMA);

    return expr;
}

// For parsing statements

// Allocate and setup the current statement
static Statement* statement_allocate(Parser* parser, StatementType type)
{
    Statement* stmt = xmalloc(sizeof(Statement));

    stmt->base.type = type;
    stmt->base.loc = get_curr_token(parser)->loc;
    stmt->base.parent = NULL; // TODO: change this from NULL to proper stmt

    return stmt;
}

static void statement_free(Statement* stmt)
{
    free(stmt);
}

static Statement* parse_label_statement(Parser* parser)
{
    Statement* stmt = statement_allocate(parser, STATEMENT_LABEL);

    Token* tok = get_curr_token(parser);

    require(parser, TOKEN_IDENTIFIER);
    require(parser, TOKEN_COLON);

    // Somehow we want to get the name from the token
    stmt->label_stmt.name = (String) {0};
    stmt->label_stmt.statement = parse_statement(parser);

    statement_free(stmt);

    return NULL;
}

static Statement* parse_case_statement(Parser* parser)
{
    // TODO: we will need to check somewhere that we are actually within the
    // TODO: context of a switch statment
    
    Statement* stmt = statement_allocate(parser, STATEMENT_CASE);

    require(parser, TOKEN_CASE);

    // TODO: somewhere we will need to parse the constant expression and then
    // TODO: eventually fold it somewhere
    
    // Get the experssion and the statment
    stmt->case_stmt.constant_expression = parse_constant_expression(parser);
    
    // Get the colon and the rest of the statment
    match(parser, TOKEN_COLON);

    stmt->case_stmt.statement = parse_statement(parser);

    statement_free(stmt);

    return NULL;
}

static Statement* parse_default_statement(Parser* parser)
{
    // TODO: add in check to make sure we are in the context of a switch
    // TODO: otherwise this is invalid

    Statement* stmt = statement_allocate(parser, STATEMENT_CASE);

    require(parser, TOKEN_DEFAULT);

    match(parser, TOKEN_COLON);

    stmt->default_stmt.statement = parse_statement(parser);

    statement_free(stmt);

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
    Statement* stmt;

    switch(curr_type(parser->stream))
    {
        // Here we are specifically looking for a label
        // e.g. fail: ...
        // TODO: we will also eventually need to check if the identifier might
        // TODO: be a type name or not i think
        case TOKEN_IDENTIFIER:
            if (next_type(parser->stream) != TOKEN_COLON)
            {
                goto case_expression_statement;
            }
            stmt = parse_label_statement(parser);
            break;

        case TOKEN_CASE:
            stmt = parse_case_statement(parser);
            break;

        case TOKEN_DEFAULT:
            stmt = parse_default_statement(parser);
            break;

        case TOKEN_LCURLY:
            stmt = parse_compound_statement(parser);
            break;
        
        /* note that if we get a ';' we're just going to match an empty 
         * expression statement so this can easily just be reduced to match a
         * semi colon later but will leave this here for now
         */
        case TOKEN_SEMI:
            stmt = parse_expression_statement(parser);
            break;

        case TOKEN_IF:
        case TOKEN_SWITCH:
            stmt = parse_selection_statement(parser);
            break;

        case TOKEN_WHILE:
        case TOKEN_DO:
        case TOKEN_FOR:
            stmt = parse_iteration_statement(parser);
            break;

        case TOKEN_GOTO:
        case TOKEN_CONTINUE:
        case TOKEN_BREAK:
        case TOKEN_RETURN:
            stmt = parse_jump_statement(parser);
            break;

        default:
            // For now we should just try to parse an expression statement
            // even if that might not be right
            
            // TODO: eventually we want to change this to make an error statement
            // TODO: and will move the logic of parsing an expression to elsewhere

            // TODO: improve the logic here to handle this case better
case_expression_statement:
            if (is_expression_start(parser, get_curr_token(parser)))
            {
                stmt = parse_expression_statement(parser);    
            }
            else if (is_typename_start(parser, get_curr_token(parser)))
            {
                parse_declaration(parser);

                stmt = NULL;
            }
            else
            {
                stmt = NULL;

                match(parser, TOKEN_EOF);

                panic("bad statement start");
            }

            break;
    }

    if (stmt != NULL)
    {
        panic("random test");
    }

    return NULL;
}

static Declaration* parse_designation(Parser* parser)
{
    parse_designator_list(parser);

    match(parser, TOKEN_EQUAL);
 
    return NULL;
}

static Declaration* parse_designator_list(Parser* parser)
{
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_DOT}, 2))
    {
        parse_designator(parser);
    }
 
    return NULL;
}

static Declaration* parse_designator(Parser* parser)
{
    if (is_match(parser, TOKEN_LBRACKET))
    {
        match(parser, TOKEN_LBRACKET);

        parse_constant_expression(parser);

        match(parser, TOKEN_RBRACKET);
    }
    else if (is_match(parser, TOKEN_DOT))
    {
        match(parser, TOKEN_DOT);
        match(parser, TOKEN_IDENTIFIER);
    }
    else
    {
        panic("parse_designator");
    }
 
    return NULL;
}

static Declaration* parse_initializer(Parser* parser)
{
    if (is_match(parser, TOKEN_LCURLY))
    {
        match(parser, TOKEN_LCURLY);

        parse_initializer_list(parser);

        if (is_match(parser, TOKEN_COMMA))
        {
            require(parser, TOKEN_COMMA);
        }

        match(parser, TOKEN_RCURLY);
    }
    else
    {
        parse_assignment_expression(parser);
    }

 
    return NULL;
}

static Declaration* parse_initializer_list(Parser* parser)
{
    while (!is_match(parser, TOKEN_RCURLY))
    {
        // If we can match a designation do that...
        if (has_match(parser, (TokenType[]) {TOKEN_DOT, TOKEN_LBRACKET}, 2))
        {
            parse_designation(parser);
        }

        // Then get the initializer
        parse_initializer(parser);

        // End of initializer list e.g. {..., }
        if (is_match(parser, TOKEN_COMMA) && is_next_match(parser, TOKEN_RCURLY))
        {
            break;
        }
        else if (is_match(parser, TOKEN_RCURLY))
        {
            break;
        }
        else // Get the comma and keep going
        {
            match(parser, TOKEN_COMMA);
            assert(!is_match(parser, TOKEN_RCURLY));
        }
    }

    return NULL;
}

static Declaration* parse_declarator(Parser* parser)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }

    parse_direct_declarator(parser);
 
    return NULL;
}

static Declaration* parse_init_declarator(Parser* parser)
{
    parse_declarator(parser);

    if (is_match(parser, TOKEN_EQUAL))
    {
        match(parser, TOKEN_EQUAL);
        parse_initializer(parser);
    }


    return NULL;
}

static Declaration* parse_init_declarator_list(Parser* parser)
{
    parse_init_declarator(parser);

    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        parse_init_declarator(parser);
    }
 
    return NULL;
}

static Declaration* parse_typedef_name(Parser* parser)
{
    // TODO: fix this later
    match(parser, TOKEN_IDENTIFIER);
 
    return NULL;
}

static Declaration* parse_enumerator(Parser* parser)
{
    match(parser, TOKEN_IDENTIFIER);
    if (is_match(parser, TOKEN_EQUAL))
    {
        require(parser, TOKEN_EQUAL);
        parse_constant_expression(parser);
    }

    return NULL;
}

static Declaration* parse_enumerator_list(Parser* parser)
{
    parse_enumerator(parser);

    while (is_match(parser, TOKEN_COMMA) && !is_next_match(parser, TOKEN_RCURLY))
    {
        require(parser, TOKEN_COMMA);

        parse_enumerator(parser);
    }
 
    return NULL;
}

static Declaration* parse_enum_specificer(Parser* parser)
{
    match(parser, TOKEN_ENUM);

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);

        if (!is_match(parser, TOKEN_LCURLY))
        {
            return NULL;
        }
    }

    // Here we should match a left curly
    match(parser, TOKEN_LCURLY);

    parse_enumerator_list(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        require(parser, TOKEN_COMMA);
    }

    match(parser, TOKEN_RCURLY);

    return NULL;
}

static Declaration* parse_direct_declarator(Parser* parser)
{   
    // Parse the first part
    switch (curr_type(parser->stream))
    {
        case TOKEN_IDENTIFIER:
            match(parser, TOKEN_IDENTIFIER);
            break;
        case TOKEN_LPAREN:
            match(parser, TOKEN_LPAREN);
            parse_declarator(parser);
            match(parser, TOKEN_RPAREN);
            break;

        default:
            panic("bad parse_direct_declarator");
            break;
    }

    while (has_match(parser, (TokenType[]) {TOKEN_LPAREN, TOKEN_LBRACKET}, 2))
    {
        switch (curr_type(parser->stream))
        {
            case TOKEN_LPAREN:
                match(parser, TOKEN_LPAREN);

                if (is_match(parser, TOKEN_IDENTIFIER))
                {
                    parse_identifier_list(parser);
                }
                else if (is_match(parser, TOKEN_RPAREN))
                {
                    /* Do nothing */
                }
                else
                {
                    parse_paramater_type_list(parser);
                }

                match(parser, TOKEN_RPAREN);
                break;

            case TOKEN_LBRACKET:
            match(parser, TOKEN_LBRACKET);

            if (is_match(parser, TOKEN_STAR))
            {
                match(parser, TOKEN_STAR);
            }
            else if (has_match(parser, type_qualifier, type_qualifier_count))
            {
                parse_type_qualifier_list(parser);

                if (is_match(parser, TOKEN_STATIC))
                {
                    match(parser, TOKEN_STATIC);
                    parse_assignment_expression(parser);
                }
                else if (is_match(parser, TOKEN_STAR))
                {
                    match(parser, TOKEN_STAR);
                }
                else
                {
                    /* should be parsing an assignment expression otherwise
                     * but I don't currently have the tools to confirm this...
                     * TODO: add check to make sure its an assignment expression
                     */
                    parse_assignment_expression(parser);
                }
            }
            else if (is_match(parser, TOKEN_STATIC))
            {
                match(parser, TOKEN_STATIC);

                if (has_match(parser, type_qualifier, type_qualifier_count))
                {
                    parse_type_qualifier_list(parser);
                }

                parse_assignment_expression(parser);
            }
            else if (is_match(parser, TOKEN_NUMBER))
            {
                match(parser, TOKEN_NUMBER);
            }

            match(parser, TOKEN_RBRACKET);
            break;

            default:
                panic("unreachable");
                break;
        }
    }
 
    return NULL;
}

static Declaration* parse_direct_abstract_declarator(Parser* parser)
{
    if (is_match(parser, TOKEN_LPAREN))
    {
        match(parser, TOKEN_LPAREN);

        if (is_match(parser, TOKEN_RPAREN))
        {
            match(parser, TOKEN_RPAREN);
        }
        else if (is_typename_start(parser, get_curr_token(parser)))
        {
            parse_paramater_type_list(parser);

            match(parser, TOKEN_RPAREN);
        }
        else
        {
            parse_abstract_declarator(parser);

            match(parser, TOKEN_RPAREN);
        }
    }
    else if (is_match(parser, TOKEN_LBRACKET))
    {
        match(parser, TOKEN_LBRACKET);

        if (is_match(parser, TOKEN_RBRACKET))
        {
            match(parser, TOKEN_RBRACKET);
        }
        else
        {
            parse_constant_expression(parser);
        }
    }
    else
    {
        panic("unexpected token type...");
    }

    // TODO: make this code better

    // Now parse the rest of it
    while (has_match(parser, (TokenType[]) {TOKEN_LBRACKET, TOKEN_LPAREN}, 2))
    {
        if (is_match(parser, TOKEN_LPAREN))
        {
            match(parser, TOKEN_LPAREN);

            if (is_match(parser, TOKEN_RPAREN))
            {
                match(parser, TOKEN_RPAREN);
            }
            else if (is_typename_start(parser, get_curr_token(parser)))
            {
                parse_paramater_type_list(parser);

                match(parser, TOKEN_RPAREN);
            }
        }

        if (is_match(parser, TOKEN_LBRACKET))
        {
            match(parser, TOKEN_LBRACKET);

            if (is_match(parser, TOKEN_RBRACKET))
            {
                match(parser, TOKEN_RBRACKET);
            }
            else
            {
                assert(is_expression_start(parser, get_curr_token(parser)));

                parse_constant_expression(parser);
            }
        }
        else
        {
            panic("unreachale");
        }
    }

    return NULL;
}

static Declaration* parse_abstract_declarator(Parser* parser)
{
    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }
    else
    {
        parse_direct_abstract_declarator(parser);
    }

    return NULL;
}

static Declaration* parse_pointer(Parser* parser)
{
    match(parser, TOKEN_STAR);

    while (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }

    if (is_match(parser, TOKEN_STAR))
    {
        parse_pointer(parser);
    }
 
    return NULL;
}

static Declaration* parse_identifier_list(Parser* parser)
{
    match(parser, TOKEN_LPAREN);
    match(parser, TOKEN_IDENTIFIER);
    while (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        match(parser, TOKEN_IDENTIFIER);
    }

    match(parser, TOKEN_RPAREN);
 
    return NULL;
}

static Declaration* parse_paramater_declaration(Parser* parser)
{
    /*
        declaration-specifiers declarator
        declaration-specifiers abstract-declarator opt
        
                               ^^^^^^^^^^^^^^^^^^^^^^^

        in the above rule we are currently ignoring that abstract declarator
        bs since I cba determining first sets and such
    */

    // TODO: determine how to do the above

    parse_declaration_specifiers(parser);

    // TODO: in the case of 'void' this parses the declaration specifiers then
    // has to kill itself since there is only ')' after it. So make the next
    // part options

    // TODO: leave this true for now then we will just fix it up later hopefully
    // TODO: need some function to determine if we're starting a declaration or not
    if (true)
    {
        parse_declarator(parser);
    }
 
    return NULL;
}

static Declaration* parse_paramater_list(Parser* parser)
{
    parse_paramater_declaration(parser);
    // Make sure we don't eat the , ... part if it exists
    while (is_match(parser, TOKEN_COMMA) && next_type(parser->stream) != TOKEN_ELIPSIS)
    {
        match(parser, TOKEN_COMMA);
        parse_paramater_declaration(parser);
    }

    return NULL;
}

static Declaration* parse_paramater_type_list(Parser* parser)
{
    parse_paramater_list(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        match(parser, TOKEN_COMMA);
        match(parser, TOKEN_ELIPSIS);
    }
    
    return NULL;
}

static Declaration* parse_type_qualifier_list(Parser* parser)
{
    parse_type_qualifier(parser);

    while (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }
 
    return NULL;
}

static Declaration* parse_specifier_qualifier_list(Parser* parser)
{
    if (has_match(parser, type_qualifier, type_qualifier_count))
    {
        parse_type_qualifier(parser);
    }
    else if (has_match(parser, type_specifier, type_specifier_count))
    {
        parse_type_specifier(parser);
    }
    else
    {
        match(parser, TOKEN_EOF);

        panic("expected specifier or qualifier");
    }

    // TODO: this could be a bit cleaner????

    while (has_match(parser, type_qualifier, type_qualifier_count)
            || has_match(parser, type_specifier, type_specifier_count))
    {
        if (has_match(parser, type_qualifier, type_qualifier_count))
        {
            parse_type_qualifier(parser);
        }
        else if (has_match(parser, type_specifier, type_specifier_count))
        {
            parse_type_specifier(parser);
        }
    }

    return NULL;
}

static Declaration* parse_struct_declarator(Parser* parser)
{   
    // TODO: this is a really bad way to do this

    // If we get a bit field off the bar
    if (is_match(parser, TOKEN_COMMA))
    {
        require(parser, TOKEN_COMMA);
        parse_constant_expression(parser);

        return NULL;
    }

    parse_declarator(parser);

    // If we maybe have a bitfield after
    if (is_match(parser, TOKEN_COMMA))
    {
        require(parser, TOKEN_COMMA);
        parse_constant_expression(parser);

        return NULL;
    }

    return NULL;
}

static Declaration* parse_struct_declarator_list(Parser* parser)
{
    parse_struct_declarator(parser);

    if (is_match(parser, TOKEN_COMMA))
    {
        require(parser, TOKEN_COMMA);
        parse_struct_declarator(parser);
    }
 
    return NULL;
}

static Declaration* parse_struct_declaration(Parser* parser)
{
    parse_specifier_qualifier_list(parser);
    parse_struct_declarator_list(parser);

    match(parser, TOKEN_SEMI);

    return NULL;
}

static Declaration* parse_struct_declaration_list(Parser* parser)
{
    while (get_curr_token(parser)->type != TOKEN_RCURLY)
    {
        parse_struct_declaration(parser);
    }

    return NULL;
}

static Declaration* parse_struct_or_union(Parser* parser)
{
    if (is_match(parser, TOKEN_STRUCT))
    {
        require(parser, TOKEN_STRUCT);
    }
    else if (is_match(parser, TOKEN_UNION))
    {
        require(parser, TOKEN_UNION);
    }
    else
    {
        panic("expected struct or union");
    }
 
    return NULL;
}

static Declaration* parse_struct_or_union_specifier(Parser* parser)
{
    assert(has_match(parser, (TokenType[]) {TOKEN_STRUCT, TOKEN_UNION}, 2));

    match(parser, curr_type(parser->stream));

    if (is_match(parser, TOKEN_IDENTIFIER))
    {
        match(parser, TOKEN_IDENTIFIER);

        // We matched an identifier so continuing the struct defn is optional...
        if (!is_match(parser, TOKEN_LCURLY))
        {
            return NULL;
        }
    }
    
    if (is_match(parser, TOKEN_LCURLY))
    {
        match(parser, TOKEN_LCURLY);
        parse_struct_declaration_list(parser);
        match(parser, TOKEN_RCURLY);
    }
    
    return NULL;
}

static Declaration* parse_function_specificer(Parser* parser)
{
    // Nothing else to do here
    match(parser, curr_type(parser->stream));
 
    return NULL;
}

static Declaration* parse_type_qualifier(Parser* parser)
{
    assert(has_match(parser, type_qualifier, type_qualifier_count));

    // Nothing else to do here
    match(parser, curr_type(parser->stream));
 
    return NULL;
}

static Declaration* parse_type_specifier(Parser* parser)
{
    if (has_match(parser, (TokenType[]) {TOKEN_STRUCT, TOKEN_UNION}, 2))
    {
        parse_struct_or_union_specifier(parser);
    }
    else if (is_match(parser, TOKEN_ENUM))
    {
        parse_enum_specificer(parser);
    }
    else
    {
        match(parser, curr_type(parser->stream));
    }

    return NULL;
}

static Declaration* parse_storage_class_specifier(Parser* parser)
{
    // Nothing else to do here
    match(parser, curr_type(parser->stream));
 
    return NULL;
}

static Declaration* parse_declaration_specifiers(Parser* parser)
{
    while (true)
    {
        if (has_match(parser, storage_class, storage_class_count))
        {
            parse_storage_class_specifier(parser);

            continue;
        }
        else if (has_match(parser, type_specifier, type_specifier_count))
        {
            parse_type_specifier(parser);

            continue;
        }
        else if (has_match(parser, type_qualifier, type_qualifier_count))
        {
            parse_type_qualifier(parser);

            continue;
        }
        else if (has_match(parser, function_specificer, function_specificer_count))
        {
            parse_function_specificer(parser);

            continue;
        }

        break;
    }


    return NULL;
}

static Declaration* parse_declaration(Parser* parser)
{
    parse_declaration_specifiers(parser);

    if (!is_match(parser, TOKEN_SEMI))
    {
        parse_init_declarator_list(parser);
    }

    return NULL;
}

static Declaration* parse_declaration_or_definition(Parser* parser)
{
    Declaration* decl = parse_declaration(parser);

    /* TODO: add declaration to list of declarations */

    if (is_match(parser, TOKEN_SEMI))
    {
        require(parser, TOKEN_SEMI);

        return NULL;
    }
    else if (is_match(parser, TOKEN_LCURLY))
    {
        parse_compound_statement(parser);

        // TODO: add the function to list of functions and that

        return NULL;
    }
    else 
    {
        match(parser, TOKEN_EOF);

        panic("expected ';' or '{'");
    }

    return NULL;
}

static void* parse_type_name(Parser* parser)
{
    Declaration* decl = parse_specifier_qualifier_list(parser);
    // TODO: parse abstract declarator if needed

    return NULL;
}

// The definitions of the functions we will use for pasing


void parse_translation_unit(TokenStream* stream, LineMap* map)
{
    Parser parser = {.stream = stream, .map = map};

    // add so we can never go past
    add_recover_token(&parser, TOKEN_EOF);

    while (curr_type(stream) != TOKEN_EOF)
    {
        // if (is_expression_start(&parser, get_curr_token(&parser)))
        // {
        //     parse_expression(&parser);
        // }
        // else
        // {
        //     eat_until(&parser, TOKEN_SEMI);
        // }
        // match(&parser, TOKEN_SEMI);
        parse_declaration_or_definition(&parser);
    }

    remove_recover_token(&parser, TOKEN_EOF);

    return;
}
