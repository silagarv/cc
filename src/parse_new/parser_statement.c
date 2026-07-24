#include "parse/declaration.h"
#include "parser_new.h"

#include <stddef.h>
#include <assert.h>

#include "files/location.h"

#include "lex/identifier_table.h"
#include "lex/token.h"

#include "parse/scope.h"
#include "parse/semantic.h"
#include "parse/statement.h"

// A temporary structure for the parsing and chaining of statements so that we
// are able to quickly parse and create a compound statement.
typedef struct StatementListTmp {
    Statement* first;
    Statement* last;
} StatementListTmp;

StatementListTmp statement_list_tmp_create(void)
{
    return (StatementListTmp) { NULL, NULL };
}

void statement_list_tmp_push(StatementListTmp* stmts, Statement* stmt)
{
    if (stmts->first == NULL)
    {
        stmts->first = stmt;
    }

    if (stmts->last == NULL)
    {
        stmts->last = stmt;
    }
    else
    {
        statement_set_next(stmts->last, stmt);
    }
}

Statement* statement_list_tmp_first(StatementListTmp* stmts)
{
    return stmts->first;
}

Statement* parse_compound_statement_internal(Parser* parser)
{
    StatementListTmp stmts = statement_list_tmp_create();

    Location lcurly = parser_consume(parser);
    Location rcurly = LOCATION_INVALID;

    while (!parser_has(parser, TOK_RCURLY, TOK_EOF))
    {
        Statement* stmt = parse_statement(parser, true);
        statement_list_tmp_push(&stmts, stmt);
    }

    if (!parser_try_consume(parser, TOK_RCURLY, &rcurly))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected '}'");
    }

    // Get the first statement from our temporary list so that we can easily
    // create the compound statement.
    Statement* first = statement_list_tmp_first(&stmts);
    return semantic_checker_handle_compound_statement(&parser->sc, lcurly,
            first, rcurly);
}

Statement* parse_compound_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_LCURLY) && "not a compound statement?");
    
    Scope scope = scope_block(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* stmt = parse_compound_statement_internal(parser);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return stmt;
}

Location parse_expected_colon(Parser* parser, const char* msg)
{
    if (!parser_is(parser, TOK_COLON))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ':' after '%s'", msg);
        return LOCATION_INVALID;
    }

    return parser_consume(parser);
}

Statement* parse_statement_after_label(Parser* parser, const char* ctx)
{
    if (parser_is(parser, TOK_RCURLY))
    {
        if (!lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, parser_current_location(parser),
                    Wc23_extensions, "%s at end of compound statement is a C23 "
                    "extension", ctx);
        }
    
        return semantic_checker_handle_empty_statement(&parser->sc,
                LOCATION_INVALID);
    }
    else if (parser_is_statement(parser))
    {
        if (!lang_opts_c23(parser->lang))
        {
            diagnostic_warning_at(parser->dm, parser_current_location(parser),
                    Wc23_extensions, "%s followed by a declaration is a C23 "
                    "extension", ctx);
        }
        // Don't return and go on to parse a statement after.
    }
    else if (!parser_is_statement(parser))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected expression after %s", ctx);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    return parse_statement(parser, true);
}

Statement* parse_case_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_case) && "not a case statement");

    Location case_loc = parser_consume(parser);
    Expression* expr = parse_constant_expression(parser);

    if (!semantic_checker_check_case_expression(&parser->sc, &expr))
    {
        if (!parser_is(parser, TOK_COLON))
        {
            parser_recover_two(parser, TOK_COLON, TOK_RCURLY,
                    RECOVER_STOP_AT_SEMI);
            return semantic_checker_handle_error_statement(&parser->sc);
        }
    }

    Location colon_loc = parse_expected_colon(parser, "case");

    // Check if we we're allowed to have a case and just return an invalid
    // statement if so.
    if (!semantic_checker_check_case_allowed(&parser->sc, case_loc))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* body = parse_statement_after_label(parser, "case label");
    return semantic_checker_handle_case_statement(&parser->sc, case_loc,
            expr, colon_loc, body);
}

Statement* parse_default_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_default) && "not a default statement?");

    Location default_loc = parser_consume(parser);
    Location colon = parse_expected_colon(parser, "default");

    // Check if we are allowed or not, aborting if this statement should be here
    if (!semantic_checker_check_default_allowed(&parser->sc, default_loc))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* stmt = parse_statement_after_label(parser, "default label");
    return semantic_checker_handle_default_statement(&parser->sc, default_loc,
            colon, stmt);
}

bool parse_expression_for_statement(Parser* parser, Location kw_location,
        Location* lparen_loc, Expression** cond, Location* rparen_loc,
        bool is_switch, const char* context)
{
    if (!parser_is(parser, TOK_LPAREN))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected '(' after '%s'", context);
        parser_recover(parser, TOK_SEMI, RECOVER_EAT);
        return false;
    }

    *lparen_loc = parser_consume(parser);

    // Parse and check the condition is valid
    *cond = parse_expression(parser);
    *cond = semantic_checker_check_condition(&parser->sc, kw_location, *cond,
            is_switch, context);

    if (!parser_is(parser, TOK_RPAREN))
    {
        // TODO: could better error recovery here be to check for a statement
        // TODO: start somehow and then only sometimes killing the parse
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ')' after condition");
        parser_recover(parser, TOK_SEMI, RECOVER_EAT);
        return false;
    }

    *rparen_loc = parser_consume(parser);

    // Clang will emit errors for exsessive paren usage. But continue on parsing
    // as if there was no error, since this error is likely only limited to one
    // paren anyways
    while (parser_is(parser, TOK_RPAREN))
    {
        diagnostic_error_at(parser->dm, parser_consume(parser),
                "extraneous ')' after condition, expected a statement");
    }

    return true;
}

Statement* parse_if_statement_internal(Parser* parser)
{
    Location if_loc = parser_consume(parser);

    Location lparen = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, if_loc, &lparen, &cond,
            &rparen, /*is_switch*/false, "if"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // FIXME: apparently the body of an 'if' statement is a scope even if it
    // FIXME: isn't a compound scope. Will also need to do this for 'else'
    Statement* if_body = parse_statement(parser, false);

    Location else_loc = LOCATION_INVALID;
    Statement* else_body = NULL;
    if (parser_is(parser, TOK_else))
    {
        else_loc = parser_consume(parser);
        else_body = parse_statement(parser, false);
    }

    return semantic_checker_handle_if_statement(&parser->sc, if_loc, lparen,
            cond, rparen, if_body, else_loc, else_body);
}

Statement* parse_if_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_if) && "not a if statement");

    Scope scope = scope_if(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* out = parse_if_statement_internal(parser);
    
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return out;
}

Statement* parse_switch_statement_internal(Parser* parser)
{
    Location switch_loc = parser_consume(parser);

    Location lparen = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, switch_loc, &lparen, &cond,
            &rparen, /*is_switch*/true, "switch"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    semantic_checker_push_switch_stack(&parser->sc);

    Statement* body = parse_statement(parser, false);
    Statement* out = semantic_checker_handle_switch_statement(&parser->sc,
            switch_loc, lparen, cond, rparen, body);

    semantic_checker_pop_switch_stack(&parser->sc);

    return out;
}

Statement* parse_switch_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_switch) && "not a switch statement");

    Scope scope = scope_switch(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* out = parse_switch_statement_internal(parser);
    
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return out;
}

Statement* parse_while_statement_internal(Parser* parser)
{
    Location while_loc = parser_consume(parser);

    Location lparen = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, while_loc, &lparen, &cond,
            &rparen, /*is_switch*/false, "while"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* body = parse_statement(parser, false);
    return semantic_checker_handle_while_statement(&parser->sc, while_loc, 
            lparen, cond, rparen, body);
}

Statement* parse_while_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_while) && "not a while statement");

    Scope scope = scope_while(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* out = parse_while_statement_internal(parser);
    
    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return out;
}

Statement* parse_do_statement_internal(Parser* parser)
{
    Location do_loc = parser_consume(parser);

    Statement* body = parse_statement(parser, false);

    // If the token is not a while just skip till we get a semi...
    if (!parser_is(parser, TOK_while))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected 'while' in do/while loop");
        parser_recover(parser, TOK_SEMI, RECOVER_EAT);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Location while_loc = parser_consume(parser);

    Location lparen = LOCATION_INVALID;
    Expression* cond = NULL;
    Location rparen = LOCATION_INVALID;
    if (!parse_expression_for_statement(parser, do_loc, &lparen, &cond, &rparen,
            /*is_switch*/false, "do/while"))
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Location semi = parse_trailing_semi(parser, "do/while statement");
    return semantic_checker_handle_do_while_statement(&parser->sc, do_loc,
            body, while_loc, lparen, cond, rparen, semi);

}

Statement* parse_do_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_do) && "not a do statement");

    Scope scope = scope_do_while(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* out = parse_do_statement_internal(parser);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return out;
}

Statement* parse_for_statement_internal(Parser* parser)
{
    Location for_loc = parser_consume(parser);

    // Make sure we got a lparen after!
    if (!parser_is(parser, TOK_LPAREN))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected '(' after 'for'");
        parser_recover(parser, TOK_SEMI, RECOVER_EAT);
        return semantic_checker_handle_error_statement(&parser->sc);
    }


    assert(parser_is(parser, TOK_LPAREN) && "expected '('");
    Location lparen_loc = parser_consume(parser);

    DeclarationGroup init_declaration = decl_group_from_empty();
    Expression* init_expression = NULL;
    if (parser_is_typename(parser))
    {
        // Warn if we're not in C99 mode about this extension.
        if (!lang_opts_c99(parser->lang))
        {
            diagnostic_warning_at(parser->dm, parser_current_location(parser),
                    Wc99_extensions, "variable declarations in for loop is a "
                    "C99-specific feature");
        }
        init_declaration = parse_declaration(parser, CONTEXT_BLOCK, NULL);
    }
    else if (parser_is_expression(parser))
    {
        init_expression = parse_expression(parser);
    }
    else if (!parser_is(parser, TOK_SEMI))
    {
        // TODO: ensure this is adequete error recovery
        diagnostic_error_at(parser->dm, parser_current_location(parser), 
                "expected expression");
        parser_recover(parser, TOK_SEMI, RECOVER_NONE);
    }

    if (!parser_try_consume(parser, TOK_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* cond = NULL;
    if (!parser_is(parser, TOK_SEMI))
    {
        cond = parse_expression(parser);
    }

    if (!parser_try_consume(parser, TOK_SEMI, NULL))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ';' in 'for' statement specifier");
    }

    Expression* inc = NULL;
    if (!parser_is(parser, TOK_RPAREN))
    {
        inc = parse_expression(parser);
    }

    Location rparen_loc = LOCATION_INVALID;
    if (!parser_is(parser, TOK_RPAREN))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected ')'");
    }
    else
    {
        rparen_loc = parser_consume(parser);
    }

    // Finally, try to parse a valid body and create the if statement
    Statement* body = parse_statement(parser, false);

    return semantic_checker_handle_for_statement(&parser->sc, for_loc,
            lparen_loc, init_declaration, init_expression, cond, inc,
            rparen_loc, body);
}

Statement* parse_for_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_for) && "not a for statement");

    Scope scope = scope_for(&parser->ast->ast_allocator);
    semantic_checker_push_scope(&parser->sc, &scope);

    Statement* out = parse_for_statement_internal(parser);

    semantic_checker_pop_scope(&parser->sc);
    scope_delete(&scope);

    return out;
}

Statement* parse_goto_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_goto) && "not a goto statement");

    Location goto_loc = parser_consume(parser);

    // Need to have an identifier next. GCC and clang have computed gotos as an
    // extension to the language but this is not supported here.
    if (!parser_is(parser, TOK_IDENTIFIER))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected identifier after 'goto'");
        parser_recover(parser, TOK_SEMI, RECOVER_EAT);
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Get the identifier name and consume the identifier.
    Identifier* id = token_get_identifier(parser_current(parser));
    Location loc = parser_consume(parser);
    Location semi = parse_trailing_semi(parser, "goto statement");

    return semantic_checker_handle_goto_statement(&parser->sc, goto_loc, id,
            loc, semi);
}

Statement* parse_continue_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_continue) && "not a continue statement");

    Location continue_loc = parser_consume(parser);
    Location semi = parse_trailing_semi(parser, "continue statement");

    return semantic_checker_handle_continue_statement(&parser->sc, continue_loc,
            semi);
}

Statement* parse_break_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_break) && "not a break statement");

    Location break_loc = parser_consume(parser);
    Location semi = parse_trailing_semi(parser, "break statement");

    return semantic_checker_handle_break_statement(&parser->sc, break_loc,
            semi);
}

Statement* parse_return_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_return) && "not a return statement");

    Location return_loc = parser_consume(parser);

    Expression* expr = NULL;
    if (!parser_is(parser, TOK_SEMI))
    {
        expr = parse_expression(parser);
    }
    Location semi = parse_trailing_semi(parser, "return statement");

    return semantic_checker_handle_return_statement(&parser->sc, return_loc,
            expr, semi);
}

Statement* parse_empty_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_SEMI) && "not a empty statement");

    Location semi = parser_consume(parser);

    return semantic_checker_handle_empty_statement(&parser->sc, semi);
}

Statement* parse_label_statement(Parser* parser)
{
    assert(parser_is(parser, TOK_IDENTIFIER)
            && parser_is_next(parser, TOK_COLON) && "not a label");

    Identifier* id = token_get_identifier(parser_current(parser));
    Location loc = parser_consume(parser);
    Location colon = parser_consume(parser);

    // Handle getting of the label and abort parsing on error
    Declaration* label = semantic_checker_act_on_label(&parser->sc, id, loc);
    if (label == NULL)
    {
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    Statement* body = parse_statement_after_label(parser, "label");
    
    return semantic_checker_handle_label_statement(&parser->sc, loc, loc, label,
            body);
}

Statement* parse_declaration_statement(Parser* parser)
{   
    Location semi = LOCATION_INVALID;
    DeclarationGroup group = parse_declaration(parser, CONTEXT_BLOCK, &semi);

    return semantic_checker_handle_declaration_statement(&parser->sc, group,
            semi);
}

Statement* parse_expression_statement(Parser* parser)
{
    // NOTE: this can be achieved through being called to parse a statement and
    // mathing nothing, when we should be matching the end of a compound stmt
    if (parser_is(parser, TOK_RCURLY))
    {   
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected statement");
        return semantic_checker_handle_error_statement(&parser->sc);
    }

    // Here we could be trying to parse the body of a for, if, switch, etc and
    // in C, declarations are not allowed there. So we must check we are 
    // actually an expression.
    if (!parser_is_expression(parser))
    {
        diagnostic_error_at(parser->dm, parser_current_location(parser),
                "expected expression");
        parser_recover_two(parser, TOK_RCURLY, TOK_SEMI, RECOVER_NONE);
        return semantic_checker_handle_error_statement(&parser->sc);
    }
    assert(parser_is_expression(parser) && "not an expression?");

    Expression* expr = parse_expression(parser);

    // Finally, check for extra ')' after certain expressions.
    if (expression_is(expr, EXPRESSION_PARENTHESISED)
            && parser_is(parser, TOK_RPAREN)
            && parser_is_next(parser, TOK_SEMI))
    {
        diagnostic_error_at(parser->dm, parser_consume(parser),
                "extraneous ')' before ';'");
    }

    Location semi = parse_trailing_semi(parser, "expression");
    return semantic_checker_handle_expression_statement(&parser->sc,
            expr, semi);
}

Statement* parse_statement_internal(Parser* parser, bool decls)
{
    switch (parser_current_type(parser))
    {
        case TOK_LCURLY:
            return parse_compound_statement(parser);

        case TOK_case:
            return parse_case_statement(parser);

        case TOK_default:
            return parse_default_statement(parser);

        case TOK_if:
            return parse_if_statement(parser);

        case TOK_switch:
            return parse_switch_statement(parser);

        case TOK_while:
            return parse_while_statement(parser);

        case TOK_do:
            return parse_do_statement(parser);

        case TOK_for:
            return parse_for_statement(parser);

        case TOK_goto:
            return parse_goto_statement(parser);

        case TOK_continue:
            return parse_continue_statement(parser);

        case TOK_break:
            return parse_break_statement(parser);

        case TOK_return:
            return parse_return_statement(parser);

        case TOK_SEMI:
            return parse_empty_statement(parser);

        case TOK_IDENTIFIER:
            if (parser_is_next(parser, TOK_COLON))
            {
                return parse_label_statement(parser);
            }
            
            /* FALLTHROUGH */

        default:
            if (decls && parser_is_typename(parser))
            {
                return parse_declaration_statement(parser);
            }
            return parse_expression_statement(parser);
    }
}

Statement* parse_statement(Parser* parser, bool decls)
{
    assert(parser_is_statement(parser) && "not a statement");

    // TODO: why do I have this? Possibly for attributes?
    return parse_statement_internal(parser, decls);
}

Statement* parse_function_body(Parser* parser)
{
    assert(parser_is(parser, TOK_LCURLY) && "not a function body?");

    // FIXME: Scope...
    // Scope scope = scope_block(AstAllocator *allocator)
    
    Statement* stmt = parse_compound_statement_internal(parser);

    return stmt;
}
