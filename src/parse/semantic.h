#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "driver/diagnostic.h"

#include "files/location.h"
#include "lex/identifier_table.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/literal_parser.h"
#include "parse/statement.h"
#include "parse/declaration.h"
#include "parse/ast.h"
#include "parse/type.h"
#include "parse/scope.h"

typedef struct SemanticChecker {
    // The diagnostic manager for this semantic checker
    DiagnosticManager* dm;

    // The identifier table store in the preprocessor so that we can create
    // anonymous types if needed
    IdentifierTable* identifiers;

    // The ast so that we can get the builtin types and all of the current types
    Ast* ast;

    // The current external scope.
    Scope* externals;

    // The current scope in the semantic checker. This is not allocated at all
    // but needs popping after each time it is used.
    Scope* scope;

    // The current function scope that we are using
    FunctionScope* function;
} SemanticChecker;

SemanticChecker sematic_checker_create(DiagnosticManager* dm,
        IdentifierTable* identifiers, Ast* ast);

void declaration_specifiers_finish(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers);
QualifiedType qualified_type_from_declaration_specifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers);

// These are our functions for handling the scopes that the semantic checker
// can see.
void semantic_checker_push_externals(SemanticChecker* sc, Scope* scope);
void semantic_checker_pop_externals(SemanticChecker* sc);
void semantic_checker_push_scope(SemanticChecker* sc, Scope* scope);
void semantic_checker_pop_scope(SemanticChecker* sc);
Scope* semantic_checker_current_scope(SemanticChecker* sc);
bool semantic_checker_current_scope_is(SemanticChecker* sc, ScopeFlags type);

// A very important helper functions for handling if an identifier token should
// be treated as a typename or not. Returns 'true' if it should and 'false' 
// otherwise
bool semantic_checker_identifier_is_typename(SemanticChecker* sc,
        Identifier* identifier);
Declaration* semantic_checker_get_typename(SemanticChecker* sc,
        Identifier* identifier);

// All of the few functions below are for processing different thing in 
// different situations.
// semantic_checker_process_specifiers -> specifiers only no declarations
// semantic_checker_process_typename -> for casts
// semantic_checker_process_function_param -> function parameters
// semantic_checker_process_struct_declarator -> struct members
// semantic_checker_process_declarator -> general catch all function
Declaration* semantic_checker_process_specifiers(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers);
QualifiedType semantic_checker_process_typename(SemanticChecker* sc,
        Declarator* declarator);
Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator);
Declaration* semantic_checker_process_struct_declarator(SemanticChecker* sc,
        Declaration* struct_decl, Declarator* declarator);
Declaration* semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator);

// Semantic checker functions for tags and enum constants. Note I eventually 
// want to remove all of these functions here and have them happen internally.
Declaration* semantic_checker_handle_enum_constant(SemanticChecker* sc,
        Location location, Identifier* identifier, Location equals,
        Expression* expression, Declaration* last_decl);
Declaration* semantic_checker_handle_tag(SemanticChecker* sc,
        DeclarationType type, Location tag_type_loc, Identifier* identifier,
        Location identifier_location, bool is_definition);


// For post declarator shenanigans
void semantic_checker_handle_function_start(SemanticChecker* sc,
        Declaration* function);
void semantic_checker_handle_function_end(SemanticChecker* sc,
        Declaration* function, Statement* body);
void semantic_checker_add_function_parameters(SemanticChecker* sc,
        Declaration* declaration);

void semantic_checker_declaration_add_initializer(SemanticChecker* sc,
        Declaration* declaration, DeclaratorContext context, Location equals,
        Initializer* initializer);
void semantic_checker_declaration_finish(SemanticChecker* sc,
        Declaration* declaration);
void semantic_checker_check_externals(SemanticChecker* sc);

void semantic_checker_finish_struct_declaration(SemanticChecker* sc,
        Declaration* struct_declaration);

// This is all of the logic for our label handling code below. The first two
// functions are for pushing a function scope (for labels) and for popping it.
// And the functions below are for allowing the creation of implicit labels (if
// first seen on a goto) and for finishing the creation of labels when that
// occurs.

void sematic_checker_push_function_scope(SemanticChecker* sc,
        FunctionScope* function);
void sematic_checker_pop_function_scope(SemanticChecker* sc);

Declaration* semantic_checker_act_on_label(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location);
Declaration* semantic_checker_act_on_goto(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location);
void sematic_checker_act_on_end_of_function(SemanticChecker* sc);

// Semantic checking functions for our expression types

Expression* semantic_checker_handle_error_expression(SemanticChecker* sc,
        Location location);

Expression* semantic_checker_handle_parenthesis_expression(SemanticChecker* sc,
        Location lparen_location, Expression* inner, Location rparen_location);
Expression* semantic_checker_handle_builtin_identifier(SemanticChecker* sc,
        Location location);
Expression* semantic_checker_handle_reference_expression(SemanticChecker* sc,
        Location identifier_location, Identifier* identifier,
        bool is_function_call);
Expression* semantic_checker_handle_number_expression(SemanticChecker* sc,
        Location number_location, LiteralValue value, bool success);
Expression* semantic_checker_handle_char_expression(SemanticChecker* sc,
        Location char_location, CharValue value, bool success);

Expression* semantic_checker_handle_array_expression(SemanticChecker* sc,
        Expression* lhs, Location lbracket_loc, Expression* member,
        Location rbracket_loc);
Expression* semantic_checker_handle_call_expression(SemanticChecker* sc,
        Expression* lhs, Location lparen_location, Expression* expr_list,
        Location rparen_location);

Expression* semantic_checker_handle_member_expression(SemanticChecker* sc,
        Expression* lhs, Location operator_loc, Identifier* identifier,
        Location identifier_location, bool dot);

Expression* semantic_checker_handle_increment_expression(SemanticChecker* sc,
        ExpressionType type, Expression* expression, Location operator_loc);
Expression* semantic_checker_handle_unary_expression(SemanticChecker* sc,
        ExpressionType type, Location operator_loc, Expression* rhs);
Expression* semantic_checker_handle_compound_literal(SemanticChecker* sc,
        Location lparen_loc, QualifiedType type, Location rparen_loc,
        Initializer* initializer);
Expression* semantic_checker_handle_address_expression(SemanticChecker* sc,
        Expression* rhs, Location and_location);
Expression* semantic_checker_handle_dereference_expression(SemanticChecker* sc,
        Expression* rhs, Location star_location);
Expression* semantic_checker_handle_sizeof_type_expression(SemanticChecker* sc,
        Location sizeof_location, Location lparen_loc, QualifiedType type,
        Location rparen_loc);
Expression* semantic_checker_handle_sizeof_expression(SemanticChecker* sc,
        Location sizeof_location, Expression* expression);

Expression* semantic_checker_handle_cast_expression(SemanticChecker* sc,
        Location lparen_loc, QualifiedType type, Location rparen_loc,
        Expression* rhs);

Expression* semantic_checker_handle_arithmetic_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs);

Expression* semantic_checker_handle_conditional_expression(SemanticChecker* sc,
        Expression* condition, Location question, Expression* true_expr,
        Location colon, Expression* false_expr);

Expression* semantic_checker_handle_assignment_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs);

Expression* semantic_checker_handle_comma_expression(SemanticChecker* sc,
        Expression* lhs, Location comma_location, Expression* rhs);

Expression* semantic_checker_expression_finalize(SemanticChecker* sc,
        Expression* expression);

// Below are functions for the semantic checker to try to convert and expression
// into an initializer. We need to give some state into the semantic checker so
// that we can accurately create the initializer.

Initializer* semantic_checker_initializer_from_expression(SemanticChecker* sc,
        Expression* expression);
Initializer* semantic_checker_initializer_from_list(SemanticChecker* sc,
        Location lcurly, InitializerListMember* initializer, Location rcurly);

bool semantic_checker_declaration_check_initializer(SemanticChecker* sc,
        Declaration* declaration, DeclaratorContext context, Initializer* init);

// Semantic checking functions for all of the statement types that we will exist
// in C. These are for the most part very simple functions which do a few basic
// checks but will just create a nice statement for us...

Expression* semantic_checker_check_condition(SemanticChecker* sc,
        Location kw_location, Expression* expression, bool is_switch,
        const char* context);

Statement* semantic_checker_handle_compound_statement(SemanticChecker* sc,
        Location lcurly, Statement* first, Location rcurly);
bool semantic_checker_check_case_allowed(SemanticChecker* sc,
        Location case_location);
Statement* semantic_checker_handle_case_statement(SemanticChecker* sc,
        Location case_location, Expression* expression,
        Location colon_location, Statement* stmt);
bool semantic_checker_check_default_allowed(SemanticChecker* sc,
        Location default_location);
Statement* semantic_checker_handle_default_statement(SemanticChecker* sc,
        Location default_location, Location colon_location, Statement* stmt);
Statement* semantic_checker_handle_if_statement(SemanticChecker* sc,
        Location if_locatoin, Location lparen_location, Expression* expression,
        Location rparen_location, Statement* if_body, Location else_location,
        Statement* else_body);
Statement* semantic_checker_handle_switch_statement(SemanticChecker* sc,
        Location switch_location, Location lparen_location,
        Expression* expression, Location rparen_location,
        Statement* body);
Statement* semantic_checker_handle_while_statement(SemanticChecker* sc,
        Location while_location, Location lparen_location,
        Expression* expression, Location rparen_location, Statement* stmt);
Statement* semantic_checker_handle_do_while_statement(SemanticChecker* sc,
        Location do_location, Statement* body, Location while_location,
        Location lparen_location, Expression* expression,
        Location rparen_location, Location semi_location);
Statement* semantic_checker_handle_for_statement(SemanticChecker* sc,
        Location for_location, Location lparen_location,
        Declaration* init_declaration, Expression* init_expression,
        Expression* condition, Expression* increment, Location rparen_location,
        Statement* body);
Statement* semantic_checker_handle_goto_statement(SemanticChecker* sc,
        Location goto_location, Identifier* identifier,
        Location identifier_location, Location semi_location);
Statement* semantic_checker_handle_continue_statement(SemanticChecker* sc,
        Location continue_location, Location semi_location);
Statement* semantic_checker_handle_break_statement(SemanticChecker* sc,
        Location break_location, Location semi_location);
Statement* semantic_checker_handle_return_statement(SemanticChecker* sc,
        Location return_location, Expression* expression,
        Location semi_location);
Statement* semantic_checker_handle_empty_statement(SemanticChecker* sc,
        Location semi_location);
Statement* semantic_checker_handle_label_statement(SemanticChecker* sc,
        Location identifier_location, Location colon_location,
        Declaration* label_declaration, Statement* statement);
Statement* semantic_checker_handle_declaration_statement(SemanticChecker* sc,
        Declaration* declaration, Location semi_location);
Statement* semantic_checker_handle_expression_statement(SemanticChecker* sc,
        Expression* expression, Location semi_location);
Statement* semantic_checker_handle_error_statement(SemanticChecker* sc);

#endif /* SEMANTIC_H */
