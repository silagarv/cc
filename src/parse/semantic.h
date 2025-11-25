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

// These are our functions for handling all of our scopes
void semantic_checker_push_scope(SemanticChecker* sc, Scope* scope);
void semantic_checker_pop_scope(SemanticChecker* sc);
Scope* semantic_checker_current_scope(SemanticChecker* sc);
Declaration* semantic_checker_lookup_ordinairy(SemanticChecker* sc,
        Identifier* identifier, bool recursive);
Declaration* semantic_checker_lookup_tag(SemanticChecker* sc,
        Identifier* identifier, bool recursive);
Declaration* semantic_checker_lookup_member(SemanticChecker* sc,
        Identifier* identifier);

void semantic_checker_add_function_parameters(SemanticChecker* sc,
        Declaration* declaration);

// Create the enumeration declaration 
Declaration* semantic_checker_create_enum(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous);
Declaration* semantic_checker_create_enum_constant(SemanticChecker* sc,
        Location location, Identifier* identifier, Location equals,
        Expression* expression, Declaration* last_decl);

Declaration* semantic_checker_create_struct(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous);
Declaration* semantic_checker_create_union(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous);

Declaration* semantic_checker_handle_tag(SemanticChecker* sc,
        DeclarationType type, Location tag_type_loc, Identifier* identifier,
        Location identifier_location, bool is_definition);

// TODO: this will need some context e.g. if static is allowed etc.
Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator);

QualifiedType semantic_checker_process_typename(SemanticChecker* sc,
        Declarator* declarator);
Declaration* semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator);
Declaration* semantic_checker_process_struct_declarator(SemanticChecker* sc,
        Declarator* declarator, Location colon_location,
        Expression* expression);
void semantic_checker_declaration_add_initializer(SemanticChecker* sc,
        Declaration* declaration, Location equals, Initializer* initializer);

// This is all of the logic for our label handling code below.
void sematic_checker_push_function_scope(SemanticChecker* sc,
        FunctionScope* function);
void sematic_checker_pop_function_scope(SemanticChecker* sc);

// Semantic checking actions for labels and that kind of thing
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
// TODO: semantic_checker_handle_function_call_expression(...);
// TODO: semantic_checker_handle_member_expression(...);
Expression* semantic_checker_handle_increment_expression(SemanticChecker* sc,
        ExpressionType type, Expression* expression, Location operator_loc);


// Semantic checking functions for the rest of our statement types

// TODO
Statement* semantic_checker_handle_compound_statement(SemanticChecker* sc);

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
