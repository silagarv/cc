#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "driver/diagnostic.h"

#include "files/location.h"
#include "lex/identifier_table.h"
#include "parse/expression.h"
#include "parse/statement.h"
#include "parse/declaration.h"
#include "parse/ast.h"
#include "parse/type.h"
#include "parse/scope.h"

typedef struct SemanticChecker {
    // The diagnostic manager for this semantic checker
    DiagnosticManager* dm;

    // The ast so that we can get the builtin types and all of the current types
    Ast* ast;

    // The current scope in the semantic checker. This is not allocated at all
    // but needs popping after each time it is used.
    Scope* scope;

    // The current function scope that we are using
    FunctionScope* function;
} SemanticChecker;

SemanticChecker sematic_checker_create(DiagnosticManager* dm, Ast* ast);

void declaration_specifiers_finish(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers);

QualifiedType qualified_type_from_declaration_specifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers);

// TODO: this will need some context e.g. if static is allowed etc.
Declaration* semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator);

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

// This is all of the logic for our label handling code below.
void sematic_checker_push_function_scope(SemanticChecker* sc,
        FunctionScope* function);
void sematic_checker_pop_function_scope(SemanticChecker* sc);

Declaration* semantic_checker_act_on_label(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location);
Declaration* semantic_checker_act_on_goto(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location);
void sematic_checker_act_on_end_of_function(SemanticChecker* sc);

// Functions for type checking expressions, declarations, and statements
Expression* typecheck_expression(Ast* ast, Expression* expression);
Declaration* typecheck_declaration(Ast* ast, Declaration* expression);
Statement* typecheck_statement(Ast* ast, Statement* expression);

#endif /* SEMANTIC_H */
