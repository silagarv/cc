#ifndef SEMANTIC_H
#define SEMANTIC_H

#include "driver/diagnostic.h"

#include "parse/expression.h"
#include "parse/statement.h"
#include "parse/declaration.h"
#include "parse/ast.h"
#include "parse/type.h"

typedef struct SemanticChecker {
    // The diagnostic manager for this semantic checker
    DiagnosticManager* dm;

    // The ast so that we can get the builtin types and all of the current types
    Ast* ast;
} SemanticChecker;

SemanticChecker sematic_checker_create(DiagnosticManager* dm, Ast* ast);

void declaration_specifiers_finish(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers);

QualifiedType qualified_type_from_declaration_specifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers);

QualifiedType semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator);

// Functions for type checking expressions, declarations, and statements
Expression* typecheck_expression(Ast* ast, Expression* expression);
Declaration* typecheck_declaration(Ast* ast, Declaration* expression);
Statement* typecheck_statement(Ast* ast, Statement* expression);

#endif /* SEMANTIC_H */
