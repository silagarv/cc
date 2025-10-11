#include "semantic.h"

#include <assert.h>

#include "parse/statement.h"
#include "parse/type.h"
#include "parse/declaration.h"

SemanticChecker sematic_checker_create(DiagnosticManager* dm, Ast* ast)
{
    SemanticChecker sc = (SemanticChecker)
    {
        .dm = dm,
        .ast = ast
    };

    return sc;
}

void declaration_specifiers_finish(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers)
{
    // Now we want to check the signedness is valid. signed or unsigned can only
    // be specified when the type is int.
    switch (specifiers->type_spec_sign)
    {
        case TYPE_SPECIFIER_SIGN_NONE:
            break;

        case TYPE_SPECIFIER_SIGN_SIGNED:
        case TYPE_SPECIFIER_SIGN_UNSIGNED:
            // If it isn't specified we set it to be int. Otherwise make sure
            // that we only have an int type. If we get signed or unsigned then
            // we disregard that in favour of the other
            if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_INT)
            {
                diagnostic_error_at(sc->dm, specifiers->location,
                        "'%s' cannot be signed or unsigned",
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_sign = TYPE_SPECIFIER_SIGN_NONE;
            }
            break;
    }

    // Fix up some things with the width and type.
    switch (specifiers->type_spec_width)
    {
        case TYPE_SPECIFIER_WIDTH_NONE:
            break;

        // Here we can only have 'short int' or 'long long int'
        case TYPE_SPECIFIER_WIDTH_SHORT:
        case TYPE_SPECIFIER_WIDTH_LONG_LONG:
            // If 'int' was ommited set it, otherwise error about how it is
            // invalid. And set it to be int. 
            if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_INT)
            {
                diagnostic_error_at(sc->dm, specifiers->location, 
                        "'%s %s' is invalid",
                        width_specifier_to_name(specifiers->type_spec_width),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            }
            break;

        // Here we can have 'long int' or 'long double'
        case TYPE_SPECIFIER_WIDTH_LONG:
            // If 'int' was ommited set it, otherwise error about how it is
            // invalid. And set it to be int. But only if its also not double
            if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_INT &&
                    specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_DOUBLE)
            {
                diagnostic_error_at(sc->dm, specifiers->location, 
                        "'%s %s' is invalid",
                        width_specifier_to_name(specifiers->type_spec_width),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            }
            break;
    }

    switch (specifiers->type_spec_complex)
    {
        case TYPE_SPECIFIER_COMPLEX_NONE:
            break;

        case TYPE_SPECIFIER_COMPLEX_COMPLEX:
        case TYPE_SPECIFIER_COMPLEX_IMAGINAIRY:
            if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_NONE)
            {
                diagnostic_warning_at(sc->dm, specifiers->location,
                        "'%s' requires type specifier; assuming 'double'",
                        complex_specifier_to_name(
                        specifiers->type_spec_complex));
                specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_DOUBLE;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_FLOAT &&
                    specifiers->type_spec_type != TYPE_SPECIFIER_TYPE_DOUBLE)
            {
                diagnostic_error_at(sc->dm, specifiers->location,
                        "'%s %s' is invalid",
                        complex_specifier_to_name(
                        specifiers->type_spec_complex),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_complex = TYPE_SPECIFIER_COMPLEX_NONE;
            }
            break;
    }

    // Now check that we have actually recieved a type during parsing
    switch (specifiers->type_spec_type)
    {
        case TYPE_SPECIFIER_TYPE_NONE:
            diagnostic_error_at(sc->dm, specifiers->location,
                    "type specifier missing; defaults to 'int'");
            specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
            break;

        default:
            break;
    }

    // Now that we believe that our type is valid we can determine the type that
    // this set of declaration specifiers should have.
}

static QualifiedType add_type_qualifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers, Type* type)
{
    TypeQualifiers qualifiers = specifiers->qualifiers;
    
    if (type_qualifier_is_restrict(qualifiers))
    {
        // TODO: check if we have a pointer type?
    }

    if (type_qualifier_is_const(qualifiers))
        ; // TODO;

    if (type_qualifier_is_volatile(qualifiers))
        ; // TODO;

    // TODO: function types?    

    return (QualifiedType) {.type = type, .qualifiers = qualifiers};
}

QualifiedType qualified_type_from_declaration_specifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers)
{
    const TypeBuiltins* builtins = &sc->ast->base_types;
    Type* type = NULL;

    switch (specifiers->type_spec_type)
    {
        case TYPE_SPECIFIER_TYPE_VOID:
            type = builtins->type_void;
            break;

        case TYPE_SPECIFIER_TYPE_CHAR:
            if (specifiers->type_spec_sign == TYPE_SPECIFIER_SIGN_NONE)
            {
                type = builtins->type_char;
            }
            else if (specifiers->type_spec_sign == TYPE_SPECIFIER_SIGN_SIGNED)
            {
                type = builtins->type_signed_char;
            }
            else
            {
                type = builtins->type_unsigned_char;
            }
            break;

        case TYPE_SPECIFIER_TYPE_INT:
            // If were not unsigned then we always go to being signed.
            if (specifiers->type_spec_sign != TYPE_SPECIFIER_SIGN_UNSIGNED)
            {
                switch (specifiers->type_spec_width)
                {
                    case TYPE_SPECIFIER_WIDTH_NONE:
                        type = builtins->type_signed_int;
                        break;

                    case TYPE_SPECIFIER_WIDTH_SHORT:
                        type = builtins->type_signed_short;
                        break;

                    case TYPE_SPECIFIER_WIDTH_LONG:
                        type = builtins->type_signed_long;
                        break;

                    case TYPE_SPECIFIER_WIDTH_LONG_LONG:
                        type = builtins->type_signed_long_long;
                        break;
                }
            }
            else
            {
                switch (specifiers->type_spec_width)
                {
                    case TYPE_SPECIFIER_WIDTH_NONE:
                        type = builtins->type_unsigned_int;
                        break;

                    case TYPE_SPECIFIER_WIDTH_SHORT:
                        type = builtins->type_unsigned_short;
                        break;

                    case TYPE_SPECIFIER_WIDTH_LONG:
                        type = builtins->type_unsigned_long;
                        break;

                    case TYPE_SPECIFIER_WIDTH_LONG_LONG:
                        type = builtins->type_unsigned_long_long;
                        break;
                }
            }
            break;

        case TYPE_SPECIFIER_TYPE_FLOAT:
            type = builtins->type_float;
            break;

        case TYPE_SPECIFIER_TYPE_DOUBLE:
            if (specifiers->type_spec_width == TYPE_SPECIFIER_WIDTH_LONG)
            {
                type = builtins->type_long_double;
            }
            else
            {
                type = builtins->type_double;
            }
            break;

        case TYPE_SPECIFIER_TYPE_BOOL:
            type = builtins->type_bool;
            break;

        case TYPE_SPECIFIER_TYPE_ENUM:
            panic("unimplemented -> enum type should be in declaration");
            break;

        case TYPE_SPECIFIER_TYPE_STRUCT:
            panic("unimplemented -> struct type should be in declaration");
            break;

        case TYPE_SPECIFIER_TYPE_UNION:
            panic("unimplemented -> union type should be in declaration");
            break;

        case TYPE_SPECIFIER_TYPE_TYPENAME:
            panic("unimplemented -> typename should be in specifiers");
            break;        

        default:
            panic("unknown type");                
    }

    assert(type != NULL);

    // TODO: support complex and imaginary types...
    
    return add_type_qualifiers(sc, specifiers, type);
}

static QualifiedType semantic_checker_process_array_declarator(
        SemanticChecker* sc, DeclaratorPiece piece);

QualifiedType semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator)
{
    return (QualifiedType) {0};
}


Expression* typecheck_expression(Ast* ast, Expression* expression);
Declaration* typecheck_declaration(Ast* ast, Declaration* expression);
Statement* typecheck_statement(Ast* ast, Statement* expression);
