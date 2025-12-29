#include "semantic.h"

#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "util/panic.h"

#include "files/location.h"

#include "driver/diagnostic.h"

#include "lex/identifier_table.h"

#include "parse/scope.h"
#include "parse/statement.h"
#include "parse/type.h"
#include "parse/declaration.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/literal_parser.h"

SemanticChecker sematic_checker_create(DiagnosticManager* dm, 
        IdentifierTable* identifiers, Ast* ast)
{
    SemanticChecker sc = (SemanticChecker)
    {
        .dm = dm,
        .identifiers = identifiers,
        .ast = ast,
        .scope = NULL,
        .function = NULL
    };

    return sc;
}

// -----------------------------------------------------------------------------
// Start functions for handling scopes
// -----------------------------------------------------------------------------

void semantic_checker_push_externals(SemanticChecker* sc, Scope* scope)
{
    sc->externals = scope;
}

void semantic_checker_pop_externals(SemanticChecker* sc)
{
    sc->externals = NULL;
}

void semantic_checker_push_scope(SemanticChecker* sc, Scope* scope)
{
    // Set the scopes parent
    scope_set_parent(scope, sc->scope);

    // Set the sematic checkers scope.
    sc->scope = scope;
}

void semantic_checker_pop_scope(SemanticChecker* sc)
{
    sc->scope = scope_get_parent(sc->scope);
}

Scope* semantic_checker_current_scope(SemanticChecker* sc)
{
    return sc->scope;
}

Declaration* semantic_checker_lookup_ordinairy(SemanticChecker* sc,
        Identifier* identifier, bool recursive)
{
    if (identifier == NULL)
    {
        return NULL;
    }

    return scope_lookup_ordinairy(sc->scope, identifier, recursive);
}

Declaration* semantic_checker_lookup_tag(SemanticChecker* sc,
        Identifier* identifier, bool recursive)
{
    if (identifier == NULL)
    {
        return NULL;
    }

    return scope_lookup_tag(sc->scope, identifier, recursive);
}

Declaration* semantic_checker_lookup_member(SemanticChecker* sc,
        Identifier* identifier)
{
    assert(scope_is(sc->scope, SCOPE_MEMBER) && "must be a member scope");

    return scope_lookup_member(sc->scope, identifier);
}

void semantic_checker_insert_ordinairy(SemanticChecker* sc,
        Declaration* declaration)
{
    scope_insert_ordinairy(sc->scope, declaration);
}

void semantic_checker_insert_tag(SemanticChecker* sc, Declaration* decl)
{
    // Any new tag declarations will not be visible outside of function scope
    // so producte a warning about this.
    if (scope_is(sc->scope, SCOPE_FUNCTION))
    {
        diagnostic_warning_at(sc->dm, decl->base.location,
                "declaration of '%s %s' will not be visible outside of this "
                "function", tag_kind_to_name(decl->base.declaration_type),
                decl->base.identifier->string.ptr);
    }

    scope_insert_tag(sc->scope, decl);
}

void semantic_checker_insert_member(SemanticChecker* sc, Declaration* decl)
{
    assert(scope_is(sc->scope, SCOPE_MEMBER) && "must be a member scope");
    assert(declaration_is(decl, DECLARATION_FIELD));

    scope_insert_member(sc->scope, decl);
}

Declaration* semantic_checker_lookup_external(SemanticChecker* sc,
        Identifier* identifier)
{
    assert(sc->externals != NULL);

    return scope_lookup_ordinairy(sc->externals, identifier, false);
}

void semantic_checker_insert_external(SemanticChecker* sc, Declaration* decl)
{
    assert(declaration_is(decl, DECLARATION_FUNCTION)
            || declaration_is(decl, DECLARATION_VARIABLE));
    assert(sc->externals != NULL);

    scope_insert_ordinairy(sc->externals, decl);
}

// -----------------------------------------------------------------------------
// End functions for handling scopes
// -----------------------------------------------------------------------------
// =============================================================================
// -----------------------------------------------------------------------------
// Start functions for handling declarations
// -----------------------------------------------------------------------------

static QualifiedType semantic_checker_get_float_type(SemanticChecker* sc)
{
    Type* flt = sc->ast->base_types.type_float;
    return (QualifiedType) {QUALIFIER_NONE, flt};
}

static QualifiedType semantic_checker_get_double_type(SemanticChecker* sc)
{
    Type* dbl = sc->ast->base_types.type_double;
    return (QualifiedType) {QUALIFIER_NONE, dbl};
}

static QualifiedType semantic_checker_get_long_double_type(SemanticChecker* sc)
{
    Type* long_double = sc->ast->base_types.type_long_double;
    return (QualifiedType) {QUALIFIER_NONE, long_double};
}

static QualifiedType semantic_checker_get_int_type(SemanticChecker* sc)
{
    Type* int_type = sc->ast->base_types.type_signed_int;
    return (QualifiedType) {QUALIFIER_NONE, int_type};
}

static QualifiedType semantic_checker_get_uint_type(SemanticChecker* sc)
{
    Type* uint_type = sc->ast->base_types.type_unsigned_int;
    return (QualifiedType) {QUALIFIER_NONE, uint_type};
}

static QualifiedType semantic_checker_get_long_type(SemanticChecker* sc)
{
    Type* long_type = sc->ast->base_types.type_signed_long;
    return (QualifiedType) {QUALIFIER_NONE, long_type};
}

static QualifiedType semantic_checker_get_ulong_type(SemanticChecker* sc)
{
    Type* ulong_type = sc->ast->base_types.type_unsigned_long;
    return (QualifiedType) {QUALIFIER_NONE, ulong_type};
}

static QualifiedType semantic_checker_get_ulong_long_type(SemanticChecker* sc)
{
    Type* ulong_long_type = sc->ast->base_types.type_unsigned_long_long;
    return (QualifiedType) {QUALIFIER_NONE, ulong_long_type};
}

static QualifiedType semantic_checker_get_void_type(SemanticChecker* sc)
{
    Type* void_type = sc->ast->base_types.type_void;
    return (QualifiedType) {QUALIFIER_NONE, void_type};
}

static QualifiedType semantic_checker_get_size_type(SemanticChecker* sc)
{
    return sc->ast->size_type;
}

static QualifiedType semantic_checker_create_array(SemanticChecker* sc,
        QualifiedType element_type, size_t length, bool is_static, bool is_star,
        bool is_vla)
{
    return type_create_array(&sc->ast->ast_allocator, element_type, length,
            is_static, is_star, is_vla);
}

static QualifiedType semantic_checker_create_pointer(SemanticChecker* sc,
        QualifiedType pointee, TypeQualifiers qualifiers)
{
    return type_create_pointer(&sc->ast->ast_allocator, pointee, qualifiers);
}

static QualifiedType semantic_checker_create_function(SemanticChecker* sc,
        QualifiedType return_type, TypeFunctionParameter* params,
        size_t num_params, bool unspecified_params, bool is_variadic)
{
    return type_create_function(&sc->ast->ast_allocator, return_type, params,
            num_params, false, is_variadic);
}

static QualifiedType semantic_checker_decay_type(SemanticChecker* sc,
        QualifiedType type, bool* decayed)
{
    // TODO: instead of us going in and manually decaying the types. I would
    // TODO: like to create a decayed type type. Which stores both the new type
    // TODO: and the type that it was originally. This is how we would implement
    // TODO: diagnostics for decayed types differing e.g. 
    // TODO:                    int arr[10] vs int arr[2]
    // TODO: occuring in different function prototypes is fine, but we might 
    // TODO: want to warn when that does occur.

    // Start by setting the fact that we haven't decayed the type
    if (decayed)
    {
        *decayed = false;
    }

    // Get the real type to see through typedefs.
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Decay function types. To do this turn the function type into a pointer to
    // the said function type
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        // Here we can use the original type and avoid desugaring since pointer
        // doesn't have to go one level lower.
        if (decayed)
        {
            *decayed = true;
        }

        return semantic_checker_create_pointer(sc, type, QUALIFIER_NONE);
    }

    // Decay array types.
    if (qualified_type_is(&real_type, TYPE_ARRAY))
    {
        // Here we will avoid having to use the real type if needed so we will
        // check if the original type is an array type. If it is then we use
        // that otherwise using the real type to create a pointer
        QualifiedType element;
        if (qualified_type_is(&type, TYPE_ARRAY))
        {
            element = type_array_get_element_type(&type);
        }
        else
        {
            element = type_array_get_element_type(&real_type);
        }

        // Set that we have decayed the array type since we are using it
        if (decayed)
        {
            *decayed = true;
        }
        return semantic_checker_create_pointer(sc, element, QUALIFIER_NONE);
    }

    // If we haven't needed to decay the type at all just return the original
    // type to the user. Since this function should primarily be used in the
    // handling of expressions and such.
    return type;
}

static QualifiedType semantic_checker_promote_integer_type(SemanticChecker* sc,
        QualifiedType* type)
{
    assert(qualified_type_is_integer(type));
    
    QualifiedType real_type = qualified_type_get_canonical(type);
    switch (qualified_type_get_kind(&real_type))
    {
        case TYPE_BOOL:
        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_U_CHAR:
        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
            return semantic_checker_get_int_type(sc);

        default: return real_type;
    }

    panic("unreachable");
    return (QualifiedType) {0};
}

static QualifiedType semantic_checker_arithmetic_conversion(SemanticChecker* sc,
        QualifiedType lhs, QualifiedType rhs)
{
    assert(qualified_type_is_arithmetic(&lhs));
    assert(qualified_type_is_arithmetic(&rhs));

    // Note: here the only reason we can safely discard the qualifiers is since
    // arithmetic conversions also nescessitates an lvalue to rvalue conversion
    // so we would have removed qualifiers anyways
    lhs = qualified_type_get_canonical(&lhs);
    lhs = qualified_type_remove_quals(&lhs);

    rhs = qualified_type_get_canonical(&rhs);
    rhs = qualified_type_remove_quals(&rhs);

    // First do conversions if one is long double double or float
    if (qualified_type_is(&lhs, TYPE_LONG_DOUBLE)
            || qualified_type_is(&rhs, TYPE_LONG_DOUBLE))
    {
        return semantic_checker_get_long_double_type(sc);
    }
    else if (qualified_type_is(&lhs, TYPE_DOUBLE)
            || qualified_type_is(&rhs, TYPE_DOUBLE))
    {
        return semantic_checker_get_double_type(sc);
    }
    else if (qualified_type_is(&lhs, TYPE_FLOAT)
            || qualified_type_is(&rhs, TYPE_FLOAT))
    {
        return semantic_checker_get_float_type(sc);
    }

    // If we're arithmetic but not a floating type well then we must be integral
    assert(qualified_type_is_integer(&lhs));
    assert(qualified_type_is_integer(&rhs));

    // First promote our integer types.
    lhs = semantic_checker_promote_integer_type(sc, &lhs);
    rhs = semantic_checker_promote_integer_type(sc, &rhs);

    // We want to make sure any qualifiers are removed from it for us to do our
    // lvalue to rvalue conversion.
    assert(qualified_type_get_quals(&lhs) == QUALIFIER_NONE);
    assert(qualified_type_get_quals(&rhs) == QUALIFIER_NONE);

    // If the promoted types are equal than that is our type to use
    if (qualified_type_builtin_equal(&lhs, &rhs))
    {
        return lhs;
    }

    // Get both the signedness and the rank of the integers.
    bool lhs_signed = qualified_type_is_signed(&lhs);
    bool rhs_signed = qualified_type_is_signed(&rhs);
    size_t lhs_rank = qualified_type_get_rank(&lhs);
    size_t rhs_rank = qualified_type_get_rank(&rhs);

    // If they have the same signedness choose based on rank
    if (lhs_signed == rhs_signed)
    {
        return lhs_rank > rhs_rank ? lhs : rhs;
    }

    // Otherwise if they differ in signedness

    // If rhs is unsigned and its rank is bigger than lhs, convert lhs to rhs
    size_t signed_rank;
    size_t unsigned_rank;
    QualifiedType signed_type;
    QualifiedType unsigned_type;
    if (lhs_signed)
    {
        signed_rank = lhs_rank;
        signed_type = lhs;
        unsigned_rank = rhs_rank;
        unsigned_type = rhs;
    }
    else
    {
        signed_rank = rhs_rank;
        signed_type = rhs;
        unsigned_rank = lhs_rank;
        unsigned_type = lhs;
    }

    if (unsigned_rank >= signed_rank)
    {
        return unsigned_type;
    }

    // If the signed can fit all of the values of the unsigned size.
    size_t signed_size = qualified_type_get_size(&signed_type);
    size_t unsigned_size = qualified_type_get_size(&unsigned_type);
    if (signed_size > unsigned_size)
    {
        return signed_type;
    }
    
    // Else, both operands undergo implicit conversion to the unsigned type
    // counterpart of the signed operand's type. Note: here we must be at least
    // int type
    switch (qualified_type_get_kind(&signed_type))
    {
        case TYPE_S_INT:
            return semantic_checker_get_uint_type(sc);

        case TYPE_S_LONG:
            return semantic_checker_get_ulong_type(sc);

        case TYPE_S_LONG_LONG:
            return semantic_checker_get_ulong_long_type(sc);
        
        default: panic("unreachable"); break;
    }

    return (QualifiedType) {0};
}

bool semantic_checker_identifier_is_typename(SemanticChecker* sc,
        Identifier* identifier)
{
    Declaration* decl = semantic_checker_lookup_ordinairy(sc, identifier, true);
    return declaration_is(decl, DECLARATION_TYPEDEF);
}

Declaration* semantic_checker_get_typename(SemanticChecker* sc,
        Identifier* identifier)
{
    Declaration* decl = semantic_checker_lookup_ordinairy(sc, identifier, true);
    assert(declaration_is(decl, DECLARATION_TYPEDEF));

    return decl;
}

void declaration_specifiers_finish(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers)
{
    // Now we want to check the signedness is valid. signed or unsigned can only
    // be specified when the type is int.
    switch (specifiers->type_spec_sign)
    {
        case SIGN_SPECIFIER_NONE:
            break;

        case SIGN_SPECIFIER_SIGNED:
        case SIGN_SPECIFIER_UNSIGNED:
            // If it isn't specified we set it to be int. Otherwise make sure
            // that we only have an int type. If we get signed or unsigned then
            // we disregard that in favour of the other
            if (specifiers->type_spec_type == TYPE_SPECIFIER_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_INT;
            }
            else if (specifiers->type_spec_type == TYPE_SPECIFIER_CHAR)
            {
                ; // char is also allowed to be signed or unsigned
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_INT)
            {
                diagnostic_error_at(sc->dm, specifiers->location,
                        "'%s' cannot be signed or unsigned",
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_sign = SIGN_SPECIFIER_NONE;
            }
            break;
    }

    // Fix up some things with the width and type.
    switch (specifiers->type_spec_width)
    {
        case WIDTH_SPECIFIER_NONE:
            break;

        // Here we can only have 'short int' or 'long long int'
        case WIDTH_SPECIFIER_SHORT:
        case WIDTH_SPECIFIER_LONG_LONG:
            // If 'int' was ommited set it, otherwise error about how it is
            // invalid. And set it to be int. 
            if (specifiers->type_spec_type == TYPE_SPECIFIER_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_INT;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_INT)
            {
                diagnostic_error_at(sc->dm, specifiers->location, 
                        "'%s %s' is invalid",
                        width_specifier_to_name(specifiers->type_spec_width),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_type = TYPE_SPECIFIER_INT;
            }
            break;

        // Here we can have 'long int' or 'long double'
        case WIDTH_SPECIFIER_LONG:
            // If 'int' was ommited set it, otherwise error about how it is
            // invalid. And set it to be int. But only if its also not double
            if (specifiers->type_spec_type == TYPE_SPECIFIER_NONE)
            {
                specifiers->type_spec_type = TYPE_SPECIFIER_INT;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_INT &&
                    specifiers->type_spec_type != TYPE_SPECIFIER_DOUBLE)
            {
                diagnostic_error_at(sc->dm, specifiers->location, 
                        "'%s %s' is invalid",
                        width_specifier_to_name(specifiers->type_spec_width),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_type = TYPE_SPECIFIER_INT;
            }
            break;
    }

    switch (specifiers->type_spec_complex)
    {
        case COMPLEX_SPECIFIER_NONE:
            break;

        case COMPLEX_SPECIFIER_COMPLEX:
        case COMPLEX_SPECIFIER_IMAGINAIRY:
            if (specifiers->type_spec_type == TYPE_SPECIFIER_NONE)
            {
                diagnostic_warning_at(sc->dm, specifiers->location,
                        "'%s' requires type specifier; assuming 'double'",
                        complex_specifier_to_name(
                        specifiers->type_spec_complex));
                specifiers->type_spec_type = TYPE_SPECIFIER_DOUBLE;
            }
            else if (specifiers->type_spec_type != TYPE_SPECIFIER_FLOAT &&
                    specifiers->type_spec_type != TYPE_SPECIFIER_DOUBLE)
            {
                diagnostic_error_at(sc->dm, specifiers->location,
                        "'%s %s' is invalid",
                        complex_specifier_to_name(
                        specifiers->type_spec_complex),
                        type_specifier_to_name(specifiers->type_spec_type));
                specifiers->type_spec_complex = COMPLEX_SPECIFIER_NONE;
            }
            break;
    }

    // Now check that we have actually recieved a type during parsing
    switch (specifiers->type_spec_type)
    {
        case TYPE_SPECIFIER_NONE:
            // diagnostic_error_at(sc->dm, specifiers->location,
            //         "type specifier missing, defaults to 'int'");
            // specifiers->type_spec_type = TYPE_SPECIFIER_TYPE_INT;
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
        // Get the read type then check that is a pointer to make sure that
        // this is allowed to be applied.
        QualifiedType real_type = type_get_canonical(type);
        if (!qualified_type_is(&real_type, TYPE_POINTER))
        {
            // Remove the restrict qualifier
            qualifiers &= ~QUALIFIER_RESTRICT;
            diagnostic_error_at(sc->dm, specifiers->location,
                    "restrict requires a pointer");
        }
    }

    // TODO: figure out below if any warning / errors should be used.
    // if (type_qualifier_is_const(qualifiers))
    // {
    //     // if (type->type_base.type == TYPE_VOID)
    //     // {
    //     //     ; // TODO: ...
    //     // }
    // }

    // if (type_qualifier_is_volatile(qualifiers))
    // {
    //     // ; // TODO
    // }

    return (QualifiedType) {.type = type, .qualifiers = qualifiers};
}

QualifiedType qualified_type_from_declaration_specifiers(SemanticChecker* sc,
        const DeclarationSpecifiers* specifiers)
{
    const TypeBuiltins* builtins = &sc->ast->base_types;
    Type* type = NULL;

    switch (specifiers->type_spec_type)
    {
        case TYPE_SPECIFIER_NONE:
            diagnostic_error_at(sc->dm, specifiers->location,
                    "type specifier missing, defaults to 'int'");
            type = builtins->type_signed_int;
            break;

        case TYPE_SPECIFIER_VOID:
            type = builtins->type_void;
            break;

        case TYPE_SPECIFIER_CHAR:
            if (specifiers->type_spec_sign == SIGN_SPECIFIER_NONE)
            {
                type = builtins->type_char;
            }
            else if (specifiers->type_spec_sign == SIGN_SPECIFIER_SIGNED)
            {
                type = builtins->type_signed_char;
            }
            else
            {
                type = builtins->type_unsigned_char;
            }
            break;

        case TYPE_SPECIFIER_INT:
            // If were not unsigned then we always go to being signed.
            if (specifiers->type_spec_sign != SIGN_SPECIFIER_UNSIGNED)
            {
                switch (specifiers->type_spec_width)
                {
                    case WIDTH_SPECIFIER_NONE:
                        type = builtins->type_signed_int;
                        break;

                    case WIDTH_SPECIFIER_SHORT:
                        type = builtins->type_signed_short;
                        break;

                    case WIDTH_SPECIFIER_LONG:
                        type = builtins->type_signed_long;
                        break;

                    case WIDTH_SPECIFIER_LONG_LONG:
                        type = builtins->type_signed_long_long;
                        break;
                }
            }
            else
            {
                switch (specifiers->type_spec_width)
                {
                    case WIDTH_SPECIFIER_NONE:
                        type = builtins->type_unsigned_int;
                        break;

                    case WIDTH_SPECIFIER_SHORT:
                        type = builtins->type_unsigned_short;
                        break;

                    case WIDTH_SPECIFIER_LONG:
                        type = builtins->type_unsigned_long;
                        break;

                    case WIDTH_SPECIFIER_LONG_LONG:
                        type = builtins->type_unsigned_long_long;
                        break;
                }
            }
            break;

        case TYPE_SPECIFIER_FLOAT:
            type = builtins->type_float;
            break;

        case TYPE_SPECIFIER_DOUBLE:
            if (specifiers->type_spec_width == WIDTH_SPECIFIER_LONG)
            {
                type = builtins->type_long_double;
            }
            else
            {
                type = builtins->type_double;
            }
            break;

        case TYPE_SPECIFIER_BOOL:
            type = builtins->type_bool;
            break;

        case TYPE_SPECIFIER_ENUM:
        {
            Declaration* enum_decl = specifiers->declaration;
            assert(declaration_is(enum_decl, DECLARATION_ENUM));

            // NOTE: qualifiers should be none here anyways
            type = enum_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_STRUCT:
        {
            Declaration* struct_decl = specifiers->declaration;
            assert(declaration_is(struct_decl, DECLARATION_STRUCT));

            type = struct_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_UNION:
        {
            Declaration* union_decl = specifiers->declaration;
            assert(declaration_is(union_decl, DECLARATION_UNION));

            type = union_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_TYPENAME:
        {
            Declaration* typename = specifiers->declaration;
            type = typename->tdef.new_type;
            break;
        }
    }

    assert(type != NULL);

    // TODO: support complex and imaginary types...
    
    return add_type_qualifiers(sc, specifiers, type);
}

static QualifiedType process_array_type(SemanticChecker* sc, Declarator* d,
        QualifiedType current, DeclaratorPiece* piece, DeclaratorContext ctx,
        bool* invalid)
{
    assert(piece->base.type == DECLARATOR_PIECE_ARRAY);

    DeclaratorPieceArray* array = (DeclaratorPieceArray*) piece;
    
    Expression* expression = array->expression;
    TypeQualifiers qualifiers = array->qualifiers;
    size_t length = 0;
    bool is_static = array->is_static;
    bool is_star = array->is_star;
    bool is_vla = array->is_star;

     // Now that we are allowed to have static at all
    if (is_static && is_star)
    {
        diagnostic_error_at(sc->dm, array->static_location,
                "'static' may not be used with an unspecified variable "
                "length array size");
        is_static = false;
    }
    else if (is_static && expression == NULL)
    {
        diagnostic_error_at(sc->dm, array->static_location,
                "'static' may not be used without an array size");
        is_static = false;
    }

    // First check for star modifier.
    if (is_star && ctx != DECL_CTX_PARAM)
    {
        diagnostic_error_at(sc->dm, array->lbracket,
                "star modifier used outside of function prototype");
    }

    // Then check for static
    if (ctx != DECL_CTX_PARAM)
    {
        if (is_static)
        {
            diagnostic_error_at(sc->dm, array->static_location,
                    "'static' used in array declarator outside of function "
                    "prototype");
            is_static = false;
            qualifiers = QUALIFIER_NONE;
        }
        else if (qualifiers != QUALIFIER_NONE)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "type qualifier used in array declarator outside of "
                    "function prototype");
            qualifiers = QUALIFIER_NONE;
        }
    }

    // Finally, we need to check if the array's element type is a complete type
    if (!qualified_type_is_complete(&current))
    {
        diagnostic_error_at(sc->dm, array->lbracket,
                "array has incomplete element type");
        return semantic_checker_get_int_type(sc);
    }

    // Finally check for an array of functions
    QualifiedType real_type = qualified_type_get_canonical(&current);
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        Identifier* name = declarator_get_identifier(d);
        const char* string = name != NULL ? name->string.ptr : "type name";
        diagnostic_error_at(sc->dm, array->lbracket,
                "'%s' declared as array of functions", string);
            
        // Recover by making it an int
        return semantic_checker_get_int_type(sc);
    }

    // TODO: will eventually need for work for vlas and other things.

    // TODO: this will need to be changed later to properly fold the expr
    if (expression && expression_is(expression, EXPRESSION_INTEGER_CONSTANT))
    {
        length = expression->integer.value.value;   
    }
    else if (expression)
    {
        diagnostic_error_at(sc->dm, array->lbracket, "cannot fold expression");
    }

    // Finally create and return the new type
    return semantic_checker_create_array(sc, current, length, is_static,
            is_star, is_vla);
}

static QualifiedType process_pointer_type(SemanticChecker* sc, Declarator* d,
        QualifiedType current, DeclaratorPiece* piece, DeclaratorContext ctx,
        bool* invalid)
{
    assert(piece->base.type == DECLARATOR_PIECE_POINTER);

    DeclaratorPiecePointer* pointer = (DeclaratorPiecePointer*) piece;

    return semantic_checker_create_pointer(sc, current, pointer->qualifiers);
}

QualifiedType check_parameter_type(SemanticChecker* sc, Declaration* parameter,
        size_t num_paramaters, bool is_variadic)
{
    QualifiedType type = declaration_get_type(parameter);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Function types should be decayed to pointers to function
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        return semantic_checker_decay_type(sc, type, NULL);
    }
    
    // Array types should be decayed to pointer to element type
    if (qualified_type_is(&real_type, TYPE_ARRAY))
    {
        return semantic_checker_decay_type(sc, type, NULL);
    }

    // If we aren't a void paramater we are good to go. Incomplete types are
    // only checked for for an actual function definition
    if (!qualified_type_is(&real_type, TYPE_VOID))
    {
        return type;
    }

    Location loc = declaration_get_location(parameter);

    // Check that void has not qualifiers and no identifier
    if (num_paramaters == 1 && !is_variadic)
    {
        if (declaration_has_identifier(parameter))
        {
            diagnostic_error_at(sc->dm, loc,
                    "argument may not have 'void' type");

            // Set the declarations type to be 'int' instead
            QualifiedType new_type = semantic_checker_get_int_type(sc);
            declaration_set_type(parameter, new_type);
            declaration_set_invalid(parameter);

            return new_type;
        }
        else if (type_qualifier_has_any(real_type.qualifiers))
        {
            diagnostic_error_at(sc->dm, loc,
                    "'void' as parameter must not have type qualifiers");

            // Get the void type with no parameters instead
            return semantic_checker_get_void_type(sc);
        }
    }

    // Can only have one void type.
    if (num_paramaters > 1 || is_variadic)
    {
        diagnostic_error_at(sc->dm, loc,
                "'void' must be the first and only parameter if specified");

        // Set the declarations type to be 'int' instead
        QualifiedType new_type = semantic_checker_get_int_type(sc);
        declaration_set_type(parameter, new_type);
        declaration_set_invalid(parameter);

        return new_type;
    }

    return type;
}

static QualifiedType process_function_type(SemanticChecker* sc, Declarator* d,
        QualifiedType current, DeclaratorPiece* piece, DeclaratorContext ctx,
        bool* invalid)
{
    assert(piece->base.type == DECLARATOR_PIECE_FUNCTION);

    DeclaratorPieceFunction* function = (DeclaratorPieceFunction*) piece;

    // TODO: is the call below needed?
    QualifiedType clean_type = qualified_type_get_canonical(&current);
    bool return_is_func = qualified_type_is(&clean_type, TYPE_FUNCTION);
    bool return_is_arr = qualified_type_is(&clean_type, TYPE_ARRAY);

    // Shouldn't both be an array and function type
    assert(!(return_is_func && return_is_arr));

    // If we're not invalid already and we returning a function or an array we
    // have a big error. This is the same as clangs stratergy. But I find this
    // weird since it only allows for one error. I think it would be better to
    // keep fixing this error if it occurs.
    if (*invalid == false && (return_is_func || return_is_arr))
    {
        const char* msg = return_is_func
                ? "function cannot return function type"
                : "function cannot return array type";
        diagnostic_error_at(sc->dm, function->lparen_loc, msg);
        
        // Note: clang recovers by setting the return type to be int
        current = semantic_checker_get_int_type(sc);
        *invalid = true;
    }

    // Now we know that the return type of the function will be the current type
    // so now we will check and warn about any qualifiers on the type.
    TypeQualifiers quals = qualified_type_get_quals(&current);
    if (type_qualifier_has_any(quals))
    {
        // TODO: a better location for the pointer qualifiers would be nice here
        Location location = d->identifier_location;
        if (type_qualifier_is_const(quals))
        {
            diagnostic_warning_at(sc->dm, location,
                    "'const' type qualifier on return type has no effect");
        }

        if (type_qualifier_is_volatile(quals))
        {
            diagnostic_warning_at(sc->dm, location,
                    "'volatile' type qualifier on return type has no effect");
        }

        if (type_qualifier_is_restrict(quals))
        {
            diagnostic_warning_at(sc->dm, location,
                    "'restrict' type qualifier on return type has no effect");
        }

        // If we have any qualifiers remove them here.
        current = qualified_type_remove_quals(&current);
    }

    // Extract the declarations and their types so that they can be used in
    // the function type.
    DeclarationList decls = function->paramaters;
    size_t num_paramaters = function->num_paramaters;

    // Allocate for our future list of parameters
    TypeFunctionParameter* params = NULL;
    TypeFunctionParameter* curr_param = NULL;

    // Get the entry and the index of the current parameter
    DeclarationListEntry* entry = declaration_list_iter(&decls);
    for (; entry != NULL; entry = declaration_list_next(entry))
    {
        Declaration* decl = declaration_list_entry_get(entry);
        QualifiedType param_type = check_parameter_type(sc, decl,
                num_paramaters, function->is_variadic);
            
        // Create the function parameter that we are going to push
        TypeFunctionParameter* new_param = type_create_function_parameter(
                &sc->ast->ast_allocator, param_type);

        // Append to our list of parameters creating it as we go.
        if (params == NULL)
        {
            params = new_param;
        }
        else
        {
            type_function_parameter_set_next(curr_param, new_param);
        }
        curr_param = new_param;
    }

    // Do a finaly check where we set the number of parameters to 0 if there is
    // one parameter, and we arent variadic, and the parameter has type void
    if (num_paramaters == 1 && !function->is_variadic)
    {
        assert(params);
        
        QualifiedType type = type_function_parameter_get_type(params);
        QualifiedType real_type = qualified_type_get_canonical(&type);
        if (qualified_type_is(&real_type, TYPE_VOID))
        {
            num_paramaters = 0;
            params = NULL;
        }
    }

    // Finally create the function type and update the current type.
    return semantic_checker_create_function(sc, current, params, num_paramaters,
            false, function->is_variadic);
}

QualifiedType semantic_checker_process_type(SemanticChecker* sc,
        Declarator* declarator)
{
    // TODO: now that we have the declarator context we can acccept / decline
    // TODO: vmt and vla's

    DeclaratorContext context = declarator->context;
    QualifiedType type = qualified_type_from_declaration_specifiers(sc, 
            declarator->specifiers);

    bool invalid = false;
    
    // The go through all of our pieces until we reach the end of our piece
    // stack. Keeping track of if we got an invalid declaration
    DeclaratorPiece* piece = declarator->piece_stack;
    for (; piece != NULL; piece = piece->base.next)
    {
        switch (piece->base.type)
        {
            case DECLARATOR_PIECE_ARRAY:
                type = process_array_type(sc, declarator, type, piece, context,
                        &invalid);
                break;

            case DECLARATOR_PIECE_POINTER:
                type = process_pointer_type(sc, declarator, type, piece,
                        context, &invalid);
                break;

            case DECLARATOR_PIECE_FUNCTION:
                type = process_function_type(sc, declarator, type, piece,
                        context, &invalid);
                break;

            case DECLARATOR_PIECE_KNR_FUNCTION:
                panic("unimplemented knr function piece processing");
                break;

            default:
                panic("unexpected declarator piece type");
                break;
        }
    }

    // If we had something that makes this invalid we need to set the declarator
    // so that the invalidation can be handled again further down the chain
    if (invalid)
    {
        declarator_set_invalid(declarator);
    }

    // type_print(&type);
    // printf("\n");

    return type;
}

static void semantic_checker_diagnose_inline(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers)
{
    TypeFunctionSpecifier function = specifiers->function_spec;
    if (function != FUNCTION_SPECIFIER_NONE)
    {
        diagnostic_error_at(sc->dm, specifiers->location,
                "'%s' can only appear on functions",
                function_specifier_to_name(function));
    }
}

Declaration* semantic_checker_process_specifiers(SemanticChecker* sc,
        DeclarationSpecifiers* specifiers)
{
    // Our warning location :)
    Location location = specifiers->location;

    // Get the declaration from the specifiers if needed
    Declaration* to_return = NULL;
    switch (specifiers->type_spec_type)
    {
        case TYPE_SPECIFIER_ENUM:
        case TYPE_SPECIFIER_STRUCT:
        case TYPE_SPECIFIER_UNION:
            to_return = specifiers->declaration;
            break;

        // At default warn if the declaration will be useless
        default:
            break;
    }

    TypeQualifiers quals = specifiers->qualifiers;
    if (type_qualifier_is_restrict(quals))
    {
        diagnostic_error_at(sc->dm, location,
                "restrict requires a pointer");
    }

    // Diagnose the use of the inline keyword
    semantic_checker_diagnose_inline(sc, specifiers);

    // Now give some nice warnings about the storage if needed
    StorageSpecifier storage = specifiers->storage_spec;
    switch (storage)
    {
        case STORAGE_NONE:
            if (to_return == NULL)
            {
                diagnostic_warning_at(sc->dm, location, 
                        "declaration does not declare anything");
                return NULL;
            }
            else
            {
                break;
            }

        case STORAGE_AUTO:
        case STORAGE_EXTERN:
        case STORAGE_REGISTER:
        case STORAGE_STATIC:
        {
            if (to_return != NULL)
            {
                diagnostic_warning_at(sc->dm, location,
                        "'%s' ignored on this declaration",
                        storage_specifier_to_name(storage));
                break;
            }
            else
            {
                diagnostic_warning_at(sc->dm, location, 
                        "declaration does not declare anything");
                return to_return;
            }
        }

        // The guarantee above does not apply here...
        case STORAGE_TYPEDEF:
            diagnostic_warning_at(sc->dm, location, "typedef requires a name");
            break;

        default:
            panic("bad storage value");
            break;
    }

    // Finally warn about unneeded qualifiers if present.
    if (type_qualifier_is_const(quals))
    {
        diagnostic_warning_at(sc->dm, location,
                "'const' ignored on this declaration");
    }

    if (type_qualifier_is_volatile(quals))
    {
        diagnostic_warning_at(sc->dm, location,
                "'volatile' ignored on this declaration");
    }

    return to_return;
}

QualifiedType semantic_checker_process_typename(SemanticChecker* sc,
        Declarator* declarator)
{
    assert(declarator->identifier == NULL);

    // TODO: should probably do something about inline etc...

    return semantic_checker_process_type(sc, declarator);
}

static Declaration* semantic_checker_lookup_previous(SemanticChecker* sc,
        Identifier* identifier, bool check_linkage, bool* this_scope)
{
    // If we got a pointer then make sure it is default to false
    assert(this_scope ? *this_scope == false : true);

    // Lookup the previous but only in the current scope
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);

    // If we found a previous declaration in the initial scope set this scope
    // to be true to note that we found it in this scope.
    if (previous != NULL)
    {
        if (this_scope != NULL)
        {
            *this_scope = true;
        }

        return previous;
    }

    if (previous == NULL && check_linkage)
    {
        previous = semantic_checker_lookup_external(sc, identifier);
    }

    return previous;
}

Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator)
{
    assert(scope_is(sc->scope, SCOPE_FUNCTION));
    assert(declarator->context == DECL_CTX_PARAM);

    // Make sure to decay the type since it is a function parameter.
    QualifiedType type = semantic_checker_process_type(sc, declarator);
    type = semantic_checker_decay_type(sc, type, NULL);

    // Check that we have no storage specifier other then register
    StorageSpecifier storage = declarator->specifiers->storage_spec;
    if (storage != STORAGE_NONE && storage != STORAGE_REGISTER)
    {
        diagnostic_error_at(sc->dm, declarator->specifiers->location,
                "invalid storage class specifier '%s' in function declarator",
                storage_specifier_to_name(storage));
        storage = STORAGE_NONE;
    }

    // Diagnose the use of inline
    semantic_checker_diagnose_inline(sc, declarator_get_specifiers(declarator));

    // Create the declaration for the function parameter. And add it into the
    // scope if we got an identifier and it wasn't a duplicate parameter.
    // NOTE: function parameters can NEVER have linkage
    Identifier* identifier = declarator_get_identifier(declarator);
    Declaration* declaration =  declaration_create_variable(
            &sc->ast->ast_allocator, declarator->identifier_location,
            identifier, type, storage, DECLARATION_LINKAGE_NONE);

    // Check for previous declaration of function parameter.
    Declaration* previous = semantic_checker_lookup_previous(sc, identifier,
            false, NULL);
    if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, declarator_get_location(declarator),
                "redefinition of parameter '%s'", identifier->string.ptr);
        declaration_set_invalid(declaration);
    }
    else
    {
        semantic_checker_insert_ordinairy(sc, declaration);
    }

    return declaration;
}

// Returns true if there we are errors int the function declaration and false
// otherwise.
static void semantic_checker_check_function_redeclaration(SemanticChecker* sc,
        Declaration* function, Declaration* previous, bool is_defn)
{
    assert(declaration_is(function, DECLARATION_FUNCTION));

    // Don't try hard for invalid declarations
    if (!declaration_is_valid(function))
    {
        return;
    }

    // Don't do anything if there was no previous decl
    if (previous == NULL)
    {
        return;
    }

    // First look up to see if we got a previous variable in the current scope
    Identifier* identifier = declaration_get_identifier(function);
    Location location =declaration_get_location(function);

    if (!declaration_is(previous, DECLARATION_FUNCTION))
    {
        diagnostic_error_at(sc->dm, location,
                "redefinition of '%s' as a different kind of symbol",
                identifier->string.ptr);
        declaration_set_invalid(function);
        return;
    }

    // Otherwise we check if the types are compatible and error is they arent.
    QualifiedType new_type = declaration_get_type(function);
    QualifiedType old_type = declaration_get_type(previous);
    if (!qualified_type_is_compatible(&new_type, &old_type))
    {
        diagnostic_error_at(sc->dm, location, "conflicting types for '%s'",
                identifier->string.ptr);
        declaration_set_invalid(function);
        return;
    }

    // Now here we know we have functions which have the same type. So now we
    // will only need to check the linkage on the functions
    DeclarationLinkage new_linkage = declaration_function_get_linkage(function);
    DeclarationLinkage old_linkage = declaration_function_get_linkage(previous);

    // It is only a problem if we try to go from externel to internal...
    if (old_linkage == DECLARATION_LINKAGE_EXTERNAL
            && new_linkage == DECLARATION_LINKAGE_INTERNAL)
    {
        diagnostic_error_at(sc->dm, location,
                "static declaration of '%s' follows non-static declaration",
                identifier->string.ptr);
        declaration_set_invalid(function);
        return;
    }

    // Finally check for function redefinitions.
    if (declaration_function_has_definition(previous) && is_defn)
    {
        diagnostic_error_at(sc->dm, location, "redefinition of '%s'",
                identifier->string.ptr);
        declaration_set_invalid(function);
    }
}

static void semantic_checker_chain_function_declaration(SemanticChecker* sc,
        Declaration* function, Declaration* previous)
{
    // Don't chain non valid declarations
    if (!declaration_is_valid(function))
    {
        return;
    }

    // This should be true if variable is 'valid'
    assert(declaration_is(function, DECLARATION_FUNCTION));
    assert(declaration_is(previous, DECLARATION_FUNCTION));

    // We want to add this function declaration to the previous
    declaration_function_add_decl(previous, function);    
}

Declaration* semantic_checker_process_function_declaration(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    QualifiedType real_type = qualified_type_get_canonical(&type);
    assert(qualified_type_is(&real_type, TYPE_FUNCTION));

    bool invalid = false;

    DeclarationSpecifiers* specifiers = declarator_get_specifiers(declarator);
    Identifier* identifier = declarator->identifier;
    Location location = declarator->identifier_location;
    assert(identifier != NULL);

    // Chceck that the functions storage is valid based on the context we are in
    DeclaratorContext ctx = declarator_get_context(declarator);
    StorageSpecifier storage = declaration_specifiers_storage(specifiers);
    switch (storage)
    {
        // These are allowed regardless of the scope
        case STORAGE_NONE:
        case STORAGE_EXTERN:
            break;

        // This is only allowed if we are not in block scope.
        case STORAGE_STATIC:
            if (ctx == DECL_CTX_BLOCK)
            {
                diagnostic_error_at(sc->dm, location, "function declared in "
                        "block scope cannot have 'static' storage class");
                invalid = true;
                storage = STORAGE_NONE;
            }
            break;

        // The storages are not allowed at all for functions...
        case STORAGE_AUTO:
        case STORAGE_REGISTER:
            diagnostic_error_at(sc->dm, location,
                    "illegal storage class '%s' on function",
                    storage_specifier_to_name(specifiers->storage_spec));
            invalid = true;
            storage = STORAGE_NONE;
            break;

        // If we got a 'typedef' function that means we should have known before
        // that we are about to process a function definition. So here we catch
        // that and error.
        case STORAGE_TYPEDEF:
            assert(declarator_is_func_defn(declarator));
            diagnostic_error_at(sc->dm, specifiers->location,
                    "function definition declared 'typedef'");
            invalid = true;
            storage = STORAGE_NONE;
            break;

        default:
            panic("invalid storage specifier");
            break;
    }

    // We must get all of the declarations in the function piece before creating
    // the function declaration itself so that we can access them later.
    DeclaratorPiece* piece = declarator_get_function_piece(declarator);
    DeclarationList decls = declarator_function_piece_get_decls(piece);

    // Compute the function linkage here
    DeclarationLinkage linkage = storage == STORAGE_STATIC
            ? DECLARATION_LINKAGE_INTERNAL
            : DECLARATION_LINKAGE_EXTERNAL;

    // Create our new function declaration.
    Declaration* declaration = declaration_create_function(
            &sc->ast->ast_allocator, location, identifier, type,
            storage, specifiers->function_spec, decls, linkage);

    // Also get whether we were expecting this function to have a definiton
    bool is_defn = declarator_is_func_defn(declarator);

    // Lookup the previous declaration in this scope.
    bool this_scope = false;
    Declaration* previous = semantic_checker_lookup_previous(sc, identifier,
            true, &this_scope);

    // Now check for function redeclarations
    semantic_checker_check_function_redeclaration(sc, declaration, previous,
            is_defn);

    // If we didn't find anything previously in any scope we should add to the
    // lexical scope. Or, if we found something but it wasn't in this scope we
    // should add it to this scope.
    if (previous == NULL || (previous != NULL && this_scope == false))
    {
        semantic_checker_insert_ordinairy(sc, declaration);
    }

    // Now we want to merge the function declarations since all function 
    // declarations have linkage. But if it's null we can simply just insert it
    // into the externals scope.
    if (previous == NULL)
    {
        semantic_checker_insert_external(sc, declaration);
    }
    else
    {
        semantic_checker_chain_function_declaration(sc, declaration, previous);
    }

    // Again like variable decl's will need to set it invalid after
    if (invalid)
    {
        declaration_set_invalid(declaration);
    }

    return declaration;
}

// Returns true if some error occured with variable redeclaration
static void semantic_checker_check_variable_redeclaration(SemanticChecker* sc,
        Declaration* variable, Declaration* old_decl)
{
    // Give up if we're already invalid
    if (!declaration_is_valid(variable))
    {
        return;
    }

    // Can't check decalration if we don't have an old declaration
    if (old_decl == NULL)
    {
        return;
    }

    Identifier* identifier = declaration_get_identifier(variable);
    Location location = declaration_get_location(variable);

    // If it wasn't a variable we get different kind of symbol warning
    if (!declaration_is(old_decl, DECLARATION_VARIABLE))
    {
        diagnostic_error_at(sc->dm, location,
                "redefinition of '%s' as a different kind of symbol",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    // Otherwise we check if the types are compatible and error is they arent.
    QualifiedType new_type = declaration_get_type(variable);
    QualifiedType old_type = declaration_get_type(old_decl);
    if (!qualified_type_is_compatible(&new_type, &old_type))
    {
        diagnostic_error_at(sc->dm, location,
                "redefinition of '%s' with a different type",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    // Below we catch some possible errors to do with variable linkage and the
    // definitions of them.
    if (declaration_variable_has_linkage(variable)
            && declaration_variable_is_extern(variable)
            && declaration_variable_has_linkage(old_decl))
    {
        // This is actually fine. since extern declarations can follow static
        // ones if the static ones had linkage.
        // ```static int foo; extern int foo;``` is fine
    }
    else if (declaration_variable_has_linkage(variable)
            && declaration_variable_is_extern(variable)
            && declaration_get_storage_class(old_decl) == STORAGE_STATIC)
    {
        // Here we are catching function local static's being redeclared as
        // extern.
        assert(!declaration_variable_has_linkage(old_decl));
        diagnostic_error_at(sc->dm, location,
                "non-static declaration of '%s' follows static declaration",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    if (declaration_get_storage_class(variable) == STORAGE_STATIC
            && declaration_variable_has_linkage(old_decl)
            && declaration_variable_is_extern(old_decl))
    {
        diagnostic_error_at(sc->dm, location,
                "static declaration of '%s' follows non-static declaration",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    if (declaration_variable_has_linkage(old_decl)
            && declaration_variable_is_extern(old_decl)
            && !declaration_variable_has_linkage(variable))
    {
        diagnostic_error_at(sc->dm, location,
                "non-extern declaration of '%s' follows extern declaration",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    if (declaration_variable_has_linkage(variable)
            && declaration_variable_is_extern(variable)
            && !declaration_variable_has_linkage(old_decl))
    {
        diagnostic_error_at(sc->dm, location,
                "extern declaration of '%s' follows non-extern declaration",
                identifier->string.ptr);
        declaration_set_invalid(variable);
        return;
    }

    // Get if we have linkage and error accordingly
    bool has_linkage = declaration_variable_has_linkage(variable);

    // Finally here, we want to give a redefinition error if we can determine
    // one has been found.
    if (has_linkage && declaration_variable_has_linkage(old_decl))
    {
        // Nothing to do here. We have linkage and the old one does too and we
        // have found that the declarations are constent with each other. So
        // since we are allowed to have multiple of the same externel
        // definitons we can just do nothing.
    }
    else if (has_linkage && !declaration_variable_has_linkage(old_decl))
    {
        panic("unreachable");

        // If we are here this means we have an extern function local but the
        // previous declaration was non-extern (or static). Note, that this is
        // already caught by the above code.
    }
    else if (!has_linkage && declaration_variable_has_linkage(old_decl))
    {
        panic("unreachable");

        // Here the new variable doesn't have linkage (must be a function local)
        // but the old one does. Meaning that we old one was declared in a
        // function for us to have been able to find it (otherwise we wouldn't
        // have looked in the externs table) But note that we have already dealt
        // with this case. non-extern extern.
    }
    else if (!has_linkage && !declaration_variable_has_linkage(old_decl))
    {
        // Redefinition of function locals. e.g. ``` int foo; int foo;```
        diagnostic_error_at(sc->dm, declaration_get_location(variable),
                "redefinition of '%s'", identifier->string.ptr);
        declaration_set_invalid(variable);
    }
}

static void semantic_checker_chain_variable_declaration(SemanticChecker* sc,
        Declaration* variable, Declaration* previous)
{
    // Don't chain invalid declarations
    if (!declaration_is_valid(variable))
    {
        return;
    }

    // This should be true if variable is 'valid'
    assert(declaration_is(variable, DECLARATION_VARIABLE));
    assert(declaration_is(previous, DECLARATION_VARIABLE));

    // Note that we should only chain variables if they have linkage. Otherwise
    // we are doing something quite wrong.
    assert(declaration_variable_has_linkage(variable));
    assert(declaration_variable_has_linkage(previous));
    
    // declaration_variable_add_decl(previous, variable);

    // Note: that we will also have to do some calculations for it the variable
    // is tentative or not.
}

Declaration* semantic_checker_process_variable(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    // True if the declaration should be set to invalid
    bool invalid = false;

    // Get our identifier and it's location
    Identifier* identifier = declarator_get_identifier(declarator);
    Location identifer_loc = declarator_get_location(declarator);
    assert(identifier != NULL && identifer_loc != LOCATION_INVALID);

    // Get the context of the declarator
    DeclaratorContext ctx = declarator_get_context(declarator);
    assert(ctx == DECL_CTX_BLOCK || ctx == DECL_CTX_FILE);

    // Now get the storage of the declarator
    DeclarationSpecifiers* spec = declarator_get_specifiers(declarator);
    StorageSpecifier storage = declaration_specifiers_storage(spec);
    assert(storage != STORAGE_TYPEDEF);

    if (ctx == DECL_CTX_FILE
            && (storage == STORAGE_AUTO || storage == STORAGE_REGISTER))
    {
        diagnostic_error_at(sc->dm, identifer_loc,
                "illegal storage class '%s' on file-scoped variable",
                storage_specifier_to_name(storage));
        invalid = true;
    }

    // Diagnose the use of the inline keyword on any variable declaration
    semantic_checker_diagnose_inline(sc, spec);

    // Now determine the type of linkage that this variable has.
    // Ignoring any errors with auto or register file scoped variables have
    // external linkage automatically. So <none>, and 'extern' will have 
    // external linkage. Whereas static will have interal linkage.
    // Block context is different. Here if the storage is none, register, or
    // auto then linkage stays as none. static is a special case here where,
    // it does not affect the linage if it is at block scope. Otherwise, if
    //  it is extern, only then does it have external linkage.
    DeclarationLinkage linkage = DECLARATION_LINKAGE_NONE;
    if (storage == STORAGE_EXTERN)
    {
        linkage = DECLARATION_LINKAGE_EXTERNAL;
    }
    else if (ctx == DECL_CTX_FILE)
    {
        if (storage == STORAGE_STATIC)
        {
            linkage = DECLARATION_LINKAGE_INTERNAL;
        }
        else
        {
            linkage = DECLARATION_LINKAGE_EXTERNAL;
        }
    }

    // Create the new deckaration that we use.
    Declaration* new_var = declaration_create_variable(&sc->ast->ast_allocator,
            identifer_loc, identifier, type, storage, linkage);
    
    // Set invalid if needed. example of why this is after checking
    // redeclaration ```register int foo;``` (at file scope). We still want to
    // add to the sybol table
    if (invalid)
    {
        declaration_set_invalid(new_var);
    }

    // Lookup the previous declaration to see if we need to chain them or not
    bool has_linkage = linkage != DECLARATION_LINKAGE_NONE;
    bool this_scope = false;
    Declaration* previous = semantic_checker_lookup_previous(sc, identifier,
            has_linkage, &this_scope);

    // Now we want to check for variable redeclaration
    semantic_checker_check_variable_redeclaration(sc, new_var, previous);

    // Below we figure out what scope(s) to add the declaration too. Considering
    // the case of the ordinairy and external's seperately

    // If we didn't find anything previously in any scope we should add to the
    // lexical scope. Or, if we found something but it wasn't in this scope we
    // should add it to this scope.
    if (previous == NULL || (previous != NULL && this_scope == false))
    {
        semantic_checker_insert_ordinairy(sc, new_var);
    }

    // If we didn't find anything and had linkage then we should add it to the 
    // external scope as well.
    if (has_linkage)
    {
        if (previous == NULL)
        {
            semantic_checker_insert_external(sc, new_var);
        }
        else
        {
            semantic_checker_chain_variable_declaration(sc, new_var, previous);
        }
    }

    return new_var;
}

Declaration* semantic_checker_process_typedef(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    DeclarationSpecifiers* specifiers = declarator_get_specifiers(declarator);
    Identifier* identifier = declarator_get_identifier(declarator);
    Location identifer_loc = declarator_get_location(declarator);

    // Diagnose the use of the inline keyword
    semantic_checker_diagnose_inline(sc, specifiers);

    // Create the typedef and the new type setting up the declaration fully.
    Declaration* tdef = declaration_create_typedef(&sc->ast->ast_allocator,
            identifer_loc, identifier, type);
    Type* new_type = type_create_typedef(&sc->ast->ast_allocator, type, tdef);
    declaration_typedef_set_type(tdef, new_type);
    
    // Check for other declarations already present
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);
    if (previous != NULL)
    {
        // TODO: c11 allows for typedef redefinitions
        bool previous_typedef = declaration_is(previous, DECLARATION_TYPEDEF);
        const char* msg = previous_typedef
                ? "redefinition of typedef '%s'"
                : "redefinition of '%s' as different kind of symbol";
        diagnostic_error_at(sc->dm, declarator->identifier_location, msg,
                identifier->string.ptr);
    }
    else
    {
        semantic_checker_insert_ordinairy(sc, tdef);
    }

    return tdef;
}

Declaration* semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator)
{
    // Don't even try if we have an invalid declarator. This is mainly here
    // to simplify the logic of the parsing functions.
    if (declarator->invalid)
    {
        return NULL;
    }

    // Regardless of what needs to be done next we must figure our the type
    // from the declarator.
    QualifiedType type = semantic_checker_process_type(sc, declarator);
    QualifiedType real_type = qualified_type_get_canonical(&type);
    
    // The main different types of declarations we will have to handle are below
    DeclarationSpecifiers* spec = declarator_get_specifiers(declarator);
    StorageSpecifier storage = declaration_specifiers_storage(spec);

    // Only process this as a typedef if it is not a function definition
    if (storage == STORAGE_TYPEDEF
            && !declarator_is_func_defn(declarator))
    {
        return semantic_checker_process_typedef(sc, declarator, type);        
    }
    
    // Catch function definitions here
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        return semantic_checker_process_function_declaration(sc, declarator,
                type);
    }

    return semantic_checker_process_variable(sc, declarator, type);
}

Declaration* semantic_checker_process_struct_declarator(SemanticChecker* sc,
        Declaration* struct_decl, Declarator* declarator)
{
    // Regardless of what needs to be done next we must figure our the type
    // from the declarator.
    QualifiedType type = semantic_checker_process_type(sc, declarator);

    // Don't even try if we have an invalid declarator. This is mainly here
    // to simplify the logic of the parsing functions.
    if (declarator_is_invalid(declarator))
    {
        return NULL;
    }

    // Ensure that the storage specifiers were removed already.
    assert(declarator->specifiers->storage_spec == STORAGE_NONE);

    Identifier* identifier = declarator->identifier;
    Location identifier_loc = declarator->identifier_location;
    Location colon_location = declarator_get_colon_location(declarator);
    Expression* expression = declarator_get_bitfield_expression(declarator);

    // We must either have an identifer of a bitfield
    assert(identifier != NULL || colon_location != LOCATION_INVALID);

    // Track if we were given an invalid field so we can give nicer errors
    bool invalid = false;

    // Check that the type is complete.
    if (!qualified_type_is_complete(&type))
    {
        diagnostic_error_at(sc->dm, identifier_loc,
                "field has incomplete type");

        // Remove possible bitfield expression
        colon_location = LOCATION_INVALID;
        expression = NULL;

        invalid = true;
    }

    // Here we only want to lookup if we got an identifier for this.
    Declaration* previous = semantic_checker_lookup_member(sc, identifier);
    if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, identifier_loc, "duplicate member '%s'",
                identifier->string.ptr);
        return NULL;
    }

    // TODO: change to expression part only once we fully have them
    if (colon_location != LOCATION_INVALID /*expression != NULL*/)
    {        
        // check the bitfield is of an integral type...
        if (!qualified_type_is_integer(&type))
        {
            diagnostic_error_at(sc->dm, identifier_loc,
                    "bit-field '%s' has not integral type",
                    identifier->string.ptr);
            
            // Skip the rest of the checks and make sure to construct it without
            // a bitfield
            colon_location = LOCATION_INVALID;
            expression = NULL;

            invalid = true;
            goto make_decl;
        }

        // Then check that the bitfield expression itself has an integral type
        // Finally, check it has a valid width...
        if (expression_is_invalid(expression))
        {
            colon_location = LOCATION_INVALID;
            expression = NULL;
        }
        else
        {
            QualifiedType e_type = expression_get_qualified_type(expression);
            if (!qualified_type_is_integer(&e_type))
            {
                diagnostic_error_at(sc->dm, expression_get_location(expression),
                        "bit-field expression must have integer type");
                colon_location = LOCATION_INVALID;
                expression = NULL;
            }
        }
    }

    // Check that we do not have a function type. note that a pointer to a
    // function is fine here.
    if (qualified_type_is(&type, TYPE_FUNCTION))
    {
        // Do not return NULL here since we will allow this for syntax purposes
        diagnostic_error_at(sc->dm, identifier_loc,
                "field '%s' declared as a function", identifier->string.ptr);
        invalid = true;
    }

    // Create the declaration for the field of the struct
make_decl:;
    Declaration* member_decl = declaration_create_field(&sc->ast->ast_allocator,
            identifier_loc, identifier, type, colon_location, expression);
    if (invalid)
    {
        // Set both the member declaration and struct declaration to be invalid
        // since this will speed up computing the struct fields if it is invalid
        declaration_set_invalid(member_decl);
        declaration_set_invalid(struct_decl);
    }

    semantic_checker_insert_member(sc, member_decl);

    return member_decl;
}

// TODO: this should be moved to live with all of my function handling functions
void semantic_checker_add_function_parameters(SemanticChecker* sc,
        Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_FUNCTION));

    // Add all of the decalrations from the function scope into the new function
    DeclarationList decls = declaration_function_get_paramaters(declaration);
    DeclarationListEntry* iter = declaration_list_iter(&decls);
    for (; iter != NULL; iter = declaration_list_next(iter))
    {
        Declaration* decl = declaration_list_entry_get(iter);

        // Any tag declarations get auto inserted
        if (declaration_is_tag(decl))
        {
            semantic_checker_insert_tag(sc, decl);
            continue;
        }

        // Otherwise we should have a function parameter here.
        QualifiedType type = declaration_get_type(decl);
        QualifiedType real_type = qualified_type_get_canonical(&type);

        // HACK: to skip adding void 'parameter' if present I want to fix this
        // up properly to only add the parameter definitions and have the
        // other definitions seperate.
        if (qualified_type_is(&real_type, TYPE_VOID))
        {
            continue;
        }

        // Make sure we have a complete type even if it doesn't have a name
        if (!qualified_type_is_complete(&real_type))
        {
            diagnostic_error_at(sc->dm, decl->base.location,
                    "variable has incomplete type");
            declaration_set_invalid(declaration);
        }

        // Insert into the symbol table if we have an identifier
        if (declaration_has_identifier(decl))
        {            
            semantic_checker_insert_ordinairy(sc, decl);
        }
        else
        {
            diagnostic_error_at(sc->dm, decl->base.location,
                    "paramater name must not be omitted in a function "
                    "definition");
        }
    }
}

void semantic_checker_handle_function_start(SemanticChecker* sc,
        Declaration* function)
{
    // We have already have an invalid declaration then we just accept it
    // without any more errors and just leave it at that.
    if (!declaration_is_valid(function))
    {
        return;
    }

    // // Otherwise get the main declaration for the function and check whether
    // // that declaration has a function body or not. If it does not then we are
    // // good to go. Otherwise, if it does error about it and indicated that we
    // // should not use it as the main definition.
    // Identifier* identifier = declaration_get_identifier(function);
    // Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
    //         false);
    // assert(previous != NULL);

    // // Check that the function does not already have a body.
    // if (declaration_function_has_body(previous))
    // {
    //     diagnostic_error_at(sc->dm, declaration_get_location(function),
    //             "redefinition of '%s'",
    //             declaration_get_identifier(function)->string.ptr);
    // }
}

void semantic_checker_handle_function_end(SemanticChecker* sc,
        Declaration* function, Statement* body)
{
    // First set the body of this function.
    assert(declaration_is(function, DECLARATION_FUNCTION));
    declaration_function_set_body(function, body);

    // Then if we want to get the previous declaration for this function and set
    // the body of the main declaration.
    Identifier* identifier = declaration_get_identifier(function);
    Declaration* previous = semantic_checker_lookup_external(sc, identifier);

    // Note that if previous == function then this was the first declaration of
    // the function. If this is the case still set the definition field to be
    // the pointer to itself so we can still easily detect function redefinition

    // Otherwise we want to set the body of the function definition to be this
    declaration_function_set_definition(previous, function);
}

bool semantic_checker_check_initializer_allowed(SemanticChecker* sc,
        Declaration* declaration, DeclaratorContext context)
{
    // First we must check that the declaration is allowed to have an 
    // initializer. If not error about it.
    if (!declaration_is(declaration, DECLARATION_VARIABLE))
    {
        diagnostic_error_at(sc->dm, declaration_get_location(declaration),
                "illegal initializer (only variables can be initialized)");
        declaration_set_invalid(declaration);
        return false;
    }

    // Now check the variable isn't a block scope extern. Since those cannot
    // have identifiers under any circumstances.
    if (declaration_is_valid(declaration) && context == DECL_CTX_BLOCK
            && declaration_variable_has_linkage(declaration)
            && declaration_variable_is_extern(declaration))
    {
        diagnostic_error_at(sc->dm, declaration_get_location(declaration),
                "declaration of block scope identifier with linkage cannot "
                "have an initializer");
        declaration_set_invalid(declaration);
        return false;
    }

    return true;
}

void semantic_checker_declaration_add_initializer(SemanticChecker* sc,
        Declaration* declaration, Location equals, Initializer* initializer)
{
    // Otherwise add the initializer to the declaration
    if (declaration_is_valid(declaration))
    {
        declaration_variable_add_initializer(declaration, initializer);
    }
}

void semantic_checker_declaration_finish(SemanticChecker* sc,
        Declaration* declaration)
{
    // Ignore any invalid declarations as to not produce more errors from them
    if (!declaration_is_valid(declaration))
    {
        return;
    }

    switch (declaration_get_kind(declaration))
    {
        case DECLARATION_VARIABLE:
        {
            QualifiedType type = declaration_get_type(declaration);
            if (!qualified_type_is_complete(&type))
            {
                diagnostic_error_at(sc->dm,
                        declaration_get_location(declaration),
                        "variable '%s' has incomplete type",
                        declaration_get_identifier(declaration)->string.ptr);
                declaration_set_invalid(declaration);
            }
            break;
        }
            
        // Do nothing for the others but make it easier to add stuff later...
        default:
            break;
    }
}

void semantic_checker_finish_struct_declaration(SemanticChecker* sc,
        Declaration* struct_declaration)
{
    assert(declaration_is(struct_declaration, DECLARATION_STRUCT) ||
            declaration_is(struct_declaration, DECLARATION_UNION));

    // Start by setting the struct declaration to be considered complete so
    // that even if errors occur when finishing the definition then we still
    // leave with a complete struct.
    declaration_struct_set_complete(struct_declaration);

    // Then check if the struct declaration was invalid or not. If it was
    // invalid then we can skip everything below and simply return.
    bool valid = declaration_is_valid(struct_declaration);
    if (!valid)
    {
        return;
    }

    bool is_struct = declaration_is(struct_declaration, DECLARATION_STRUCT);
}

static Declaration* semantic_checker_create_enum_constant(SemanticChecker* sc,
        Location location, Identifier* identifier, Location equals,
        QualifiedType type, Expression* expression, int value)
{
    return declaration_create_enum_constant(&sc->ast->ast_allocator, location,
            identifier, type, equals, expression, value);
}

Declaration* semantic_checker_handle_enum_constant(SemanticChecker* sc,
        Location location, Identifier* identifier, Location equals,
        Expression* expression, Declaration* last_decl)
{
    Declaration* previous = scope_lookup_ordinairy(sc->scope, identifier,
            false);
    if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, location, "redefinition of '%s'",
                identifier->string.ptr);
        return declaration_create_error(&sc->ast->ast_allocator, location);
    }

    // TODO: also need to determine the value of the enum
    int value;
    if (expression != NULL)
    {
        // Here we got given an expression that we want to fold
        if (!expression_is(expression, EXPRESSION_INTEGER_CONSTANT))
        {
            diagnostic_error_at(sc->dm, expression_get_location(expression),
                    "expression cannot be folded at this time");
            value = 0;
        }
        else
        {
            IntegerValue int_value = expression->integer.value;

            if (int_value.value > INT_MAX)
            {
                diagnostic_error_at(sc->dm, location,
                        "enumerator value '%s' not in range of int",
                        identifier->string.ptr);
                value = INT_MAX;
            }
            else
            {
                value = (int) int_value.value;
            }
        }
    }
    else
    {
        if (last_decl == NULL)
        {
            value = 0;
        }
        else
        {
            value = declaration_enum_constant_get_value(last_decl);
    
            if (value == INT_MAX)
            {
                diagnostic_error_at(sc->dm, location,
                        "overflow in enumeration value '%s'",
                        identifier->string.ptr);
                value = 0;
            }
            else
            {
                value++;
            }
        }
    }
    
    // Create and add the declaration into the ordinairy namespace
    Declaration* new_decl = semantic_checker_create_enum_constant(sc, location,
            identifier, equals, semantic_checker_get_int_type(sc), expression,
            value);
    semantic_checker_insert_ordinairy(sc, new_decl);

    return new_decl;
}

static Declaration* semantic_checker_create_enum(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous)
{
    // NOTE: if the enum is anonymous it is not inserted into the tag symbols

    // Create the enum type and a declaration for the enum itself
    QualifiedType type = type_create_enum(&sc->ast->ast_allocator,
            sc->ast->base_types.type_signed_int);
    Declaration* decl = declaration_create_enum(&sc->ast->ast_allocator, 
            enum_location, name, type, anonymous);
    type_enum_set_declaration(&type, decl);

    // Only, insert it into the scope if it's not anonymous
    if (!anonymous)
    {
        semantic_checker_insert_tag(sc, decl);
    }

    return decl;
}

static Declaration* semantic_checker_create_struct(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous)
{
    QualifiedType type = (QualifiedType)
    {
        QUALIFIER_NONE,
        type_create_struct(&sc->ast->ast_allocator)
    };
    Declaration* decl = declaration_create_struct(&sc->ast->ast_allocator,
            enum_location, name, type);
    type_struct_set_declaration(type.type, decl);

    if (!anonymous)
    {
        semantic_checker_insert_tag(sc, decl);
    }
    
    return decl;
}

static Declaration* semantic_checker_create_union(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous)
{
    QualifiedType type = (QualifiedType)
    {
        QUALIFIER_NONE,
        type_create_union(&sc->ast->ast_allocator)
    };
    Declaration* decl = declaration_create_union(&sc->ast->ast_allocator,
            enum_location, name, type);
    type_union_set_declaration(type.type, decl);

    if (!anonymous)
    {
        semantic_checker_insert_tag(sc, decl);
    }
    
    return decl;
}

static Declaration* sematantic_checker_create_tag(SemanticChecker* sc,
        DeclarationType type, Location tag_type_loc, Identifier* identifier,
        Location identifier_loc)
{
    bool anonymous = (identifier == NULL);
    Identifier* tag_name = !anonymous
                ? identifier
                : identifier_table_get(sc->identifiers, "<anonymous>");
    Location decl_location = !anonymous ? identifier_loc : tag_type_loc;

    switch (type)
    {
        case DECLARATION_ENUM:
            return semantic_checker_create_enum(sc, decl_location, tag_name,
                    anonymous);

        case DECLARATION_STRUCT:
            return semantic_checker_create_struct(sc, decl_location, tag_name,
                    anonymous);

        case DECLARATION_UNION:
            return semantic_checker_create_union(sc, decl_location, tag_name,
                    anonymous);

        default:
            panic("bad tag type in semantic_checker_create_tag");
            return NULL;
    }
}

Declaration* semantic_checker_handle_tag(SemanticChecker* sc,
        DeclarationType type, Location tag_type_loc, Identifier* identifier,
        Location identifier_location, bool is_definition)
{
    // If we know it's a definition, don't look it up recursively, otherwise
    // do so since we will want to get a previous declaration.
    Declaration* previous = semantic_checker_lookup_tag(sc, identifier,
            !is_definition);

    // Make sure that the tag types match. If the tag types don't match then
    // we want to instead create an anonymous declaration instead.
    // NOTE: if identifier was null previous == NULL
    if (previous != NULL && !declaration_is(previous, type))
    {
        diagnostic_error_at(sc->dm, identifier_location, "use of '%s' with tag"
                " type that does not match previous declaration",
                identifier->string.ptr);

        identifier = NULL;
        identifier_location = LOCATION_INVALID;
        previous = NULL;
    }
    
    Declaration* declaration = NULL;
    if (!is_definition && previous == NULL)
    {
        // Create the implicit tag and make a warning about the implicit decl if
        // it is an enum declaration.
        declaration =  sematantic_checker_create_tag(sc, type, tag_type_loc,
                identifier, identifier_location);
        if (type == DECLARATION_ENUM && identifier != NULL)
        {
            diagnostic_warning_at(sc->dm, declaration->base.location,
                    "ISO C forbids forward references to 'enum' types");
        }
    }
    else if (!is_definition && previous != NULL)
    {
        declaration = previous;
    }
    else if (is_definition && previous == NULL)
    {
        // Can simply create the tag with no worries.
        declaration = sematantic_checker_create_tag(sc, type, tag_type_loc,
                identifier, identifier_location);
    }
    else if (is_definition && previous != NULL)
    {
        bool complete = false;
        switch (type)
        {
            case DECLARATION_ENUM:
                complete = declaration_enum_has_entries(previous);
                break;

            case DECLARATION_STRUCT:
            case DECLARATION_UNION:
                complete = declaration_struct_is_complete(previous);
                break;
        }

        // If we're not complete that's fine since we are about to give
        // the tag declaration a definition
        if (!complete)
        {
            declaration = previous;
        }
        else
        {
            // Otherwise we have a redefinition
            diagnostic_error_at(sc->dm, identifier_location,
                    "redefinition of %s '%s'", tag_kind_to_name(type),
                    identifier->string.ptr);
            
            // Make all of the current stuff blank so we can create a new tag
            identifier = NULL;
            identifier_location = LOCATION_INVALID;
            previous = NULL;

            // Can simply create the tag with no worries.
            declaration = sematantic_checker_create_tag(sc, type, tag_type_loc,
                    identifier, identifier_location);
        }
    }
    else
    {
        panic("unreachable");
    }
    assert(declaration != NULL);

    return declaration;
}

// -----------------------------------------------------------------------------
// End functions for handling declarations
// -----------------------------------------------------------------------------
// =============================================================================
// -----------------------------------------------------------------------------
// Start functions for handling labels
// -----------------------------------------------------------------------------

void sematic_checker_push_function_scope(SemanticChecker* sc,
        FunctionScope* function)
{
    assert(!sc->function && "should not have a function scope");

    sc->function = function;
}

void sematic_checker_pop_function_scope(SemanticChecker* sc)
{
    assert(sc->function && "should have a function scope");

    sc->function = NULL;
}

Declaration* semantic_checker_lookup_label(SemanticChecker* sc,
        Identifier* name)
{
    return function_scope_lookup(sc->function, name, true);
}

Declaration* semantic_checker_act_on_label(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location)
{
    // Attempt to look up the current declaration from the function scope;
    Declaration* current = function_scope_lookup(sc->function, identifier,
            true);

    // If we couldn't find any definition of the label then create and insert
    // the label into the current function scope.
    if (!current)
    {
        Declaration* declaration = declaration_create_label(
                &sc->ast->ast_allocator, identifier, identifier_location,
                false);
        function_scope_insert(sc->function, declaration);
        return declaration;
    }

    // If we're not implicit that means we've seen the label definition before
    // This is a problem so error and return
    if (!current->base.implicit)
    {
        diagnostic_error_at(sc->dm, identifier_location,
                "redefinition of label '%s'", identifier->string.ptr);
        // TODO: add note for original location?
        return NULL;
    }

    // If we're here we have seen the label before but only from one or more
    // goto's. Since we have finally seen the proper definition we want to
    // overwrite some of the current values
    current->base.implicit = false;
    current->base.location = identifier_location;

    return current;
}

Declaration* semantic_checker_act_on_goto(SemanticChecker* sc,
        Identifier* identifier, Location identifier_location)
{
    // If we already have the label in the function scope return that
    Declaration* decl = function_scope_lookup(sc->function, identifier, true);
    if (decl)
    {
        return decl;
    }

    decl = declaration_create_label(&sc->ast->ast_allocator, identifier,
            identifier_location, true);
    function_scope_insert(sc->function, decl);

    return decl;
}

void sematic_checker_act_on_end_of_function(SemanticChecker* sc)
{
    for (size_t i = 0; i < declaration_vector_size(&sc->function->used_labels);
            i++)
    {
        // For each declaration lookup the label in the function scope only
        // allowing for actual label definitions. Then we can error about ones
        // which were not declared.
        Declaration* decl = declaration_vector_get(&sc->function->used_labels,
                i);
        if (!function_scope_lookup(sc->function, decl->base.identifier, false))
        {
            diagnostic_error_at(sc->dm, decl->base.location,
                    "use of undeclared label '%s'",
                    decl->base.identifier->string.ptr);
        }

        // TODO: warn about declared but unused labels?
    }
}

// -----------------------------------------------------------------------------
// End functions for handling labels
// -----------------------------------------------------------------------------
// =============================================================================
// -----------------------------------------------------------------------------
// Start functions for expressions
// -----------------------------------------------------------------------------


// TODO: make some functions for lvalues and stuff

// TODO: make functions for promoting integers, array to pointer conversion, and
// TODO: anything else that we might need to use in order to properly handle
// TODO: expression types.

// TODO: need to have some lvalue to rvalue conversion taking place. and also
// TODO: need to have integer conversion to appropriate types taking place

static Expression* semantic_checker_create_implicit_cast(SemanticChecker* sc,
        Expression* expression, QualifiedType cast_to)
{
    return expression_create_implicit_cast(&sc->ast->ast_allocator, cast_to,
            expression);
}

static Expression* semantic_checker_cast_to_void(SemanticChecker* sc,
        Expression* expression)
{
    QualifiedType void_type = semantic_checker_get_void_type(sc);
    return semantic_checker_create_implicit_cast(sc, expression, void_type);
}

static Expression* semantic_checker_promote_integer(SemanticChecker* sc,
        Expression* expression)
{
    // Get the current type
    QualifiedType type = expression_get_qualified_type(expression);
    if (!qualified_type_is_integer(&type))
    {
        return expression;
    }
    assert(qualified_type_is_integer(&type));

    QualifiedType real_type = qualified_type_get_canonical(&type);
    switch (qualified_type_get_kind(&real_type))
    {
        // These are all of the types that will need to be promoted if 
        // arithmetic is performed on them.
        case TYPE_BOOL:
        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_U_CHAR:
        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
        {
            // Create a case to the integer type. Since we know that all of the
            // above types can be represented by a plain integer so WE will 
            // never need to promote to unsigned integer.
            // NOTE: this may not be true for other targets if we ever do that.
            return semantic_checker_create_implicit_cast(sc, expression,
                    semantic_checker_get_int_type(sc));
        }

        // We have an integer type with no conversion to a larger type needed so
        // we can just return the current expression to the user.
        default: return expression;
    }

    panic("uncreachable");
    return NULL;
}

static Expression* semantic_checker_decay_expression_type(SemanticChecker* sc,
        Expression* expression)
{
    // Ignore invalid expression
    if (expression_is_invalid(expression))
    {
        return expression;
    }

    // Get the type of the expression that we might want to decay
    QualifiedType type = expression_get_qualified_type(expression);

    // Note: we can only get an array type if we have an lvalue
    if (qualified_type_is(&type, TYPE_ARRAY))
    {
        // Ignore the parenthesis and see if we get a register expression to
        // check for the possibility of having a register array
        Expression* no_parens = expression_ignore_parenthesis(expression);
        if (expression_is(no_parens, EXPRESSION_REFERENCE))
        {
            Declaration* ref = expression_reference_get_decl(no_parens);
            if (declaration_get_storage_class(ref) == STORAGE_REGISTER)
            {
                diagnostic_error_at(sc->dm, expression_get_location(no_parens),
                        "address of register variable requested");
                return semantic_checker_handle_error_expression(sc,
                        expression_get_location(expression));
            }
        }
    }

    // Attempt to decay the type noting if it got decayed or not
    bool decayed;
    QualifiedType decayed_type = semantic_checker_decay_type(sc, type,
            &decayed);

    // If we didn't get decayed return the expression as is. No changes are
    // needed to the expression
    if (!decayed)
    {
        return expression;
    }

    // Otherwise if we were decayed create an implicit cast expression to the
    // new decayed type. e.g. int[3] -> int *
    return semantic_checker_create_implicit_cast(sc, expression, decayed_type);
}

static bool type_is_subscriptable(const QualifiedType* type)
{
    if (qualified_type_is(type, TYPE_POINTER))
    {
        return true;
    }
    else if (qualified_type_is(type, TYPE_ARRAY))
    {
        return true;
    }

    return false;
}

static QualifiedType get_inner_type(const QualifiedType* type)
{
    assert(type_is_subscriptable(type));

    if (qualified_type_is(type, TYPE_POINTER))
    {
        return type_pointer_get_pointee(type);
    }
    else
    {
        return type_array_get_element_type(type);
    }
}

// Tye value kind of an expression. I.e. is it an lvalue, rvalue, etc...
typedef enum ExpressionValueKind {
    VALUE_KIND_LVALUE,
    VALUE_KIND_MODIFIABLE_LVALUE,
    VALUE_KIND_RVALUE,
    VALUE_KIND_FUNCTION_DESIGNATOR
} ExpressionValueKind;

static bool expression_value_kind_is_lvalue(ExpressionValueKind kind)
{
    return kind == VALUE_KIND_LVALUE || kind == VALUE_KIND_MODIFIABLE_LVALUE;
}

static ExpressionValueKind expression_classify(SemanticChecker* sc,
        const Expression* expression)
{
    QualifiedType expr_type = expression_get_qualified_type(expression);
    QualifiedType real_type = qualified_type_get_canonical(&expr_type);

    // If it is not an object type then we have a function.
    if (!qualifier_type_is_object_type(&real_type))
    {
        return VALUE_KIND_FUNCTION_DESIGNATOR;
    }

    if (qualified_type_is(&real_type, TYPE_VOID))
    {
        return VALUE_KIND_RVALUE;
    }

    ExpressionValueKind kind;
    switch (expression_get_kind(expression))
    {
        case EXPRESSION_ERROR:

        case EXPRESSION_ENUMERATION_CONSTANT:
        case EXPRESSION_INTEGER_CONSTANT:    
        case EXPRESSION_FLOATING_CONSTANT:
        case EXPRESSION_CHARACTER_CONSTANT:

        case EXPRESSION_FUNCTION_CALL:

        case EXPRESSION_CAST:

        case EXPRESSION_UNARY_PLUS:
        case EXPRESSION_UNARY_MINUS:
        case EXPRESSION_UNARY_BIT_NOT:
        case EXPRESSION_UNARY_NOT:

        case EXPRESSION_UNARY_PRE_INCREMENT:
        case EXPRESSION_UNARY_PRE_DECREMENT:
        case EXPRESSION_UNARY_POST_INCREMENT:
        case EXPRESSION_UNARY_POST_DECREMENT:

        case EXPRESSION_SIZEOF_TYPE:
        case EXPRESSION_SIZEOF_EXPRESSION:

        case EXPRESSION_BINARY_TIMES:
        case EXPRESSION_BINARY_DIVIDE:
        case EXPRESSION_BINARY_MODULO:
        case EXPRESSION_BINARY_ADD:
        case EXPRESSION_BINARY_SUBTRACT:
        case EXPRESSION_BINARY_SHIFT_LEFT:
        case EXPRESSION_BINARY_SHIFT_RIGHT:
        case EXPRESSION_BINARY_LESS_THAN:
        case EXPRESSION_BINARY_GREATER_THAN:
        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
        case EXPRESSION_BINARY_EQUAL:
        case EXPRESSION_BINARY_NOT_EQUAL:
        case EXPRESSION_BINARY_AND:
        case EXPRESSION_BINARY_XOR:
        case EXPRESSION_BINARY_OR:
        case EXPRESSION_BINARY_LOGICAL_AND:
        case EXPRESSION_BINARY_LOGICAL_OR:

        case EXPRESSION_BINARY_ASSIGN:
        case EXPRESSION_BINARY_TIMES_ASSIGN:
        case EXPRESSION_BINARY_DIVIDE_ASSIGN:
        case EXPRESSION_BINARY_MODULO_ASSIGN:
        case EXPRESSION_BINARY_ADD_ASSIGN:
        case EXPRESSION_BINARY_SUBTRACT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN:
        case EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN:
        case EXPRESSION_BINARY_AND_ASSIGN:
        case EXPRESSION_BINARY_XOR_ASSIGN:
        case EXPRESSION_BINARY_OR_ASSIGN:

        case EXPRESSION_CONDITIONAL:

        case EXPRESSION_COMMA:
            return VALUE_KIND_RVALUE;

        // The address of expression is not itself an lvalue.
        case EXPRESSION_UNARY_ADDRESS:

        // TODO: I'm not sure if this is correct
        case EXPRESSION_CAST_IMPLICIT:
            return VALUE_KIND_RVALUE;

        case EXPRESSION_REFERENCE:
        case EXPRESSION_ARRAY_ACCESS:
        case EXPRESSION_STRING_LITERAL:
        case EXPRESSION_COMPOUND_LITERAL:

        case EXPRESSION_MEMBER_POINTER_ACCESS:            

        case EXPRESSION_UNARY_DEREFERENCE: // If applied to a pointer to object
            kind = VALUE_KIND_LVALUE;
            break;

        // Special case, this is only a lvalue if the left hand side is
        case EXPRESSION_MEMBER_ACCESS:
        {
            Expression* lhs = expression_member_access_get_lhs(expression);
            
            // TODO: is this correct? Or should I be classifying the expression
            // TODO: and not the LHS
            return expression_classify(sc, lhs);   
        }

        case EXPRESSION_PARENTHESISED:
        {
            Expression* inner = expression_parenthesised_get_inner(expression);
            return expression_classify(sc, inner);
        }

        case EXPRESSION_ARRAY_DECAY:
        {
            Expression* inner = expression_array_decay_get_inner(expression);
            return expression_classify(sc, inner);
        }

        case EXPRESSION_LVALUE_CAST:
        {
            Expression* inner = expression_lvalue_cast_get_inner(expression);
            return expression_classify(sc, inner);
        }

        // TODO: this could possible get triggered by implicit cast but im not
        // TODO: too sure yet
        default:
            panic("cannot classify expression kind; unimplemented");
            return VALUE_KIND_RVALUE;
    }

    // Should only try to check for modifiable lvalues if we have an lvalue
    assert(kind == VALUE_KIND_LVALUE);

    // TODO: there is more to it than this but this will work for now...
    if (!type_qualifier_is_const(expr_type.qualifiers))
    {
        return VALUE_KIND_MODIFIABLE_LVALUE;
    }
    
    // // TODO: check if expression is modifiable lvalue or not (below is wrong)
    // if (kind == VALUE_KIND_LVALUE)
    // {
    //     return VALUE_KIND_MODIFIABLE_LVALUE;
    // }

    // Okay now we know we have an lvalue type, but we want to be able to know 
    // if it is modifiable or not.

    return kind;
}

static bool expression_is_modifiable_lvalue(const Expression* expression)
{
    ExpressionValueKind kind = expression_classify(NULL, expression);

    // Get the real type
    QualifiedType type = expression_get_qualified_type(expression);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Must be complete type
    if (!qualified_type_is_complete(&real_type))
    {
        return false;
    }
    
    // Must not be an array type
    if (qualified_type_is(&real_type, TYPE_ARRAY))
    {
        return false;
    }

    // Must not be const qualified
    TypeQualifiers qualifiers = real_type.qualifiers;
    if (type_qualifier_is_const(qualifiers))
    {
        return false;
    }

    // If its a struct / union must not have any const qualified members
    // (recursively)
    if (qualified_type_is(&real_type, TYPE_STRUCT) ||
            qualified_type_is(&real_type, TYPE_UNION))
    {
        panic("struct / union modifiable lvalue not implemented");
    }

    return true;
}

static Expression* semantic_checker_lvalue_to_rvalue(SemanticChecker* sc,
        Expression* expression)
{
    if (expression_is_invalid(expression))
    {
        return expression;
    }

    ExpressionValueKind kind = expression_classify(sc, expression);

    // If we are not an lvalue or an rvalue make sure to just return the
    // expression as is.
    if (!expression_value_kind_is_lvalue(kind))
    {
        return expression;
    }

    // Get the canonical type and remove the qualifiers to convert from lvalue
    // to an rvalue
    QualifiedType type = expression_get_qualified_type(expression);
    type = qualified_type_get_canonical(&type);

    QualifiedType unqualed = qualified_type_remove_quals(&type);
    return expression_create_lvalue_cast(&sc->ast->ast_allocator, expression,
            unqualed);
}

static Expression* semantic_checker_func_array_lvalue_convert(
        SemanticChecker* sc, Expression* expr)
{
    // Check each time before we attempt anything that the expression is valid
    if (expression_is_invalid(expr))
    {
        return expr;
    }
    expr = semantic_checker_decay_expression_type(sc, expr);

    if (expression_is_invalid(expr))
    {
        return expr;
    }
    expr = semantic_checker_lvalue_to_rvalue(sc, expr);

    return expr;
}

Expression* semantic_checker_handle_error_expression(SemanticChecker* sc,
        Location location)
{
     return expression_create_error(&sc->ast->ast_allocator,
                sc->ast->base_types.type_error, location);
}

Expression* semantic_checker_handle_parenthesis_expression(SemanticChecker* sc,
        Location lparen_location, Expression* inner, Location rparen_location)
{
    Expression* expr = expression_create_parenthesised(&sc->ast->ast_allocator,
            lparen_location, rparen_location, inner);

    // Set the new expression to be invalid if the inner expression was to
    // poison any new ast nodes that get created from it
    if (expression_is_invalid(inner))
    {
        expression_set_invalid(expr);
    }

    return expr;
}

Expression* semantic_checker_handle_builtin_identifier(SemanticChecker* sc,
        Location location)
{
    // __func__ is not valid inside of a function. Both clang and gcc treat this
    // as a warning however and clang seems to act as if it is equal to the
    // empty string.
    if (sc->function == NULL)
    {
        diagnostic_error_at(sc->dm, location,
                "predefined identifier is only valid inside function");
        return semantic_checker_handle_error_expression(sc, location);
    }

    // TODO: need to create a new expression type for this...
    QualifiedType type;
    return semantic_checker_handle_error_expression(sc, location);
}

Expression* semantic_checker_handle_reference_expression(SemanticChecker* sc,
        Location identifier_location, Identifier* identifier,
        bool is_function_call)
{
    // Attempt to get the declaration that this corrosponds to.
    Declaration* declaration = semantic_checker_lookup_ordinairy(sc,
            identifier, true);

    if (declaration == NULL)
    {
        const char* context = !is_function_call
                ? "use of undeclared identifier '%s'"
                : "call to undeclared function '%s'";
        diagnostic_error_at(sc->dm, identifier_location, context,
                identifier->string.ptr);

        // TODO: handle builting function giving it type int() and putting the
        // TODO: declaration at the identifier location.
        return semantic_checker_handle_error_expression(sc,
                identifier_location);
    }
    else if (declaration_is(declaration, DECLARATION_TYPEDEF))
    {
        diagnostic_error_at(sc->dm, identifier_location,
                "unexpected type name '%s': expected expression",
                identifier->string.ptr);
        return semantic_checker_handle_error_expression(sc,
                identifier_location);
    }
    else if (declaration_is(declaration, DECLARATION_ENUM_CONSTANT))
    {
        return expression_create_enum_constant(&sc->ast->ast_allocator,
                identifier, identifier_location, declaration,
                semantic_checker_get_int_type(sc));
    }

    // Here we should either be a variable or a function
    assert(declaration_is(declaration, DECLARATION_VARIABLE)
            || declaration_is(declaration, DECLARATION_FUNCTION));

    // Get the type of the expression and create it
    QualifiedType type = declaration_get_type(declaration);
    Expression* reference = expression_create_reference(&sc->ast->ast_allocator,
            identifier, identifier_location, declaration, type);
    
    // If the declaration was not valid then set the expression to be invalid as
    // well to help cascading errors.
    if (!declaration_is_valid(declaration))
    {
        expression_set_invalid(reference);
    }

    // Except when it is the operand of the sizeof operator or the unary & 
    // operator, or is a string literal used to initialize an array, an 
    // expression that has type ‘array of type’ is converted to an expression
    // with type ‘pointer to type’ that points to the initial element of the
    // array object and is not an lvalue. If the array object has register 
    // storage class, the behavior is undefined

    return reference;
}

static Expression* semantic_checker_handle_integer_constant(SemanticChecker* sc,
        Location integer_location, IntegerValue value)
{
    Type* type = NULL;
    switch (value.type)
    {
        case INTEGER_VALUE_ERROR:
            panic("unreachable");
            break;

        case INTEGER_VALUE_INTEGER:
            type = sc->ast->base_types.type_signed_int;
            break;

        case INTEGER_VALUE_UNSIGNED_INTEGER:
            type = sc->ast->base_types.type_unsigned_int;
            break;

        case INTEGER_VALUE_LONG:
            type = sc->ast->base_types.type_signed_long;
            break;

        case INTEGER_VALUE_UNSIGNED_LONG:
            type = sc->ast->base_types.type_unsigned_long;
            break;

        case INTEGER_VALUE_LONG_LONG:
            type = sc->ast->base_types.type_signed_long_long;
            break;

        case INTEGER_VALUE_UNSIGNED_LONG_LONG:
            type = sc->ast->base_types.type_unsigned_long_long;
            break;
    }

    QualifiedType qual_type = {QUALIFIER_NONE, type};
    return expression_create_integer(&sc->ast->ast_allocator, integer_location,
            value, qual_type);
}

static Expression* semantic_checker_handle_floating_constant(SemanticChecker* sc,
        Location float_location, FloatingValue value)
{
    Type* type = NULL;
    switch (value.type)
    {
        case FLOATING_VALUE_ERROR:
            panic("unreachable");
            break;

        case FLOATING_VALUE_FLOAT:
            type = sc->ast->base_types.type_float;
            break;

        case FLOATING_VALUE_DOUBLE:
            type = sc->ast->base_types.type_double;
            break;

        case FLOATING_VALUE_LONG_DOUBLE:
            type = sc->ast->base_types.type_long_double;
            break;
    }

    QualifiedType qual_type = {QUALIFIER_NONE, type};
    return expression_create_float(&sc->ast->ast_allocator, float_location,
            value, qual_type);
}

Expression* semantic_checker_handle_number_expression(SemanticChecker* sc,
        Location number_location, LiteralValue value, bool success)
{
    if (!success)
    {
        return semantic_checker_handle_error_expression(sc, number_location);
    }

    ValueType type = literal_value_get_type(&value);

    if (type == VALUE_INTEGER_TYPE)
    {
        return semantic_checker_handle_integer_constant(sc, number_location,
                value.value.integer);
    }
    else
    {
        return semantic_checker_handle_floating_constant(sc, number_location,
                value.value.floating);
    }
}

Expression* semantic_checker_handle_char_expression(SemanticChecker* sc,
        Location char_location, CharValue value, bool success)
{
    if (!success)
    {
        return semantic_checker_handle_error_expression(sc, char_location);
    }

    // TODO: is this correct if the char is wide?
    Type* type = sc->ast->base_types.type_signed_int;
    QualifiedType qual_type = {QUALIFIER_NONE, type};
    return expression_create_character(&sc->ast->ast_allocator, char_location,
            value, qual_type);
}

// TODO: string literals

Expression* semantic_checker_handle_array_expression(SemanticChecker* sc,
        Expression* lhs, Location lbracket_loc, Expression* member,
        Location rbracket_loc)
{    
    if (expression_is_invalid(lhs) || expression_is_invalid(member))
    {
        return semantic_checker_handle_error_expression(sc, lbracket_loc);
    }

    // TODO: I almost certainly want to handle this whole situation differently
    // TODO: should involve the decaying of the expressions to their decayed
    // TODO: type

    // TODO: I probably do care about the decayed type here?
    QualifiedType type_lhs = expression_get_qualified_type(lhs);
    QualifiedType decayed_lhs = semantic_checker_decay_type(sc, type_lhs, NULL);

    // TODO: I probably do care about the decayed type here?
    QualifiedType type_member = expression_get_qualified_type(member);
    QualifiedType decayed_rhs = semantic_checker_decay_type(sc, type_member,
            NULL);

    // Make sure that one of our types is an array or pointer
    bool lhs_is_array = type_is_subscriptable(&decayed_lhs);
    bool rhs_is_array = type_is_subscriptable(&decayed_rhs);
    if (!lhs_is_array && !rhs_is_array)
    {
        diagnostic_error_at(sc->dm, lbracket_loc,
                "subscripted value is not an array, or pointer");
        return semantic_checker_handle_error_expression(sc, lbracket_loc);
    }

    // If we have determined the lhs is the 'array' side then we want to check
    // that the rhs is an integer, otherwise check the lhs is an integer
    Expression* check_integer = lhs_is_array ? member : lhs;
    QualifiedType subscript_type = lhs_is_array ? decayed_rhs : decayed_lhs;

    if (!qualified_type_is_integer(&subscript_type))
    {
        diagnostic_error_at(sc->dm, lbracket_loc,
                "array subscript is not an integer");
        return semantic_checker_handle_error_expression(sc, lbracket_loc);
    }

    // Okay we have an expression we know is an array and an expression that
    // we know is a pointer. We just need to get the inner type of the once that
    // is an array.
    QualifiedType array_type = lhs_is_array ? decayed_lhs : decayed_rhs;
    QualifiedType expr_type = get_inner_type(&array_type);

    // Create the array expression remembering which side is the array side
    return expression_create_array(&sc->ast->ast_allocator,
            lbracket_loc, rbracket_loc, lhs, member, expr_type, lhs_is_array);
}

static bool semantic_checker_is_callable(SemanticChecker* sc,
        const Expression* expression)
{
    QualifiedType type = expression_get_qualified_type(expression);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Note: any callable object should have been correctly decayed to a pointer
    // to function type so no function types should exist. Although it may be
    // slower to decay then check it is easier to deal with and check we are
    // doing the correct thing
    assert(!qualified_type_is(&real_type, TYPE_FUNCTION));

    // Any callable expression should be of type 'pointer to function'
    if (qualified_type_is(&real_type, TYPE_POINTER))
    {
        QualifiedType inner = type_pointer_get_pointee(&real_type);
        return qualified_type_is(&inner, TYPE_FUNCTION);
    }

    return false;
}

// TODO: finish handling this call expression
Expression* semantic_checker_handle_call_expression(SemanticChecker* sc,
        Expression* lhs, Location lparen_location, Expression* expr_list,
        Location rparen_location)
{
    // Do nothing if we got an error
    if (expression_is_invalid(lhs))
    {
        return semantic_checker_handle_error_expression(sc, lparen_location);
    }

    // Decay the expression to it's corrosponding type
    lhs = semantic_checker_decay_expression_type(sc, lhs);

    if (!semantic_checker_is_callable(sc, lhs))
    {
        /* TODO: when type printing it should say "type '<type>'"*/
        diagnostic_error_at(sc->dm, lparen_location,
                "called object is not a function or function pointer");
        return semantic_checker_handle_error_expression(sc, lparen_location);
    }

    // TODO: we know we have a function and or function pointer so 

    // Now we know we have a function we want to attempt to check that the 
    // parameters are valid and the types that they should be.

    // Get the return type of the function
    QualifiedType pointer = expression_get_qualified_type(lhs);
    QualifiedType function_type = type_pointer_get_pointee(&pointer);
    QualifiedType real_type = qualified_type_get_canonical(&function_type);
    
    QualifiedType return_type = type_function_get_return(&function_type);
    return semantic_checker_handle_error_expression(sc, lparen_location);
}

static Declaration* find_member_declaration(QualifiedType* type,
        Identifier* identifier)
{
    Declaration* struct_decl = qualified_type_struct_get_declaration(type);
    DeclarationList decls = declaration_struct_get_members(struct_decl);

    DeclarationListEntry* current = declaration_list_iter(&decls);
    for (; current != NULL; current = declaration_list_next(current))
    {
        Declaration* member = declaration_list_entry_get(current);

        // Skip unnamed fields (e.g. int : 3)
        if (!declaration_has_identifier(member))
        {
            continue;
        }

        // Now we know that we have an identifier we must get it and compare it
        // to the identifier we are looking for.
        Identifier* member_name = declaration_get_identifier(member);
        if (member_name == identifier)
        {
            return member;
        }
    }

    return NULL;
}

Expression* semantic_checker_handle_member_expression(SemanticChecker* sc,
        Expression* lhs, Location operator_loc, Identifier* identifier,
        Location identifier_location, bool dot)
{
    if (expression_is_invalid(lhs))
    {
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    QualifiedType type = expression_get_qualified_type(lhs);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // If we're using an arrow get the base type from the type we are pointing
    // to. Otherwise, simple use the type as the base type.
    QualifiedType base_type;
    if (!dot)
    {
        if (!qualified_type_is(&real_type, TYPE_POINTER))
        {
            // Check if the base type is a compound type if so suggest the user
            // use a dot instead to (potentially) make this valid.
            bool is_base_compound = qualified_type_is_compound(&real_type);
            diagnostic_error_at(sc->dm, operator_loc,
                    "member reference type is not a pointer%s",
                    !is_base_compound ? "" : "; did you mean to use '.'?");
            if (is_base_compound)
            {
                base_type = type;
            }
            else 
            {
                return semantic_checker_handle_error_expression(sc,
                        operator_loc);
            }
        }
        else
        {
            base_type = type_pointer_get_pointee(&real_type);
        }
    }
    else
    {
        // Pre-emptively set the base type to the current type and we will do a
        // check to see if the user mistyped a '.' for a '->'
        base_type = type;

        // Check if we have a pointer to a compound type. Note if we have a
        // pointer then the top path will always be wrong no matter what
        if (qualified_type_is(&real_type, TYPE_POINTER))
        {
            QualifiedType pointee = type_pointer_get_pointee(&real_type);
            if (qualified_type_is_compound(&pointee))
            {
                diagnostic_error_at(sc->dm, operator_loc, "member reference "
                        "type is a pointer; did you mean to use '->'");
                base_type = pointee;
            }
        }
    }

    // Now check that the base type is a valid compound type
    if (!qualified_type_is_compound(&base_type))
    {
        diagnostic_error_at(sc->dm, operator_loc,
                "member reference base type is not a structure or union");
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Attmpet to find the declaration for the member
    Declaration* member = find_member_declaration(&base_type, identifier);
    if (member == NULL)
    {
        // Here is the bad case where we didn't find the appropriate member in
        // the structure or union so then we  need to error about it and return
        // an error expression
        Declaration* struct_decl = qualified_type_struct_get_declaration(
                &base_type);
        DeclarationType kind = declaration_get_kind(struct_decl);
        diagnostic_error_at(sc->dm, identifier_location,
                "no member named '%s' in '%s %s'", identifier->string.ptr,
                tag_kind_to_name(kind),
                declaration_get_identifier(struct_decl)->string.ptr);
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // The type of the expresion we are about to create is the type of the 
    // member that we are referencing.
    QualifiedType expression_type = declaration_get_type(member); 
    return expression_create_member_access(&sc->ast->ast_allocator,
            operator_loc, lhs, member, expression_type, dot);
}

Expression* semantic_checker_handle_increment_expression(SemanticChecker* sc,
        ExpressionType type, Expression* expression, Location operator_loc)
{
    assert(type == EXPRESSION_UNARY_PRE_INCREMENT ||
            type == EXPRESSION_UNARY_POST_INCREMENT ||
            type == EXPRESSION_UNARY_PRE_DECREMENT ||
            type == EXPRESSION_UNARY_POST_DECREMENT);

    // Ignore if invalid expression
    if (expression_is_invalid(expression))
    {
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Expression firstly needs to be an lvalue
    ExpressionValueKind kind = expression_classify(sc, expression);
    if (!expression_value_kind_is_lvalue(kind))
    {
        diagnostic_error_at(sc->dm, operator_loc,
                "expression is not assignable");
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Set increment if were are ++a or a++
    bool increment = (type == EXPRESSION_UNARY_POST_INCREMENT
            || type == EXPRESSION_UNARY_PRE_INCREMENT) ? true : false;
        
    // Now check if we are actually a type that we are allowed to increment or
    // decrement. If we are not produce an error
    QualifiedType expr_type = expression_get_qualified_type(expression);
    QualifiedType real_type = qualified_type_get_canonical(&expr_type);
    if (qualified_type_is(&real_type, TYPE_POINTER))
    {
        QualifiedType pointee = type_pointer_get_pointee(&real_type);
        if (!qualified_type_is_complete(&pointee))
        {
            diagnostic_error_at(sc->dm, operator_loc,
                    "arithmetic on a pointer to an imcomplete type");
            return semantic_checker_handle_error_expression(sc, operator_loc);
        }
    }
    else if (!qualified_type_is_integer(&real_type))
    {   
        // TODO: add 'of type ...'
        diagnostic_error_at(sc->dm, operator_loc, "cannot %s value",
                increment ? "increment" : "decrement");
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Then check that the expression type is actually modifiable. We know that
    // we have an lvalue with integer type at this point
    if (kind != VALUE_KIND_MODIFIABLE_LVALUE)
    {
        // TODO: the clang error messages here get more specific if we know
        // we have a variable and what not. Maybe implement that way later?
        diagnostic_error_at(sc->dm, operator_loc,
                "read-only variable is not assignable");
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Get the type of the expression and remove any qualifiers that we might
    // have. Note that we check that the current type itself cannot be const
    // since that would be quite bad.
    QualifiedType qual_type = expression_get_qualified_type(expression);
    assert(!type_qualifier_is_const(qual_type.qualifiers));

    qual_type = qualified_type_remove_quals(&qual_type);
    return expression_create_unary(&sc->ast->ast_allocator, type, operator_loc,
            expression, qual_type);
}

Expression* semantic_checker_handle_unary_expression(SemanticChecker* sc,
        ExpressionType type, Location operator_loc, Expression* rhs)
{
    // Do nothing if we have an invalid expression
    if (expression_is_invalid(rhs))
    {
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Convert from lvalue to rvalue
    rhs = semantic_checker_func_array_lvalue_convert(sc, rhs);

    // also promote the right hand side of the expression if needed
    rhs = semantic_checker_promote_integer(sc, rhs);

    // The fucntion we will use to check the type of the right hand side and the
    // expr type to add context to the message
    bool (*checking_func)(const QualifiedType*) = NULL;
    const char* expr_type = NULL;
    switch (type)
    {
        // These 2 should have arithmetic type
        case EXPRESSION_UNARY_PLUS:
            checking_func = qualified_type_is_arithmetic;
            expr_type = "unary plus";
            break;

        case EXPRESSION_UNARY_MINUS:
            checking_func = qualified_type_is_arithmetic;
            expr_type = "unary minus";
            break;

        // This should have integer type
        case EXPRESSION_UNARY_BIT_NOT:
            checking_func = qualified_type_is_integer;
            expr_type = "unary bit-complement";
            break;

        // This should have scalar type
        case EXPRESSION_UNARY_NOT:
            checking_func = qualified_type_is_scaler;
            expr_type = "unary exclamation mark";
            break;

        default:
            panic("unexpected unary expression type");
            return semantic_checker_handle_error_expression(sc, operator_loc);
    }
    assert(checking_func);
    assert(expr_type);

    // Get the rhs type and see if we have an error
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    if (!checking_func(&rhs_type))
    {
        // TODO: add the name of the type after the word 'type' e.g. type '%s'
        diagnostic_error_at(sc->dm, operator_loc,
                "invalid argument type to %s expression", expr_type);
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // The result of a unary expression in all cases is the type of it's
    // promoted operand unless it is the not operator, in which case it's type
    // is automatically int 
    QualifiedType return_type = rhs_type;
    if (type == EXPRESSION_UNARY_NOT)
    {
        return_type = semantic_checker_get_int_type(sc);
    }

    return expression_create_unary(&sc->ast->ast_allocator, type, operator_loc,
            rhs, return_type);
}

// Enum to represent the different failure modes of the address expression.
// This is to help provide more accurate messages when diagnosing issues.
typedef enum AddressableKind {
    ADDRESSABLE_OK, // Okay to take address of
    ADDRESSABLE_ERR_RVALUE, // cannot take address of rvalue
    ADDRESSABLE_ERR_REGISTER, // cannot take address of register variable
    ADDRESSABLE_ERR_BITFIELD // cannot take address of bitfield
} AddressableKind;

static AddressableKind check_expression_addressable(SemanticChecker* sc,
        Expression* expression)
{
    // First classify the expression to 
    ExpressionValueKind vk = expression_classify(sc, expression);

    // First check if we have an rvalue and error since that will always be
    // invalid to take the address of that.
    if (vk == VALUE_KIND_RVALUE)
    {
        return ADDRESSABLE_ERR_RVALUE;
    }

    // If we're a function designator we can simply return the expression of
    // pointer to the function type.
    if (vk == VALUE_KIND_FUNCTION_DESIGNATOR)
    {
        return ADDRESSABLE_OK;
    }

    Expression* no_parens = expression_ignore_parenthesis(expression);
    if (expression_is(no_parens, EXPRESSION_UNARY_DEREFERENCE)
            || expression_is(no_parens, EXPRESSION_ARRAY_ACCESS))
    {
        return ADDRESSABLE_OK;
    }

    // Can be an lvalue but must no designate an object that is a bitfield and
    // cannot be declared with the register specifier
    if (expression_value_kind_is_lvalue(vk))
    {
        if (expression_is(no_parens, EXPRESSION_REFERENCE))
        {
            Declaration* decl = expression_reference_get_decl(no_parens);
            if (declaration_get_storage_class(decl) == STORAGE_REGISTER)
            {
                return ADDRESSABLE_ERR_REGISTER;
            }
        }
        else if (expression_is(no_parens, EXPRESSION_MEMBER_ACCESS)
                || expression_is(no_parens, EXPRESSION_MEMBER_POINTER_ACCESS))
        {
            Declaration* decl = expression_member_access_get_decl(no_parens);
            if (declaration_field_has_bitfield(decl))
            {
                return ADDRESSABLE_ERR_BITFIELD;
            }

            // Finally check the lhs most side is addressable and simply return
            // that. This is to handle the cases of nested struct access as that
            // is not handled by any of the above code.
            Expression* lhs = expression_member_access_get_most_lhs(no_parens);
            return check_expression_addressable(sc, lhs);
        }
    }

    return ADDRESSABLE_OK;
}

Expression* semantic_checker_handle_address_expression(SemanticChecker* sc,
        Expression* rhs, Location and_location)
{
    // Ignore invalid expressions.
    if (expression_is_invalid(rhs))
    {
        return semantic_checker_handle_error_expression(sc, and_location);
    }
    
    // he operand of the unary & operator shall be either a function designator,
    // the result of a [] or unary * operator, or an lvalue that designates an
    // object that is not a bit-field and is not declared with the register
    // storage-class specifier

    AddressableKind ak = check_expression_addressable(sc, rhs);
    if (ak != ADDRESSABLE_OK)
    {
        const char* msg = NULL;
        switch (ak)
        {
            case ADDRESSABLE_ERR_RVALUE:
                msg = "cannot take the address of an rvalue";
                break;

            case ADDRESSABLE_ERR_REGISTER:
                msg = "address of register variable requested";
                break;
            
            case ADDRESSABLE_ERR_BITFIELD:
                msg = "address of bit-field requested";
                break;

            default:
                panic("unexpected addressable kind");
                break;
        }

        diagnostic_error_at(sc->dm, and_location, msg);
        return semantic_checker_handle_error_expression(sc, and_location);
    }

    // Create the type of pointer to (expression type)
    QualifiedType expr_type = expression_get_qualified_type(rhs);
    QualifiedType pointer = semantic_checker_create_pointer(sc, expr_type,
            QUALIFIER_NONE);

    // Finally create the unary expression
    return expression_create_unary(&sc->ast->ast_allocator,
            EXPRESSION_UNARY_ADDRESS, and_location, rhs, pointer);
}

Expression* semantic_checker_handle_dereference_expression(SemanticChecker* sc,
        Expression* rhs, Location star_location)
{
    // First decay the expression since we know that derefence is valid if an
    // expresion has pointer type. if the expression is invalid nothing will
    // happen
    rhs = semantic_checker_decay_expression_type(sc, rhs);
    
    // Check for invalid expressions since we want to prevent cascading errors
    if (expression_is_invalid(rhs))
    {
        return semantic_checker_handle_error_expression(sc, star_location);
    }

    // If it is not a pointer type then we have a problem
    QualifiedType type = expression_get_qualified_type(rhs);
    type = qualified_type_get_canonical(&type);
    if (!qualified_type_is(&type, TYPE_POINTER))
    {
        diagnostic_error_at(sc->dm, star_location,
                "indirection requires pointer operand");
        return semantic_checker_handle_error_expression(sc, star_location);
    }

    // Okay we have checked the only possible constraint. So all we need to do
    // here is get the pointee type and create the expression.
    QualifiedType pointee = type_pointer_get_pointee(&type);
    return expression_create_unary(&sc->ast->ast_allocator,
            EXPRESSION_UNARY_DEREFERENCE, star_location, rhs, pointee);
}

Expression* semantic_checker_handle_sizeof_type_expression(SemanticChecker* sc,
        Location sizeof_location, Location lparen_loc, QualifiedType type,
        Location rparen_loc)
{
    // Get the real type and make sure that it is complete
    QualifiedType real_type = qualified_type_get_canonical(&type);
    if (!qualified_type_is_complete(&real_type))
    {
        diagnostic_error_at(sc->dm, sizeof_location,
                "invalid application of 'sizeof' to an incomplete type");
        return semantic_checker_handle_error_expression(sc, sizeof_location);
    }

    // Get the standard size type for the value of the expression
    QualifiedType size_type = semantic_checker_get_size_type(sc);
    
    return NULL;
}

Expression* semantic_checker_handle_sizeof_expression(SemanticChecker* sc,
        Location sizeof_location, Expression* expression)
{
    if (expression_is_invalid(expression))
    {
        return semantic_checker_handle_error_expression(sc, sizeof_location);
    }

    QualifiedType type = expression_get_qualified_type(expression);
    QualifiedType real_type = qualified_type_get_canonical(&type);
    if (!qualified_type_is_complete(&real_type))
    {
        diagnostic_error_at(sc->dm, sizeof_location,
                "invalid application of 'sizeof' to incomplete type");
        return semantic_checker_handle_error_expression(sc, sizeof_location);
    }

    // Get the standard size type for the value of the expression
    QualifiedType size_type = semantic_checker_get_size_type(sc);

    return NULL;
}

Expression* semantic_checker_handle_cast_expression(SemanticChecker* sc,
        Location lparen_loc, QualifiedType type, Location rparen_loc,
        Expression* rhs)
{
    if (expression_is_invalid(rhs))
    {
        semantic_checker_handle_error_expression(sc, lparen_loc);
    }

    // Both the named type and the right hand side should have scaler type
    // unless the named type is a void type
    QualifiedType real_type = qualified_type_get_canonical(&type);
    if (qualified_type_is(&real_type, TYPE_VOID))
    {
        return expression_create_cast(&sc->ast->ast_allocator, lparen_loc,
                type, rparen_loc, rhs);
    }

    // First check if names type is scaler
    if (!qualified_type_is_scaler(&real_type))
    {
        // diagnostic_error_at(sc->dm, lparen_loc,
        //         "used type '%s' where arithmetic or pointer type is required",
        //         "<type name>");
        diagnostic_error_at(sc->dm, lparen_loc,
                "expected arithmetic or pointer type");
        semantic_checker_handle_error_expression(sc, lparen_loc);
    }

    // Then check if the expression given is scaler
    QualifiedType expr_type = expression_get_qualified_type(rhs);
    QualifiedType real_expr_type = qualified_type_get_canonical(&expr_type);
    if (!qualified_type_is_scaler(&real_expr_type))
    {
        // TODO: instead of location being the lparen make it the location of
        // TODO: the operand. Will need to get locations going for all of our
        // TODO: expressions for this to work
        // diagnostic_error_at(sc->dm, lparen_loc, "operand of type '%s' where "
        //         "arithmetic or pointer type is required", "<type name>");
        diagnostic_error_at(sc->dm, lparen_loc,
                "expected arithmetic or pointer type");
        semantic_checker_handle_error_expression(sc, lparen_loc);
    }

    return expression_create_cast(&sc->ast->ast_allocator, lparen_loc, type,
            rparen_loc, rhs);
}

// A nice helper function for perfoming normal arithmetic conversions for us on
// our expression operands. This function differs from the previous conversion
// function in that it makes sure to retain the original expression (no extra)
// cast in the expression if the expression was already of the type that we 
// wanted it to be.
static void semantic_checker_do_usual_arithmetic_conversions(
        SemanticChecker* sc, Expression** lhs, Expression** rhs)
{
    assert(expression_is_valid(*lhs) && expression_is_valid(*rhs));

    // First lvlaue to rvalue before doing anything
    *lhs = semantic_checker_func_array_lvalue_convert(sc, *lhs);
    *rhs = semantic_checker_func_array_lvalue_convert(sc, *rhs);

    // First check that both types are actually arithmetic
    QualifiedType lhs_type = expression_get_qualified_type(*lhs);
    lhs_type = qualified_type_get_canonical(&lhs_type);
    lhs_type = qualified_type_remove_quals(&lhs_type);

    QualifiedType rhs_type = expression_get_qualified_type(*rhs);
    rhs_type = qualified_type_get_canonical(&rhs_type);
    rhs_type = qualified_type_remove_quals(&rhs_type);

    // If either the left-hand or right-hand side is not arithmetic there is no
    // point in doing this conversion at all so bail out of this to save some
    // time.
    if (!qualified_type_is_arithmetic(&lhs_type))
    {
        return;
    }

    if (!qualified_type_is_arithmetic(&rhs_type))
    {
        return;
    }

    // Then get the arithmetic type that will be used for the arithmetic
    // conversion.
    QualifiedType arithmetic_type = semantic_checker_arithmetic_conversion(sc,
            lhs_type, rhs_type);
    
    // If either of the types aren't equal, create an implicit cast from one to
    // the other.
    if (!qualified_type_is_equal(&arithmetic_type, &lhs_type))
    {
        *lhs = semantic_checker_create_implicit_cast(sc, *lhs, arithmetic_type);
    }

    if (!qualified_type_is_equal(&arithmetic_type, &rhs_type))
    {
        *rhs = semantic_checker_create_implicit_cast(sc, *rhs, arithmetic_type);
    }
}

// Helper function to print an general error when invalid operands are provided
// to a binary expression. Prints the operator used to try to be as helpful as
// possible.
static void semantic_checker_binary_invalid_operands(SemanticChecker* sc,
        ExpressionType type, Location operator_location)
{
    const char* expression_operator = NULL;
    switch (type)
    {
        case EXPRESSION_BINARY_TIMES:
            expression_operator = "*";
            break;

        case EXPRESSION_BINARY_DIVIDE:
            expression_operator = "/";
            break;

        case EXPRESSION_BINARY_MODULO:
            expression_operator = "%";
            break;

        case EXPRESSION_BINARY_ADD:
            expression_operator = "+";
            break;

        case EXPRESSION_BINARY_SUBTRACT:
            expression_operator = "-";
            break;

        case EXPRESSION_BINARY_SHIFT_LEFT:
            expression_operator = "<<";
            break;

        case EXPRESSION_BINARY_SHIFT_RIGHT:
            expression_operator = ">>";
            break;

        case EXPRESSION_BINARY_LESS_THAN:
            expression_operator = "<";
            break;

        case EXPRESSION_BINARY_GREATER_THAN:
            expression_operator = ">";
            break;

        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
            expression_operator = "<=";
            break;

        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
            expression_operator = ">=";
            break;

        case EXPRESSION_BINARY_EQUAL:
            expression_operator = "==";
            break;

        case EXPRESSION_BINARY_NOT_EQUAL:
            expression_operator = "!=";
            break;

        case EXPRESSION_BINARY_AND:
            expression_operator = "&";
            break;

        case EXPRESSION_BINARY_XOR:
            expression_operator = "^";
            break;

        case EXPRESSION_BINARY_OR:
            expression_operator = "|";
            break;

        case EXPRESSION_BINARY_LOGICAL_AND:
            expression_operator = "&&";
            break;

        case EXPRESSION_BINARY_LOGICAL_OR:
            expression_operator = "||";
            break;

        case EXPRESSION_BINARY_ASSIGN:
            expression_operator = "=";
            break;

        case EXPRESSION_BINARY_TIMES_ASSIGN:
            expression_operator = "*=";
            break;

        case EXPRESSION_BINARY_DIVIDE_ASSIGN:
            expression_operator = "/=";
            break;

        case EXPRESSION_BINARY_MODULO_ASSIGN:
            expression_operator = "%/";
            break;

        case EXPRESSION_BINARY_ADD_ASSIGN:
            expression_operator = "+=";
            break;

        case EXPRESSION_BINARY_SUBTRACT_ASSIGN:
            expression_operator = "-=";
            break;

        case EXPRESSION_BINARY_SHIFT_LEFT_ASSIGN:
            expression_operator = "<<=";
            break;

        case EXPRESSION_BINARY_SHIFT_RIGHT_ASSIGN:
            expression_operator = ">>=";
            break;

        case EXPRESSION_BINARY_AND_ASSIGN:
            expression_operator = "&=";
            break;

        case EXPRESSION_BINARY_XOR_ASSIGN:
            expression_operator = "^=";
            break;

        case EXPRESSION_BINARY_OR_ASSIGN:
            expression_operator = "|=";
            break;

        default: panic("invalid binary operator type"); return; 
    }

    diagnostic_error_at(sc->dm, operator_location,
            "invalid operands to binary %s expression", expression_operator);
}

Expression* semantic_checker_handle_logical_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_LOGICAL_AND
            || type == EXPRESSION_BINARY_LOGICAL_OR);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));
        
    // Check both operants are of scalar type
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    if (!qualified_type_is_scaler(&lhs_type)
            || !qualified_type_is_scaler(&rhs_type))
    {
        semantic_checker_binary_invalid_operands(sc, type, operator_loc);
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Note: the standard does not explicitly state the below but this happens
    // according to a clang ast-dump so I will do it as well.

    // Promote integer types
    lhs = semantic_checker_promote_integer(sc, lhs);
    rhs = semantic_checker_promote_integer(sc, rhs);

    // The return type for both logical expressions is 'int'
    QualifiedType result_type = semantic_checker_get_int_type(sc);
    return expression_create_binary(&sc->ast->ast_allocator, type, operator_loc,
            lhs, rhs, result_type);
}

Expression* semantic_checker_handle_bitwise_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_XOR || type == EXPRESSION_BINARY_OR
            || type == EXPRESSION_BINARY_AND);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));

    // Check both operants are of integer type
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    if (!qualified_type_is_integer(&lhs_type)
            || !qualified_type_is_integer(&rhs_type))
    {
        semantic_checker_binary_invalid_operands(sc, type, operator_loc);
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Now here we do our usual arithmetic conversions for the lhs and rhs
    semantic_checker_do_usual_arithmetic_conversions(sc, &lhs, &rhs);

    // Now we know that the types of lhs and rhs should be the same, so just get
    // the left hand side here
    QualifiedType result_type = expression_get_qualified_type(lhs);
    return expression_create_binary(&sc->ast->ast_allocator, type, operator_loc,
            lhs, rhs, result_type);
}

Expression* semantic_checker_handle_shift_expression(SemanticChecker* sc, 
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_SHIFT_LEFT
            || type == EXPRESSION_BINARY_SHIFT_RIGHT);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));

    // Check both operants are of integer type
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    if (!qualified_type_is_integer(&lhs_type)
            || !qualified_type_is_integer(&rhs_type))
    {
        semantic_checker_binary_invalid_operands(sc, type, operator_loc);
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Promote integer types
    lhs = semantic_checker_promote_integer(sc, lhs);
    rhs = semantic_checker_promote_integer(sc, rhs);

    // `The type of the result is that of the promoted left operand`
    QualifiedType result_type = expression_get_qualified_type(lhs);

    // TODO: add checks for if the right operand is an expression that will be
    // TODO: negative and warn about it?
    return expression_create_binary(&sc->ast->ast_allocator, type, operator_loc,
            lhs, rhs, result_type);
}

// TODO: would this enum be nice to have in our expression.h file so that
// TODO: codegen can know about the type of addition that it needs to handle
typedef enum ExpressionAdditionKind {
    EXPRESSION_ADDITION_ARITHMETIC, // Both operands are arithmetic
    EXPRESSION_ADDITION_POINTER_INT, // Pointer (+/-) int
    EXPRESSION_ADDITION_INT_POINTER, // int + pointer (must be addition)
    EXPRESSION_ADDITION_POINTER_POINTER, // pointer - pointer (must be sub) 
} ExpressionAdditionKind;

// For both + and - expressions
Expression* semantic_checker_handle_add_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location op_location,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_ADD || type == EXPRESSION_BINARY_SUBTRACT);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));

    // For addition, either both operands shall have arithmetic type, or one 
    // operand shall be a pointer to an object type and the other shall have 
    // integer type. (Incrementing is equivalent to adding 1.)

    // For subtraction, either both operands shall have arithmetic type, or
    // both operands are pointers to qualified or unqualified versions of
    // compatible object types, or the left hand operand is a pointer and the
    // right hand operand has integer type

    // First get the type of both of the expressions
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);

    ExpressionAdditionKind kind;
    if (qualified_type_is_arithmetic(&lhs_type)
            && qualified_type_is_arithmetic(&rhs_type))
    {
        kind = EXPRESSION_ADDITION_ARITHMETIC;
    }
    else if (qualified_type_is_pointer(&lhs_type)
            && qualified_type_is_integer(&rhs_type))
    {
        QualifiedType pointee = type_pointer_get_pointee(&lhs_type);
        if (!qualified_type_is_complete(&pointee))
        {
            diagnostic_error_at(sc->dm, op_location,
                    "arithmetic on a pointer to an incomplete type");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        kind = EXPRESSION_ADDITION_POINTER_INT;
    }
    else if (type == EXPRESSION_BINARY_ADD
            && qualified_type_is_integer(&lhs_type)
            && qualified_type_is_pointer(&rhs_type))
    {
        QualifiedType pointee = type_pointer_get_pointee(&rhs_type);
        if (!qualified_type_is_complete(&pointee))
        {
            diagnostic_error_at(sc->dm, op_location,
                    "arithmetic on a pointer to an incomplete type");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        kind = EXPRESSION_ADDITION_INT_POINTER;
    }
    else if (type == EXPRESSION_BINARY_SUBTRACT
            && qualified_type_is_pointer(&lhs_type)
            && qualified_type_is_pointer(&rhs_type))
    {
        if (!qualified_type_is_compatible_no_quals(&lhs_type, &rhs_type))
        {
            diagnostic_error_at(sc->dm, op_location,
                    "pointers are not compatible types");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        // If it is not directly a pointer desugar it (it should be a typedef)
        if (!qualified_type_is(&lhs_type, TYPE_POINTER))
        {
            assert(qualified_type_is(&lhs_type, TYPE_TYPEDEF));
            lhs_type = qualified_type_get_canonical(&lhs_type);
        }
        QualifiedType lhs_pointee = type_pointer_get_pointee(&lhs_type);
        if (!qualified_type_is_complete(&lhs_pointee))
        {
            diagnostic_error_at(sc->dm, op_location,
                    "arithmetic on a pointer to an incomplete type");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        // If it is not directly a pointer desugar it (it should be a typedef)
        if (!qualified_type_is(&rhs_type, TYPE_POINTER))
        {
            assert(qualified_type_is(&rhs_type, TYPE_TYPEDEF));
            rhs_type = qualified_type_get_canonical(&rhs_type);
        }
        QualifiedType rhs_pointee = type_pointer_get_pointee(&rhs_type);
        if (!qualified_type_is_complete(&rhs_pointee))
        {
            diagnostic_error_at(sc->dm, op_location,
                    "arithmetic on a pointer to an incomplete type");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        kind = EXPRESSION_ADDITION_POINTER_POINTER;
    }
    else
    { 
        semantic_checker_binary_invalid_operands(sc, type, op_location);
        return semantic_checker_handle_error_expression(sc, op_location);
    }

    // Okay now we know we have valid operands to our binary addition expression
    // what we can do is implement the semantics of the expression afterwards.
    QualifiedType result_type = {0};
    switch (kind)
    {
        // If both operands have arithmetic type, the usual arithmetic 
        // conversions are performed on them.
        case EXPRESSION_ADDITION_ARITHMETIC:
        {
            semantic_checker_do_usual_arithmetic_conversions(sc, &lhs, &rhs);
            result_type = expression_get_qualified_type(lhs);
            break;
        }

        // When an expression that has integer type is added to or subtracted
        // from a pointer, the result has the type of the pointer operand
        case EXPRESSION_ADDITION_POINTER_INT:
        case EXPRESSION_ADDITION_INT_POINTER:
        {
            Expression* pointer = (kind == EXPRESSION_ADDITION_POINTER_INT)
                    ? lhs : rhs;
            result_type = expression_get_qualified_type(pointer);
            break;
        }

        // The size of the result is implementation-defined, and its type 
        // (a signed integer type) is ptrdiff_t defined in the <stddef.h> header
        case EXPRESSION_ADDITION_POINTER_POINTER:
            // TODO: this is not portable at all, there should be some way to
            // TODO: get the ptrdiff_t type without the header. I.e. it should
            // TODO: be defined somewhere in the target.
            result_type = semantic_checker_get_long_type(sc);
            break;

        default:
            panic("invalid expression kind after checking operands");
            break;
    }

    // Finally one we know the types and everything about this expression we can
    // finally create it and be done
    return expression_create_binary(&sc->ast->ast_allocator, type, op_location,
            lhs, rhs, result_type);
}

// For *, /, and % expression
Expression* semantic_checker_handle_mult_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location op_location,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_TIMES || type == EXPRESSION_BINARY_DIVIDE
            || type == EXPRESSION_BINARY_MODULO);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));

    // For both '*' and '/' the operands just have to be arithmetic, but for 
    // '%' the operands should be integer.
    bool need_integer = (type == EXPRESSION_BINARY_MODULO);
    bool (*operand_checking_func)(const QualifiedType*) = need_integer
            ? qualified_type_is_integer
            : qualified_type_is_arithmetic;

    // Check both operants are of integer type
    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);
    if (!operand_checking_func(&lhs_type)
            || !operand_checking_func(&rhs_type))
    {
        semantic_checker_binary_invalid_operands(sc, type, op_location);
        return semantic_checker_handle_error_expression(sc, op_location);
    }

    // Now here we do our usual arithmetic conversions for the lhs and rhs
    semantic_checker_do_usual_arithmetic_conversions(sc, &lhs, &rhs);

    // Now we know that the types of lhs and rhs should be the same, so just get
    // the left hand side here
    QualifiedType result_type = expression_get_qualified_type(lhs);
    return expression_create_binary(&sc->ast->ast_allocator, type, op_location,
            lhs, rhs, result_type);
}

Expression* semantic_checker_handle_relational_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location op_location,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_LESS_THAN
            || type == EXPRESSION_BINARY_GREATER_THAN
            || type == EXPRESSION_BINARY_LESS_THAN_EQUAL
            || type == EXPRESSION_BINARY_GREATER_THAN_EQUAL);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));

    // For relational expressions one of the following should hold:
    // both operands have real type (we will just consider arithmetic for now)
    //
    // both operands are pointers to qualified or unqualified versions of 
    // compatible object types
    // 
    // both operands are pointers to qualified or 
    // unqualified versions of compatible incomplete types

    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);

    bool arithmetic_comparison;
    if (qualified_type_is_arithmetic(&lhs_type)
            && qualified_type_is_arithmetic(&rhs_type))
    {
        arithmetic_comparison = true;
    }
    else if (qualified_type_is_pointer(&lhs_type)
            && qualified_type_is_pointer(&rhs_type))
    {
        arithmetic_comparison = false;

        // The types should be compaitable even if they are not complete
        if (!qualified_type_is_compatible_no_quals(&lhs_type, &rhs_type))
        {
            // TODO: same as equality expression
            diagnostic_error_at(sc->dm, op_location,
                    "comparison of distinct pointer types");
            return semantic_checker_handle_error_expression(sc, op_location);
        }
    }
    else
    {
        semantic_checker_binary_invalid_operands(sc, type, op_location);
        return semantic_checker_handle_error_expression(sc, op_location);
    }

    // Now modify our expression based on if this was an arithetic comparison
    // or if it was a pointer comparison.
    if (arithmetic_comparison)
    {
        semantic_checker_do_usual_arithmetic_conversions(sc, &lhs, &rhs);        
    }
    else
    {
        // We already know that the pointer types are compatible with each other
        // so I don't believe that any addition modifications to the expressions
        // needs to happed :)
    }

    // The type of relational expressions should be 'int', so get this and 
    // create the expression
    QualifiedType result_type = semantic_checker_get_int_type(sc);
    return expression_create_binary(&sc->ast->ast_allocator, type, op_location,
            lhs, rhs, result_type);
}

// TODO: will eventually need to go back and fix some things in this function
// TODO: as it does not take into account integer constant expressions properly
// TODO: so will not necessarily be correct.
static bool semantic_checker_is_null_pointer(SemanticChecker* sc, Expression* e)
{
    // An integer constant expression with the value 0, or such an expression 
    // cast to type void *, is called a null pointer constant.
    // any object or function.

    // Ignore invalid expressions
    if (expression_is_invalid(e))
    {
        return false;
    }

    // If it is a cast expression get the inner if it is cast to a void pointer
    if (expression_is(e, EXPRESSION_CAST))
    {
        QualifiedType type = expression_get_qualified_type(e);
        type = qualified_type_get_canonical(&type);

        // if it isn't a cast to a pointer type cannot be a null pointer
        if (!qualified_type_is_pointer(&type))
        {
            return false;
        }

        QualifiedType pointee = type_pointer_get_pointee(&type);
        pointee = qualified_type_get_canonical(&pointee);
        if (!qualified_type_is(&pointee, TYPE_VOID))
        {
            return false;
        }

        e = expression_cast_get_inner(e);
    }

    // Okay now we have removed the cast if it exists and so now we can try to
    // see if we have an integer constant expression.
    if (expression_is(e, EXPRESSION_INTEGER_CONSTANT))
    {
        IntegerValue value = expression_integer_get_value(e);
        if (value.value == 0)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    return false;
}

// Another internal enum to help us implement the semantic of the eqaulity
// expression so that we are able to implement the semantic checking a bit 
// easier
typedef enum EqualityExpressionType {
    EQUALITY_EXPRESSION_ARITHMETIC, // Artihmetic comparison
    EQUALITY_EXPRESSION_POINTER_NULL, // pointer lhs, null rhs
    EQUALITY_EXPRESSION_NULL_POINTER, // pointer rhs, null lhs
    EQUALITY_EXPRESSION_POINTER_POINTER // both sides pointer
} EqualityExpressionType;

Expression* semantic_checker_handle_equality_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location op_location,
        Expression* rhs)
{
    assert(type == EXPRESSION_BINARY_EQUAL 
            || type == EXPRESSION_BINARY_NOT_EQUAL);
    assert(expression_is_valid(lhs) && expression_is_valid(rhs));
    // TODO: add assert that we're not an lvalue?

    // For equality expressions one of the following should hold:
    // Both operands have arithemetic type
    //
    // One operand is a pointer and the other is a null pointer constant
    //
    // Both operands are pointers to qualified or unqualified compatible types
    //
    // One operand is a pointer to an object or incomplete type and the other is
    // a pointer to a qualified or unqualified version of void

    QualifiedType lhs_type = expression_get_qualified_type(lhs);
    QualifiedType rhs_type = expression_get_qualified_type(rhs);

    EqualityExpressionType kind;
    bool compatible_ptr = false;
    bool lhs_void = false; // This is only used if kind is pointer pointer
    if (qualified_type_is_arithmetic(&lhs_type)
            && qualified_type_is_arithmetic(&rhs_type))
    {
        kind = EQUALITY_EXPRESSION_ARITHMETIC;
    }
    else if (qualified_type_is_pointer(&lhs_type)
            && semantic_checker_is_null_pointer(sc, rhs))
    {
        kind = EQUALITY_EXPRESSION_POINTER_NULL;
    }
    else if (semantic_checker_is_null_pointer(sc, lhs)
            && qualified_type_is_pointer(&rhs_type))
    {
        kind = EQUALITY_EXPRESSION_NULL_POINTER;
    }
    else if (qualified_type_is_pointer(&lhs_type)
            && qualified_type_is_pointer(&rhs_type))
    {
        // Okay, now we want to ensure out which of the cases above holds for
        // the pointers. The pointers can either be to compatible types, or
        // one can be object or incomplete and the other can be void

        // Get the canonical so we can compare to void
        QualifiedType real_lhs_type = qualified_type_get_canonical(&lhs_type);
        QualifiedType real_rhs_type= qualified_type_get_canonical(&rhs_type);

        // Get the pointee types so that we can check if one of them is void
        QualifiedType pointee_lhs = type_pointer_get_pointee(&real_lhs_type);
        QualifiedType pointee_rhs = type_pointer_get_pointee(&real_rhs_type);
        if (qualified_type_is_compatible_no_quals(&pointee_lhs, &pointee_rhs))
        {
            // OK -> make sure we know they are compatible
            compatible_ptr = true;
        }
        else if (qualified_type_is(&pointee_lhs, TYPE_VOID))
        {
            // OK -> We know the other is object or incomplete already
            lhs_void = true;
        }
        else if (qualified_type_is(&pointee_rhs, TYPE_VOID))
        {
            // OK -> We know the other is object or incomplete already
            lhs_void = false;
        }
        else
        {
            // This is an error but GCC and Clang both have this as an extension
            // TODO: is this actually an error or is it something that i need to
            // TODO: handle properly???
            diagnostic_error_at(sc->dm, op_location,
                    "comparison of distinct pointer types");
            return semantic_checker_handle_error_expression(sc, op_location);
        }

        kind = EQUALITY_EXPRESSION_POINTER_POINTER;
    }
    else
    {
        semantic_checker_binary_invalid_operands(sc, type, op_location);
        return semantic_checker_handle_error_expression(sc, op_location);
    }

    // Switch on the equality expression kind and complete the necessary 
    // checking based on the kind of equality expression that we ended up 
    // getting
    switch (kind)
    {
        // If both of the operands have arithmetic type, the usual arithmetic 
        // conversions are performed
        case EQUALITY_EXPRESSION_ARITHMETIC:
            semantic_checker_do_usual_arithmetic_conversions(sc, &lhs, &rhs);
            break;

        // Otherwise, at least one operand is a pointer

        // if one operand is a pointer and the other is a null pointer constant,
        // the null pointer constant is converted to the type of the pointer.
        case EQUALITY_EXPRESSION_NULL_POINTER:
        case EQUALITY_EXPRESSION_POINTER_NULL:
        {
            bool lhs_null = (kind == EQUALITY_EXPRESSION_NULL_POINTER);

            // First extract the expression types based on which one is the
            // null and which other is the pointers
            Expression* ptr = lhs_null ? rhs : lhs;
            
            // Then get the type of the pointer so we can convert the constant
            // to it.
            QualifiedType ptr_type = expression_get_qualified_type(ptr);
            Expression* null = lhs_null ? lhs : rhs;
            null = semantic_checker_create_implicit_cast(sc, null, ptr_type);

            // Now set the null pointer side to have the implicit cast.
            if (lhs_null)
            {
                lhs = null;
            }
            else
            {
                rhs = null;
            }
            break;
        }

        case EQUALITY_EXPRESSION_POINTER_POINTER:
        {
            // Nothing to do if they are both compatible
            if (compatible_ptr)
            {
                break;
            }
            
            // The convert the non-void side to be the same type as the void 
            // side.
            if (lhs_void)
            {
                rhs = semantic_checker_create_implicit_cast(sc, rhs,
                        expression_get_qualified_type(lhs));
            }
            else
            {
                lhs = semantic_checker_create_implicit_cast(sc, lhs,
                        expression_get_qualified_type(rhs));
            }
            break;
        }

        default:
            panic("invalid equality expression kind, unreachable");
            break;
    }

    // The expression should return an 'int' type. So here finally create the
    // expression.
    QualifiedType result_type = semantic_checker_get_int_type(sc);
    return expression_create_binary(&sc->ast->ast_allocator, type, op_location,
            lhs, rhs, result_type);
}

Expression* semantic_checker_handle_arithmetic_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs)
{
    // First try to lvalue convert each expressions bailing if this fails for
    // either of them.
    lhs = semantic_checker_func_array_lvalue_convert(sc, lhs);
    if (expression_is_invalid(lhs))
    {
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    rhs = semantic_checker_func_array_lvalue_convert(sc, rhs);
    if (expression_is_invalid(rhs))
    {
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // Dispatch based on the type of expression that we have into more specific
    // functions which handle the logic. Noting that in the functions we are
    // more than okay to assert that we have valid expressions.
    switch (type)
    {
        // Logical operations
        case EXPRESSION_BINARY_LOGICAL_OR:
        case EXPRESSION_BINARY_LOGICAL_AND:
            return semantic_checker_handle_logical_expression(sc, type, lhs,
                    operator_loc, rhs);

        // All of these below require integer types
        case EXPRESSION_BINARY_OR:
        case EXPRESSION_BINARY_XOR:
        case EXPRESSION_BINARY_AND:
            return semantic_checker_handle_bitwise_expression(sc, type, lhs,
                    operator_loc, rhs);

        // These have different semantics to the bitwise operations
        case EXPRESSION_BINARY_SHIFT_LEFT:
        case EXPRESSION_BINARY_SHIFT_RIGHT:
            return semantic_checker_handle_shift_expression(sc, type, lhs,
                    operator_loc, rhs);

        // Arithmetic rules for these can involve pointers
        case EXPRESSION_BINARY_ADD:
        case EXPRESSION_BINARY_SUBTRACT:
            return semantic_checker_handle_add_expression(sc, type, lhs,    
                    operator_loc, rhs);

        // Arithmetic rules for these cannot involve pointers
        case EXPRESSION_BINARY_TIMES:
        case EXPRESSION_BINARY_DIVIDE:
        case EXPRESSION_BINARY_MODULO:
            return semantic_checker_handle_mult_expression(sc, type, lhs,    
                    operator_loc, rhs);

        // Also handle equality and relational expression as part of our
        // arithmetic expressions since they are already here.
        case EXPRESSION_BINARY_EQUAL:
        case EXPRESSION_BINARY_NOT_EQUAL:
            return semantic_checker_handle_equality_expression(sc, type, lhs,    
                    operator_loc, rhs);

        case EXPRESSION_BINARY_LESS_THAN:
        case EXPRESSION_BINARY_GREATER_THAN:
        case EXPRESSION_BINARY_LESS_THAN_EQUAL:
        case EXPRESSION_BINARY_GREATER_THAN_EQUAL:
            return semantic_checker_handle_relational_expression(sc, type, lhs,    
                    operator_loc, rhs);

        default:
            panic("unhandled arithmetic expression type");
            return semantic_checker_handle_error_expression(sc, operator_loc);
    }    
}

Expression* semantic_checker_handle_assignment_expression(SemanticChecker* sc,
        ExpressionType type, Expression* lhs, Location operator_loc,
        Expression* rhs)
{
    return semantic_checker_handle_error_expression(sc, operator_loc);   
}

Expression* semantic_checker_handle_comma_expression(SemanticChecker* sc,
        Expression* lhs, Location comma_location, Expression* rhs)
{
    // gg ez.
    QualifiedType expr_type = expression_get_qualified_type(rhs);
    return expression_create_binary(&sc->ast->ast_allocator, EXPRESSION_COMMA,
            comma_location, lhs, rhs, expr_type);
}

// -----------------------------------------------------------------------------
// End functions for expressions
// -----------------------------------------------------------------------------
// =============================================================================
// -----------------------------------------------------------------------------
// Start functions for statements
// -----------------------------------------------------------------------------

Statement* semantic_checker_handle_compound_statement(SemanticChecker* sc,
        Location lcurly, Statement* first, Location rcurly)
{
    return statement_create_compound(&sc->ast->ast_allocator, lcurly, rcurly,
            first);
}

bool semantic_checker_check_case_allowed(SemanticChecker* sc,
        Location case_location)
{
    Scope* switch_scope = scope_get_switch(sc->scope);

    bool allowed = switch_scope != NULL;

    if (!allowed)
    {
        diagnostic_error_at(sc->dm, case_location,
                "'case' statement not in switch statement");
    }

    return allowed;
}

Statement* semantic_checker_handle_case_statement(SemanticChecker* sc,
        Location case_location, Expression* expression,
        Location colon_location, Statement* stmt)
{
    // TODO: here fold and check the case
    return statement_create_case(&sc->ast->ast_allocator, case_location,
            colon_location, expression, (IntegerValue) {0}, stmt);
}

bool semantic_checker_check_default_allowed(SemanticChecker* sc,
        Location default_location)
{
    Scope* switch_scope = scope_get_switch(sc->scope);

    bool allowed = switch_scope != NULL;

    if (!allowed)
    {
        diagnostic_error_at(sc->dm, default_location,
                "'default' statement not in switch statement");
    }

    return allowed;
}

Statement* semantic_checker_handle_default_statement(SemanticChecker* sc,
        Location default_location, Location colon_location, Statement* stmt)
{
    return statement_create_default(&sc->ast->ast_allocator, default_location,
            colon_location, stmt);
}

Statement* semantic_checker_handle_if_statement(SemanticChecker* sc,
        Location if_locatoin, Location lparen_location, Expression* expression,
        Location rparen_location, Statement* if_body, Location else_location,
        Statement* else_body)
{
    // TODO: I also want to move this check to be earlier, since, with nested if
    // TODO: statements, this ends up building the inward to outwards meaning
    // TODO: diagnostics occur in the opposite order to what they should be.

    // TODO: should if expression type be 'int' or '_Bool'

    // If the expression if not scalar type error and implicitly cast it to
    // an int
    QualifiedType expr_type = expression_get_qualified_type(expression);
    if (!qualified_type_is_scaler(&expr_type))
    {
        diagnostic_error_at(sc->dm, if_locatoin,
                "statement requires expression of scalar type");
        expression_set_invalid(expression);
        expression = semantic_checker_create_implicit_cast(sc, expression,
                semantic_checker_get_int_type(sc));
    }

    // I believe that is the only thing that needs to be checked.
    return statement_create_if(&sc->ast->ast_allocator, if_locatoin,
            lparen_location, rparen_location, else_location, expression,
            if_body, else_body);
}

Statement* semantic_checker_handle_switch_statement(SemanticChecker* sc,
        Location switch_location, Location lparen_location,
        Expression* expression, Location rparen_location,
        Statement* body)
{
    Statement* switch_stmt = statement_create_switch(&sc->ast->ast_allocator,
            switch_location, lparen_location, rparen_location, expression);
    statement_switch_set_body(switch_stmt, body);

    return body;
}

Statement* semantic_checker_handle_while_statement(SemanticChecker* sc,
        Location while_location, Location lparen_location,
        Expression* expression, Location rparen_location, Statement* stmt)
{
    Statement* while_stmt = statement_create_while(&sc->ast->ast_allocator,
            while_location, lparen_location, rparen_location, expression);
    statement_while_set_body(while_stmt, stmt);

    return while_stmt;
}

Statement* semantic_checker_handle_do_while_statement(SemanticChecker* sc,
        Location do_location, Statement* body, Location while_location,
        Location lparen_location, Expression* expression,
        Location rparen_location, Location semi_location)
{
    Statement* do_while = statement_create_do_while(&sc->ast->ast_allocator,
            do_location);
    statement_do_while_set_body(do_while, while_location, lparen_location,
            rparen_location, expression, body);

    return do_while;
}

Statement* semantic_checker_handle_for_statement(SemanticChecker* sc,
        Location for_location, Location lparen_location,
        Declaration* init_declaration, Expression* init_expression,
        Expression* condition, Expression* increment, Location rparen_location,
        Statement* body)
{
    // Do nothing if we got an error statment (no body)
    if (statement_is(body, STATEMENT_ERROR))
    {
        return semantic_checker_handle_error_statement(sc);
    }

    // Otherwise we will need to gather our initialisation statment.
    Statement* init_statement;
    if (init_declaration != NULL)
    {
        assert(init_expression == NULL);

        // Create out init statement regardless of what happens
        init_statement = semantic_checker_handle_declaration_statement(sc,
                init_declaration, LOCATION_INVALID);
        
        // Make sure we have a variable declaration here
        Identifier* identifier = declaration_get_identifier(init_declaration);
        Location location = declaration_get_location(init_declaration);
        if (!declaration_is(init_declaration, DECLARATION_VARIABLE))
        {
            diagnostic_error_at(sc->dm, location,
                    "non-variable declaration in 'for' loop");
            declaration_set_invalid(init_declaration);
        }
        else if (declaration_is_valid(init_declaration))
        {
            // Make sure that the storage class is okay
            StorageSpecifier storage = declaration_get_storage_class(
                    init_declaration);
            if (storage == STORAGE_EXTERN || storage == STORAGE_STATIC)
            {
                diagnostic_error_at(sc->dm, location,
                        "declaration of non-local variable in 'for' loop");
                declaration_set_invalid(init_declaration);
            }
        }
    }
    else if (init_expression != NULL)
    {
        assert(init_declaration == NULL);
        init_statement = semantic_checker_handle_expression_statement(sc,
                init_expression, LOCATION_INVALID);
    }
    else
    {
        // TODO: is this correct?
        init_statement = semantic_checker_handle_error_statement(sc);
    }

    // Create the for statement
    Statement* for_stmt = statement_create_for(&sc->ast->ast_allocator,
            for_location, lparen_location, rparen_location, init_statement,
            condition, increment);
    statement_for_set_body(for_stmt, body);

    return for_stmt;
}

Statement* semantic_checker_handle_goto_statement(SemanticChecker* sc,
        Location goto_location, Identifier* identifier,
        Location identifier_location, Location semi_location)
{
    // Get the label from the semantic checker. This should never be null as
    // we will implicitly create a label if it does not already exist.
    // TODO: we can also use this call to help diagnose unused labels too!
    Declaration* label = semantic_checker_act_on_goto(sc, identifier,
            identifier_location);
    
    return statement_create_goto(&sc->ast->ast_allocator, goto_location,
            semi_location, label);
}

Statement* semantic_checker_handle_continue_statement(SemanticChecker* sc,
        Location continue_location, Location semi_location)
{
    Scope* continuable = scope_get_continue(sc->scope);

    if (continuable == NULL)
    {
        diagnostic_error_at(sc->dm, continue_location, 
                "'continue' statement not in loop statement");
        return statement_create_error(&sc->ast->ast_allocator);
    }

    return statement_create_contine(&sc->ast->ast_allocator, continue_location, 
            semi_location);
}

Statement* semantic_checker_handle_break_statement(SemanticChecker* sc,
        Location break_location, Location semi_location)
{
    Scope* breakable = scope_get_break(sc->scope);

    if (breakable == NULL)
    {
        diagnostic_error_at(sc->dm, break_location,
                "'break' statement not in loop or switch statement");
        return statement_create_error(&sc->ast->ast_allocator);
    }

    return statement_create_break(&sc->ast->ast_allocator, break_location, 
            semi_location);
}

Statement* semantic_checker_handle_return_statement(SemanticChecker* sc,
        Location return_location, Expression* expression,
        Location semi_location)
{
    if (expression != NULL && expression_is_invalid(expression))
    {
        return semantic_checker_handle_error_statement(sc);
    }

    FunctionScope* scope = sc->function;
    Declaration* function = function_scope_get_function(scope);
    assert(declaration_is(function, DECLARATION_FUNCTION));

    // Get the return type from the function
    QualifiedType type = declaration_get_type(function);
    QualifiedType return_type = type_function_get_return(&type);
    QualifiedType real_type = qualified_type_get_canonical(&return_type);

    // Check for a return type mismatch
    if (qualified_type_is(&real_type, TYPE_VOID) && expression != NULL)
    {
        QualifiedType expr_type = expression_get_qualified_type(expression);
        QualifiedType real_expr = qualified_type_get_canonical(&expr_type);

        // Note: this below, is a -Wpedantic in Clang and GCC
        if (qualified_type_is(&real_expr, TYPE_VOID))
        {
            diagnostic_error_at(sc->dm, return_location,
                    "void function '%s' should not return void expression",
                    function->base.identifier->string.ptr);
        }
        else
        {
            diagnostic_error_at(sc->dm, return_location,
                    "void function '%s' should not return a value",
                    function->base.identifier->string.ptr);

            // Copy Clang behaviour and create implicit cast to void since this
            // is a warning in Clang.
            expression = semantic_checker_cast_to_void(sc, expression);
        }
    }
    else if (!qualified_type_is(&real_type, TYPE_VOID))
    {
        if (expression == NULL)
        {
            diagnostic_error_at(sc->dm, return_location,
                    "non-void function '%s' should return a value",
                    function->base.identifier->string.ptr);
            expression = semantic_checker_handle_error_expression(sc,
                    semi_location);
        }
        else
        {
            // TODO: check the expressions type matches the current functions 
            // TODO: type!
        }
    }
    
    // Create and return the statement
    return statement_create_return(&sc->ast->ast_allocator, return_location,
            semi_location, expression);
}

Statement* semantic_checker_handle_empty_statement(SemanticChecker* sc,
        Location semi_location)
{
    return statement_create_empty(&sc->ast->ast_allocator, semi_location);
}

Statement* semantic_checker_handle_label_statement(SemanticChecker* sc,
        Location identifier_location, Location colon_location,
        Declaration* label_declaration, Statement* statement)
{
    return statement_create_label(&sc->ast->ast_allocator, identifier_location,
                colon_location, label_declaration, statement);
}

Statement* semantic_checker_handle_declaration_statement(SemanticChecker* sc,
        Declaration* declaration, Location semi_location)
{
    // Handle a posisble empty declaration.
    if (declaration == NULL)
    {
        // TODO: instead of forwarding the NULL, should I make a statement of
        // TODO: empty declaration type? Would this be useful at all?
        return NULL;
    }

    // Note: the declaration should have been checked already so this is simple!
    return statement_create_declaration(&sc->ast->ast_allocator, semi_location,
            declaration);
}

Statement* semantic_checker_handle_expression_statement(SemanticChecker* sc,
        Expression* expression, Location semi_location)
{
    return statement_create_expression(&sc->ast->ast_allocator, semi_location,
            expression);
}

Statement* semantic_checker_handle_error_statement(SemanticChecker* sc)
{
    return statement_create_error(&sc->ast->ast_allocator);
}

// -----------------------------------------------------------------------------
// End functions for statements
// -----------------------------------------------------------------------------
