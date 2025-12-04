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
#include "parse/ast_allocator.h"
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

// -----------------------------------------------------------------------------
// End functions for handling scopes
// -----------------------------------------------------------------------------
// =============================================================================
// -----------------------------------------------------------------------------
// Start functions for handling declarations
// -----------------------------------------------------------------------------

static QualifiedType semantic_checker_get_int_type(SemanticChecker* sc)
{
    Type* int_type = sc->ast->base_types.type_signed_int;
    return (QualifiedType) {TYPE_QUALIFIER_NONE, int_type};
}

static QualifiedType semantic_checker_get_void_type(SemanticChecker* sc)
{
    Type* void_type = sc->ast->base_types.type_void;
    return (QualifiedType) {TYPE_QUALIFIER_NONE, void_type};
}

static QualifiedType semantic_checker_decay_type(SemanticChecker* sc,
        QualifiedType type)
{
    // TODO: instead of us going in and manually decaying the types. I would
    // TODO: like to create a decayed type type. Which stores both the new type
    // TODO: and the type that it was originally. This is how we would implement
    // TODO: diagnostics for decayed types differing e.g. 
    // TODO:                    int arr[10] vs int arr[2]
    // TODO: occuring in different function prototypes is fine, but we might 
    // TODO: want to warn when that does occur.

    // Get the real type to see through typedefs.
    QualifiedType real_type = qualified_type_get_canonical(&type);

    assert(qualified_type_is(&real_type, TYPE_ARRAY)
            || qualified_type_is(&real_type, TYPE_FUNCTION));

    // Decay function types. To do this turn the function type into a pointer to
    // the said function type
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        // Here we can use the original type and avoid desugaring since pointer
        // doesn't have to go one level lower.
        return type_create_pointer(&sc->ast->ast_allocator, type,
                TYPE_QUALIFIER_NONE);
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
        return type_create_pointer(&sc->ast->ast_allocator, element,
                    TYPE_QUALIFIER_NONE);
    }

    // We should never be here, but as fallback just return the original type
    panic("failed to decay type");
    return type;
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
            else if (specifiers->type_spec_type == TYPE_SPECIFIER_TYPE_CHAR)
            {
                ; // char is also allowed to be signed or unsigned
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
            qualifiers &= ~TYPE_QUALIFIER_RESTRICT;
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
        case TYPE_SPECIFIER_TYPE_NONE:
            diagnostic_error_at(sc->dm, specifiers->location,
                    "type specifier missing, defaults to 'int'");
            type = builtins->type_signed_int;
            break;

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
        {
            Declaration* enum_decl = specifiers->declaration;
            assert(declaration_is(enum_decl, DECLARATION_ENUM));

            // NOTE: qualifiers should be none here anyways
            type = enum_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_TYPE_STRUCT:
        {
            Declaration* struct_decl = specifiers->declaration;
            assert(declaration_is(struct_decl, DECLARATION_STRUCT));

            type = struct_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_TYPE_UNION:
        {
            Declaration* union_decl = specifiers->declaration;
            assert(declaration_is(union_decl, DECLARATION_UNION));

            type = union_decl->base.qualified_type.type;
            break;
        }

        case TYPE_SPECIFIER_TYPE_TYPENAME:
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
    if (is_star && ctx != DECLARATION_CONTEXT_FUNCTION_PARAM)
    {
        diagnostic_error_at(sc->dm, array->lbracket,
                "star modifier used outside of function prototype");
    }

    // Then check for static
    if (ctx != DECLARATION_CONTEXT_FUNCTION_PARAM)
    {
        if (is_static)
        {
            diagnostic_error_at(sc->dm, array->static_location,
                    "'static' used in array declarator outside of function "
                    "prototype");
            is_static = false;
            qualifiers = TYPE_QUALIFIER_NONE;
        }
        else if (qualifiers != TYPE_QUALIFIER_NONE)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "type qualifier used in array declarator outside of "
                    "function prototype");
            qualifiers = TYPE_QUALIFIER_NONE;
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
    QualifiedType new_type = type_create_array(&sc->ast->ast_allocator, current,
            length, is_static, is_star, is_vla);
    return new_type;
}

static QualifiedType process_pointer_type(SemanticChecker* sc, Declarator* d,
        QualifiedType current, DeclaratorPiece* piece, DeclaratorContext ctx,
        bool* invalid)
{
    assert(piece->base.type == DECLARATOR_PIECE_POINTER);

    DeclaratorPiecePointer* pointer = (DeclaratorPiecePointer*) piece;

    QualifiedType new_type = type_create_pointer(&sc->ast->ast_allocator,
            current, pointer->qualifiers);
    return new_type;
}

QualifiedType check_parameter_type(SemanticChecker* sc, Declaration* parameter,
        size_t num_paramaters, bool is_variadic)
{
    QualifiedType type = declaration_get_type(parameter);
    QualifiedType real_type = qualified_type_get_canonical(&type);

    // Function types should be decayed to pointers to function
    if (qualified_type_is(&real_type, TYPE_FUNCTION))
    {
        return semantic_checker_decay_type(sc, type);
    }
    
    // Array types should be decayed to pointer to element type
    if (qualified_type_is(&real_type, TYPE_ARRAY))
    {
        return semantic_checker_decay_type(sc, type);
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
        }
    }

    // Finally create the function type and update the current type.
    QualifiedType new_type = type_create_function(&sc->ast->ast_allocator,
            current, params, num_paramaters, false, function->is_variadic);
    return new_type;
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
                type = process_pointer_type(sc, declarator, type, piece, context,
                        &invalid);
                break;

            case DECLARATOR_PIECE_FUNCTION:
                type = process_function_type(sc, declarator, type, piece, context,
                        &invalid);
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
    if (function != TYPE_FUNCTION_SPECIFIER_NONE)
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
        case TYPE_SPECIFIER_TYPE_ENUM:
        case TYPE_SPECIFIER_TYPE_STRUCT:
        case TYPE_SPECIFIER_TYPE_UNION:
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
    TypeStorageSpecifier storage = specifiers->storage_spec;
    switch (storage)
    {
        case TYPE_STORAGE_SPECIFIER_NONE:
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

        case TYPE_STORAGE_SPECIFIER_AUTO:
        case TYPE_STORAGE_SPECIFIER_EXTERN:
        case TYPE_STORAGE_SPECIFIER_REGISTER:
        case TYPE_STORAGE_SPECIFIER_STATIC:
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
        case TYPE_STORAGE_SPECIFIER_TYPEDEF:
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

Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator)
{
    assert(scope_is(sc->scope, SCOPE_FUNCTION));
    assert(declarator->context == DECLARATION_CONTEXT_FUNCTION_PARAM);

    QualifiedType type = semantic_checker_process_type(sc, declarator);

    // Check that we have no storage specifier other then register
    TypeStorageSpecifier storage = declarator->specifiers->storage_spec;
    if (storage != TYPE_STORAGE_SPECIFIER_NONE
            && storage != TYPE_STORAGE_SPECIFIER_REGISTER)
    {
        diagnostic_error_at(sc->dm, declarator->specifiers->location,
                "invalid storage class specifier in function declarator");
        storage = TYPE_STORAGE_SPECIFIER_NONE;
    }

    // Diagnose the use of inline
    semantic_checker_diagnose_inline(sc, declarator_get_specifiers(declarator));

    // Create the declaration for the function parameter. And add it into the
    // scope if we got an identifier and it wasn't a duplicate parameter.
    Identifier* identifier = declarator_get_identifier(declarator);
    Declaration* declaration =  declaration_create_variable(
            &sc->ast->ast_allocator, declarator->identifier_location,
            identifier, type, storage);

    // Check for previous declaration of function parameter.
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);
    if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, declarator->identifier_location,
                "redefinition of parameter '%s'", identifier->string.ptr);
        declaration_set_invalid(declaration);
    }
    else
    {
        semantic_checker_insert_ordinairy(sc, declaration);
    }

    return declaration;
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
    TypeStorageSpecifier storage = declaration_specifiers_storage(specifiers);
    switch (storage)
    {
        // These are allowed regardless of the scope
        case TYPE_STORAGE_SPECIFIER_NONE:
        case TYPE_STORAGE_SPECIFIER_EXTERN:
            break;

        // This is only allowed if we are not in block scope.
        case TYPE_STORAGE_SPECIFIER_STATIC:
            if (ctx == DECLARATION_CONTEXT_BLOCK)
            {
                diagnostic_error_at(sc->dm, location, "function declared in "
                        "block scope cannot have 'static' storage class");
                invalid = true;
                storage = TYPE_STORAGE_SPECIFIER_NONE;
            }
            break;

        // The storages are not allowed at all for functions...
        case TYPE_STORAGE_SPECIFIER_AUTO:
        case TYPE_STORAGE_SPECIFIER_REGISTER:
            diagnostic_error_at(sc->dm, location,
                    "illegal storage class '%s' on function",
                    storage_specifier_to_name(specifiers->storage_spec));
            invalid = true;
            storage = TYPE_STORAGE_SPECIFIER_NONE;
            break;

        // If we got a 'typedef' function that means we should have known before
        // that we are about to process a function definition. So here we catch
        // that and error.
        case TYPE_STORAGE_SPECIFIER_TYPEDEF:
            assert(declarator_is_func_defn(declarator));
            diagnostic_error_at(sc->dm, specifiers->location,
                "function definition declared 'typedef'");
            invalid = true;
            storage = TYPE_STORAGE_SPECIFIER_NONE;
            break;

        default:
            panic("invalid storage specifier");
            break;
    }

    // We must get all of the declarations in the function piece before creating
    // the function declaration itself so that we can access them later.
    DeclaratorPiece* piece = declarator_get_function_piece(declarator);
    Declaration* decls = declarator_function_piece_get_decls(piece);

    // Create our new function declaration.
    Declaration* declaration = declaration_create_function(
            &sc->ast->ast_allocator, location, identifier, type,
            storage, specifiers->function_spec, decls);
    
    // Now that we have all of our function types and a declaration for it
    // we will want to check that it is compatible with all of our previous
    // definitions if we have any.
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);

    // No previous entries. Nothing could possibly be wrong at this point in
    // time so add it into the ordinairy scope and can simply return.
    if (previous == NULL)
    {
        semantic_checker_insert_ordinairy(sc, declaration);
        return declaration;
    }

    assert(previous != NULL);

    // Otherwise we are sure we have something. So first check if the previous
    // was a function or not. If not, produce and error, invalidate this and
    // return this declaration.
    if (!declaration_is(previous, DECLARATION_FUNCTION))
    {
        diagnostic_error_at(sc->dm, location,
                "redefinition of '%s' as a different kind of symbol",
                identifier->string.ptr);
        declaration_set_invalid(declaration);
        return declaration;
    }

    // Okay. Here we know that both types should be function type. So we will
    // need to check that both types are compatible.
    QualifiedType previous_type = declaration_get_type(previous);

    assert(qualified_type_is(&type, TYPE_FUNCTION));
    assert(qualified_type_is(&previous_type, TYPE_FUNCTION));

    bool compatible = false;

    // If the types are not compatible, error, set it invalid and simply return
    // the current function declaration. Since we will use the first function
    // definition as the master function declaration always.
    if (!compatible)
    {
        diagnostic_error_at(sc->dm, location, "conflicting types for '%s'",
                identifier->string.ptr);
        declaration_set_invalid(declaration);
        return declaration;
    }

    panic("TODO: finish process_function_declaration");
    return NULL;
}

Declaration* semantic_checker_process_variable(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    // Extract important things from the declarator
    const DeclarationSpecifiers* specifiers = declarator->specifiers;
    Identifier* identifier = declarator->identifier;
    Location identifer_loc = declarator->identifier_location;

    // Check the storage is valid
    TypeStorageSpecifier storage = specifiers->storage_spec;
    assert(storage != TYPE_STORAGE_SPECIFIER_TYPEDEF);
    switch (declarator->context)
    {
        // This context does not have any restrictions on the storage class
        case DECLARATION_CONTEXT_BLOCK:
            break;

        // File scoped declarations cannot have register or auto specified
        case DECLARATION_CONTEXT_FILE:
            if (storage == TYPE_STORAGE_SPECIFIER_REGISTER
                    || storage == TYPE_STORAGE_SPECIFIER_AUTO)
            {
                diagnostic_error_at(sc->dm, identifer_loc,
                        "illegal storage class '%s' on file scoped variable",
                        storage_specifier_to_name(storage));
            }
            break;

        // All of these three cases should be handled seperately
        case DECLARATION_CONTEXT_STRUCT:
        case DECLARATION_CONTEXT_FUNCTION_PARAM:
        case DECLARATION_CONTEXT_TYPE_NAME:
            panic("declaration context should not be handled here");
            return NULL;

        default:
            panic("unexpected declaration context");
            return NULL;
    }

    // Diagnose the use of the inline keyword
    semantic_checker_diagnose_inline(sc, declarator_get_specifiers(declarator));

    // Create the new deckaration that we use.
    Declaration* declaration = declaration_create_variable(
            &sc->ast->ast_allocator, identifer_loc, identifier, type, storage);
    
    // Okay now we have create our variable we can add the declaration into the
    // current scope if it doesn't already contain this identifier
    Declaration* previous = scope_lookup_ordinairy(sc->scope, identifier,
            false);
    if (previous != NULL)
    {
        // Otherwise we need to check if they are the same type, and that we
        // have not already have some kind of initializer for it yet. But also
        // checking if we've had it yet or not is for some reason different if
        // we are at file scope :(

        // TODO: for file scope only it is okay if:
        // TODO:    same type
        // TODO:    only 1 initializer
        // TODO:    no extern initializer

        diagnostic_error_at(sc->dm, identifer_loc, "redefinition of '%s'",
                identifier->string.ptr);
        return declaration;
    }

    // Insert this into the scope 
    semantic_checker_insert_ordinairy(sc, declaration);

    return declaration;
}

Declaration* semantic_checker_process_typedef(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    DeclarationSpecifiers* specifiers = declarator_get_specifiers(declarator);
    Identifier* identifier = declarator->identifier;
    Location identifer_loc = declarator->identifier_location;

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
    TypeStorageSpecifier storage = declaration_specifiers_storage(spec);

    // Only process this as a typedef if it is not a function definition
    if (storage == TYPE_STORAGE_SPECIFIER_TYPEDEF
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
    assert(declarator->specifiers->storage_spec == TYPE_STORAGE_SPECIFIER_NONE);

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
        colon_location = LOCATION_INVALID;
        expression = NULL;
        /*return NULL;*/
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
    assert(scope_is(sc->scope, SCOPE_FUNCTION_BODY));

    // Add all of the decalrations from the function scope into the new function
    // body scope.
    Declaration* all_decls = declaration_function_get_paramaters(declaration);
    for (; all_decls != NULL; all_decls = declaration_get_next(all_decls))
    {
        // Any tag declarations get auto inserted
        if (declaration_is_tag(all_decls))
        {
            scope_insert_tag(sc->scope, all_decls);
            continue;
        }

        // Otherwise we should have a function parameter here.
        QualifiedType type = declaration_get_type(all_decls);
        QualifiedType real_type = qualified_type_get_canonical(&type);
        if (declaration_has_identifier(all_decls))
        {
            // Now we need to check that the type is complete and not 'void'
            // if (qualified_type_is(&real_type, TYPE_VOID))
            // {
            //     diagnostic_error_at(sc->dm, all_decls->base.location,
            //             "argument may not have 'void' type");
            //     declaration_set_invalid(declaration);
            // }
            
            if (!qualified_type_is_complete(&real_type))
            {
                // NOTE: that structs are not fully implemented so don't have
                // this error for now
                diagnostic_error_at(sc->dm, all_decls->base.location,
                        "variable '%s' has incomplete type",
                        all_decls->base.identifier->string.ptr);
                declaration_set_invalid(declaration);
            }

            // Insert it anyway since any semantic action with the declaration
            // will fail with no error if we identified one earlier. Also noting
            // that it will not produce any additional errors for us too.
            scope_insert_ordinairy(sc->scope, all_decls);
        }
        else
        {
            // Here we should only error if it is not a void parameter. Since 
            // then we can assume that void will be the only declaration if it
            // appears.
            if (!qualified_type_is(&real_type, TYPE_VOID))
            {
                diagnostic_error_at(sc->dm, all_decls->base.location,
                        "paramater name must not be omitted in a function "
                        "definition");
            }
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
    }

    // Otherwise get the main declaration for the function and check whether
    // that declaration has a function body or not. If it does not then we are
    // good to go. Otherwise, if it does error about it and indicated that we
    // should not use it as the main definition.
    Identifier* identifier = declaration_get_identifier(function);
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);
    (assert(previous));

    // Check that the function does not already have a body.
    if (declaration_function_has_body(previous))
    {
        diagnostic_error_at(sc->dm, declaration_get_location(function),
                "redefinition of '%s'",
                declaration_get_identifier(function)->string.ptr);
    }
}

void semantic_checker_set_function_body(SemanticChecker* sc,
        Declaration* function, Statement* stmt)
{
    // Otherwise get the main declaration for the function
    Identifier* identifier = declaration_get_identifier(function);
    assert(identifier);
    Declaration* boss_decl = semantic_checker_lookup_ordinairy(sc, identifier,
            true);
    assert(boss_decl);

    // Check if the definition if the first reference of the function and set 
    // both decl's as necessary
    if (boss_decl == function)
    {
        declaration_function_set_body(function, stmt);
        declaration_function_set_definition(boss_decl, function);
        return;
    }
    else
    {
        // Otherwise set this declarations body, and also the boss decls body

        // Set the functions body
        declaration_function_set_body(function, stmt);

        // And if it's not a redefinition set the main decls body and definition
        // field.
        // if (!redefinition)
        // {
        //     declaration_function_set_body(boss_decl, stmt);
        //     declaration_function_set_definition(boss_decl, function);
        // }
    }
}

void semantic_checker_declaration_add_initializer(SemanticChecker* sc,
        Declaration* declaration, Location equals, Initializer* initializer)
{
    // First we must check that the declaration is allowed to have an 
    // initializer. If not error about it.
    if (!declaration_is(declaration, DECLARATION_VARIABLE))
    {
        diagnostic_error_at(sc->dm, equals,
                "illegal initializer (only variables can be initialized)");
        declaration_set_invalid(declaration);
        return;
    }

    // Otherwise add the initializer to the declaration
    declaration_variable_add_initializer(declaration, initializer);

    // TODO: we also need to update our declarations type and check that this
    // TODO: will work properly
}

void semantic_checker_declaration_finish(SemanticChecker* sc,
        Declaration* declaration)
{
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
        TYPE_QUALIFIER_NONE,
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
        TYPE_QUALIFIER_NONE,
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
    QualifiedType current_type = expression_get_qualified_type(expression);
    assert(qualified_type_is_integer(&current_type));

    switch (qualified_type_get_kind(&current_type))
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
            QualifiedType int_type = semantic_checker_get_int_type(sc);
            return semantic_checker_create_implicit_cast(sc, expression,
                    int_type);
        }

        // We have an integer type with no conversion to a larger type needed so
        // we can just return the current expression to the user.
        default:
            return expression;
    }

    panic("uncreachable");
    return NULL;
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

        case EXPRESSION_REFERENCE:
        case EXPRESSION_ARRAY_ACCESS:
        case EXPRESSION_STRING_LITERAL:
        case EXPRESSION_COMPOUND_LITERAL:
            kind = VALUE_KIND_LVALUE;
            break;

        // the result of a member access (dot) operator if its left-hand
        // argument is lvalue 
        case EXPRESSION_MEMBER_ACCESS:

        // the result of a member access through pointer -> operator 
        case EXPRESSION_MEMBER_POINTER_ACCESS:
            // panic("TODO: both member access cases");
            kind = VALUE_KIND_LVALUE;
            break;

        // address and dereferene definitely are but the below seem to also
        // be accepted by clang and gcc
        case EXPRESSION_UNARY_ADDRESS:
            // the result of the indirection (unary *) operator applied to a 
            // pointer to object 

        case EXPRESSION_UNARY_DEREFERENCE:
            panic("TODO: unary address operators");
            kind = VALUE_KIND_LVALUE;
            break;

        case EXPRESSION_PARENTHESISED:
        {
            Expression* inner = expression_parenthesised_get_inner(expression);
            return expression_classify(sc, inner);
        }

        default:
            panic("cannot classify expression kind; unimplemented");
            return VALUE_KIND_RVALUE;
    }

    // Should only try to check for modifiable lvalues if we have an lvalue
    assert(kind == VALUE_KIND_LVALUE);
    
    // TODO: check if expression is modifiable lvalue or not (below is wrong)
    if (kind == VALUE_KIND_LVALUE)
    {
        return VALUE_KIND_MODIFIABLE_LVALUE;
    }

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

Expression* semantic_checker_handle_error_expression(SemanticChecker* sc,
        Location location)
{
     return expression_create_error(&sc->ast->ast_allocator,
                sc->ast->base_types.type_error);
}

Expression* semantic_checker_handle_parenthesis_expression(SemanticChecker* sc,
        Location lparen_location, Expression* inner, Location rparen_location)
{
    // TODO: reinstate assert
    // assert(inner != NULL);

    return expression_create_parenthesised(&sc->ast->ast_allocator,
            lparen_location, rparen_location, inner);
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
        return expression_create_error(&sc->ast->ast_allocator,
                sc->ast->base_types.type_error);
    }
    else if (declaration_is(declaration, DECLARATION_TYPEDEF))
    {
        diagnostic_error_at(sc->dm, identifier_location,
                "unexpected type name '%s': expected expression",
                identifier->string.ptr);
        return expression_create_error(&sc->ast->ast_allocator,
                sc->ast->base_types.type_error);
    }

    QualifiedType type = declaration_get_type(declaration);
    return expression_create_reference(&sc->ast->ast_allocator, identifier,
            identifier_location, declaration, type);
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

    QualifiedType qual_type = {TYPE_QUALIFIER_NONE, type};
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

    QualifiedType qual_type = {TYPE_QUALIFIER_NONE, type};
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
    QualifiedType qual_type = {TYPE_QUALIFIER_NONE, type};
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

    QualifiedType type_lhs = expression_get_qualified_type(lhs);
    QualifiedType type_member = expression_get_qualified_type(member);

    bool lhs_is_array = type_is_subscriptable(&type_lhs);
    bool rhs_is_array = type_is_subscriptable(&type_member);

    if (!lhs_is_array && !rhs_is_array)
    {
        diagnostic_error_at(sc->dm, lbracket_loc,
                "subscripted value is not an array, or pointer");
        return semantic_checker_handle_error_expression(sc, lbracket_loc);
    }

    // If we have determined the lhs is the 'array' side then we want to check
    // that the rhs is an integer, otherwise check the lhs is an integer
    Expression* check_integer = lhs_is_array ? member : lhs;
    QualifiedType subscript_type = expression_get_qualified_type(check_integer);

    if (!qualified_type_is_integer(&subscript_type))
    {
        diagnostic_error_at(sc->dm, lbracket_loc,
                "array subscript is not an integer");
        return semantic_checker_handle_error_expression(sc, lbracket_loc);
    }

    // Okay we have an expression we know is an array and an expression that
    // we know is a pointer. We just need to get the inner type of the once that
    // is an array.
    QualifiedType array_type = lhs_is_array
            ? expression_get_qualified_type(lhs)
            : expression_get_qualified_type(member);
    QualifiedType expr_type = get_inner_type(&array_type);

    // Create the array expression remembering which side is the array side
    return expression_create_array(&sc->ast->ast_allocator,
            lbracket_loc, rbracket_loc, lhs, member, expr_type, lhs_is_array);
}

Expression* semantic_checker_handle_call_expression(SemanticChecker* sc,
        Expression* lhs, Location lparen_location, Expression* expr_list,
        Location rparen_location)
{
    QualifiedType call_type = expression_get_qualified_type(lhs);
    QualifiedType real_type = qualified_type_get_canonical(&call_type);

    // TODO: this...

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

    if (kind != VALUE_KIND_MODIFIABLE_LVALUE)
    {
        diagnostic_error_at(sc->dm, operator_loc,
                "read only variable is not assignable");
        return semantic_checker_handle_error_expression(sc, operator_loc);
    }

    // TODO: determine the type... I think some integer types get promoted
    QualifiedType qual_type = {0};
    return NULL;
    // return expression_create_unary(&sc->ast->ast_allocator, type, operator_loc,
    //         expression, qual_type);
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
    // Note, that all of the statements and conditions should be checked already
    // to see if they are okay. So there isn't really any checking that should
    // be done here...
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
    Statement* init_statement;
    if (init_declaration != NULL)
    {
        assert(init_expression == NULL);

        // Create out init statement regardless of what happens
        init_statement = semantic_checker_handle_declaration_statement(sc,
                init_declaration, LOCATION_INVALID);
        
        // Only check any of the below if we still think it is valid
        if (declaration_is_valid(init_declaration))
        {
            // Make sure that the storage class is okay
            TypeStorageSpecifier storage = declaration_get_storage_class(
                    init_declaration);
            Location identifier = declaration_get_location(init_declaration);
            switch (storage)
            {
                case TYPE_STORAGE_SPECIFIER_NONE:
                case TYPE_STORAGE_SPECIFIER_REGISTER:
                case TYPE_STORAGE_SPECIFIER_AUTO:
                    break;

                case TYPE_STORAGE_SPECIFIER_EXTERN:
                case TYPE_STORAGE_SPECIFIER_STATIC:
                    diagnostic_error_at(sc->dm, identifier,
                            "declaration of non-local variable in 'for' loop");
                    declaration_set_invalid(init_declaration);
                    break;

                case TYPE_STORAGE_SPECIFIER_TYPEDEF:
                    diagnostic_error_at(sc->dm, identifier,
                            "non-variable declaration in 'for' loop");
                    declaration_set_invalid(init_declaration);
                    break;

                default:
                    panic("unexpected storage specifier");
                    break;
            }

            // Also check that it is not a function
            if (declaration_is(init_declaration, DECLARATION_FUNCTION))
            {
                diagnostic_error_at(sc->dm, identifier,
                            "non-variable declaration in 'for' loop");
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
    FunctionScope* scope = sc->function;
    Declaration* function = function_scope_get_function(scope);
    assert(declaration_is(function, DECLARATION_FUNCTION));

    // Get the return type from the function
    QualifiedType type = declaration_get_type(function);
    QualifiedType return_type = type.type->type_function.return_type;
    QualifiedType real_type = qualified_type_get_canonical(&return_type);

    // Check for a return type mismatch
    if (qualified_type_is(&real_type, TYPE_VOID) && expression != NULL)
    {
        // TODO: clang does this thing where if will create an AST node with an
        // TODO: implicit cast to void expression
        diagnostic_error_at(sc->dm, return_location,
                "void function '%s' should not return a value",
                function->base.identifier->string.ptr);
        expression = NULL;
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

    Statement* return_stmt = statement_create_return(&sc->ast->ast_allocator,
            return_location, semi_location, expression);

    return return_stmt;
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
    // Handle a posisble empty declaration
    if (declaration == NULL)
    {
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
