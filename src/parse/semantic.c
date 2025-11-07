#include "semantic.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>

#include "files/location.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/parser.h"
#include "util/buffer.h"

#include "driver/diagnostic.h"

#include "lex/identifier_table.h"

#include "parse/scope.h"
#include "parse/statement.h"
#include "parse/symbol.h"
#include "parse/type.h"
#include "parse/declaration.h"
#include "util/panic.h"

SemanticChecker sematic_checker_create(DiagnosticManager* dm, Ast* ast)
{
    SemanticChecker sc = (SemanticChecker)
    {
        .dm = dm,
        .ast = ast,
        .scope = NULL,
        .function = NULL
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
                    "type specifier missing, defaults to 'int'");
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
        // If it's not a pointer remove it and issue and error
        if (type->type_base.type != TYPE_POINTER)
        {
            qualifiers &= ~TYPE_QUALIFIER_RESTRICT;
            diagnostic_error_at(sc->dm, specifiers->location,
                    "restrict requires a pointer");
        }
    }

    if (type_qualifier_is_const(qualifiers))
    {
        if (type->type_base.type == TYPE_VOID)
        {
            ; // TODO: ...
        }
    }

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
    }

    assert(type != NULL);

    // TODO: support complex and imaginary types...
    
    return add_type_qualifiers(sc, specifiers, type);
}

static void semantic_checker_process_array(SemanticChecker* sc,
        QualifiedType* type, DeclaratorPiece* piece, DeclaratorContext context)
{
    assert(piece->base.type == DECLARATOR_PIECE_ARRAY);

    DeclaratorPieceArray* array = (DeclaratorPieceArray*) piece;
    
    TypeQualifiers qualifiers = TYPE_QUALIFIER_NONE;
    size_t length = 0;
    bool is_static = false;
    bool is_star = false;
    bool is_vla = false;

    // Check if static is allowed here
    if (array->is_static)
    {
        if (context != DECLARATION_CONTEXT_FUNCTION_PARAM)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "'static' used in array declarator outside of function "
                    "prototype");
        }
        else
        {
            is_static = true;
        }
    }

    // Check our type qualifiers
    if (array->qualifiers != TYPE_QUALIFIER_NONE)
    {
        if (context != DECLARATION_CONTEXT_FUNCTION_PARAM)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "type qualifier used in array declarator outside of "
                    "function prototype");
        }
        else
        {
            qualifiers = array->qualifiers;
        }
    }

    // Check if we are allowed star arrays here
    if (array->is_star)
    {
        if (context != DECLARATION_CONTEXT_FUNCTION_PARAM)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "star modifier used outside of function prototype");
        }
        else if (is_static)
        {
            diagnostic_error_at(sc->dm, array->lbracket,
                    "'static' may not not be used with an unspecifier "
                    "variable length array size");
        }
        else
        {
            is_star = true;
            is_vla = true;
        }
    }

    if (array->expression && 
            array->expression->base.kind != EXPRESSION_INTEGER_CONSTANT)
    {
        diagnostic_error_at(sc->dm, array->lbracket,
                "cannot determine array size at this time");
    }

    // Finally, we need to check if the array's element type is a complete type
    if (/*is_complete_type(type)*/0)
    {
        ; /* error */
    }

    // Finally create and set the new type
    QualifiedType new_type = type_create_array(&sc->ast->ast_allocator, type,
            length, is_static, is_star, is_vla);
    *type = new_type;
}

static void semantic_checker_process_pointer(SemanticChecker* sc,
        QualifiedType* type, DeclaratorPiece* piece, DeclaratorContext context)
{
    assert(piece->base.type == DECLARATOR_PIECE_POINTER);

    DeclaratorPiecePointer* pointer = (DeclaratorPiecePointer*) piece;

    // TODO: figure out the restrictions on pointers

    QualifiedType new_type = type_create_pointer(&sc->ast->ast_allocator, type,
            pointer->qualifiers);
    *type = new_type;
}

static void semantic_checker_process_function(SemanticChecker* sc,
        QualifiedType* type, DeclaratorPiece* piece, DeclaratorContext context)
{
    assert(piece->base.type == DECLARATOR_PIECE_FUNCTION);

    DeclaratorPieceFunction* function = (DeclaratorPieceFunction*) piece;

    printf("processing function piece\n");

    // TODO: figure out what needs checking...

}

QualifiedType semantic_checker_process_type(SemanticChecker* sc,
        Declarator* declarator)
{
    // TODO: now that we have the declarator context we can acccept / decline
    // TODO: vmt and vla's

    DeclaratorContext context = declarator->context;

    // First start by getting the type from the declaration specifiers
    QualifiedType type = qualified_type_from_declaration_specifiers(sc, 
            declarator->specifiers);
    
    // We will want to iterate backwards through our 'stack' or declaration
    // pieces
    size_t num_pieces = declarator_piece_vector_size(&declarator->pieces);
    for (size_t i = 0; i < num_pieces; i++)
    {   
        // Iterate backwars this way since size_t will underflow to UINT64_MAX
        DeclaratorPiece piece = 
                declarator_piece_vector_get(&declarator->pieces,
                num_pieces - 1 - i);
        
        switch (piece.base.type)
        {
            case DECLARATOR_PIECE_ARRAY:
                semantic_checker_process_array(sc, &type, &piece, context);
                break;

            case DECLARATOR_PIECE_POINTER:
                semantic_checker_process_pointer(sc, &type, &piece, context);
                break;

            case DECLARATOR_PIECE_FUNCTION:
                semantic_checker_process_function(sc, &type, &piece, context);
                break;

            case DECLARATOR_PIECE_KNR_FUNCTION:
                panic("unimplemented knr function piece processing");
                break;
        }
    }

    type_print(&type);  
    printf("\n");

    return type;
}

Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator)
{
    // Make sure we are actually processing a function param
    assert(declarator->context == DECLARATION_CONTEXT_FUNCTION_PARAM);

    QualifiedType type = semantic_checker_process_type(sc, declarator);

    // Check that we have no storage specifier other then register
    TypeStorageSpecifier storage = declarator->specifiers->storage_spec;
    if (storage != TYPE_STORAGE_SPECIFIER_NONE && 
            storage != TYPE_STORAGE_SPECIFIER_REGISTER)
    {
        diagnostic_error_at(sc->dm, declarator->specifiers->location,
                "invalid storage class specifier in function declarator");
        storage = TYPE_STORAGE_SPECIFIER_NONE;
    }

    // TODO: Also need to check that we don't have an incomplete type

    return declaration_create_variable(&sc->ast->ast_allocator,
            declarator->identifier_location, declarator->identifier, type, 
            storage, NULL);
}

Declaration* semantic_checker_process_variable(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type, Location equal_loc,
        Initializer* initializer)
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
        // All of these three cases should be handled seperately
        case DECLARATION_CONTEXT_STRUCT:
        case DECLARATION_CONTEXT_FUNCTION_PARAM:
        case DECLARATION_CONTEXT_TYPE_NAME:
            panic("bad context type!");
            return NULL;

        case DECLARATION_CONTEXT_BLOCK:
            // TODO: figure out the constraints on this...
            // panic("todo -> block context");
            break;

        case DECLARATION_CONTEXT_FILE:
            if (storage == TYPE_STORAGE_SPECIFIER_REGISTER ||
                    storage == TYPE_STORAGE_SPECIFIER_AUTO)
            {
                diagnostic_error_at(sc->dm, identifer_loc,
                        "illegal storage class '%s' on file scoped variable",
                        storage_specifier_to_name(storage));
            }
            break;
    }

    // Also check that no function specifiers were given
    if (specifiers->function_spec != TYPE_FUNCTION_SPECIFIER_NONE)
    {
        diagnostic_error_at(sc->dm, identifer_loc,
                "'%s' can only appear on functions",
                function_specifier_to_name(specifiers->function_spec));
    }

    Declaration* declaration = declaration_create_variable(
            &sc->ast->ast_allocator, identifer_loc, identifier, type, storage,
            initializer);
    
    // TODO: now that we have the declaration we can process the initializer

    // Okay now we have create our variable we can add the declaration into the
    // current scope if it doesn't already contain this identifier
    Declaration* previous = scope_lookup_ordinairy(sc->scope, identifier,
            false);
    if (previous == NULL)
    {
        // It wasn't in the scope at all so we can simply insert it and we're
        // done.
        scope_insert_ordinairy(sc->scope, declaration);
    }
    else
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
    }

    return declaration;
}

Declaration* semantic_checker_process_declarator(SemanticChecker* sc,
        Declarator* declarator, Location equals, Initializer* initializer)
{
    // Don't even try if we have an invalid declarator. This is mainly here
    // to simplify the logic of the parsing functions.
    if (declarator->invalid)
    {
        return NULL;
    }

    // If we got an invalid declarator bail early
    if (declarator->identifier == NULL)
    {
        return NULL;
    }

    // Regardless of what needs to be done next we must figure our the type
    // from the declarator.
    QualifiedType type = semantic_checker_process_type(sc, declarator);
    
    // The main different types of declarations we will have to handle are below
    if (declarator->specifiers->storage_spec == TYPE_STORAGE_SPECIFIER_TYPEDEF)
    {
        // TODO: handle typedef
        panic("cannot handle typedef at this time");
        return NULL;
    }
    else if (qualified_type_is(&type, TYPE_FUNCTION))
    {
        if (initializer != NULL)
        {
            // TODO: error about initializer;
        }

        // TODO: handle function
        panic("should not handle functions at this time");
        return NULL;
    }
    else
    {
        return semantic_checker_process_variable(sc, declarator, type, equals,
                initializer);
    }
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
    // TODO: should add assert to lookup if we are in ordinariy ns or not

    return scope_lookup_ordinairy(sc->scope, identifier, recursive);
}

Declaration* semantic_checker_lookup_tag(SemanticChecker* sc,
        Identifier* identifier, bool recursive)
{
    // TODO: should add assert to lookup if we are a tag ns or not

    return scope_lookup_tag(sc->scope, identifier, recursive);
}

Declaration* semantic_checker_lookup_member(SemanticChecker* sc,
        Identifier* identifier)
{
    assert(scope_is(sc->scope, SCOPE_MEMBER) && "must be a member scope");

    return scope_lookup_member(sc->scope, identifier);
}

// All of our functions for handling function scopes
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

Expression* typecheck_expression(Ast* ast, Expression* expression);
Declaration* typecheck_declaration(Ast* ast, Declaration* expression);
Statement* typecheck_statement(Ast* ast, Statement* expression);
