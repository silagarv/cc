#include "semantic.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include "files/location.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/literal_parser.h"
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
            panic("unimplemented -> union type should be in declaration");
            break;

        case TYPE_SPECIFIER_TYPE_TYPENAME:
            type = specifiers->type;
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

    panic("useless function");
    // scope_insert_member();
}

Declaration* semantic_checker_process_function_param(SemanticChecker* sc,
        Declarator* declarator)
{
    assert(scope_is(sc->scope, SCOPE_FUNCTION));
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
    // TODO: For above it seems we only need to check the above if we are in a
    // TODO: function definition instead of a function declaration.

    bool insert = true;
    Identifier* identifier = declarator->identifier;

    // Check if we have a previous declaration and if we then give a
    // redefinition error.
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);

    if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, declarator->identifier_location,
                "redefinition of parameter '%s'", identifier->string.ptr);

        identifier = identifier_table_get(sc->identifiers, "<invalid>");
        insert = false;
    }

    // Create the declaration for the function parameter. And add it into the
    // scope if we got an identifier and it wasn't a duplicate parameter.
    Declaration* declaration =  declaration_create_variable(
            &sc->ast->ast_allocator, declarator->identifier_location,
            identifier, type, storage);
    
    if (identifier != NULL && insert)
    {
        semantic_checker_insert_ordinairy(sc, declaration);
    }
    
    return declaration;
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
            &sc->ast->ast_allocator, identifer_loc, identifier, type, storage);
    
    // TODO: now that we have the declaration we can process the initializer

    // Okay now we have create our variable we can add the declaration into the
    // current scope if it doesn't already contain this identifier
    Declaration* previous = scope_lookup_ordinairy(sc->scope, identifier,
            false);
    if (previous == NULL)
    {
        // It wasn't in the scope at all so we can simply insert it and we're
        // done.
        semantic_checker_insert_ordinairy(sc, declaration);
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

Declaration* semantic_checker_process_typedef(SemanticChecker* sc,
        Declarator* declarator, QualifiedType type)
{
    const DeclarationSpecifiers* specifiers = declarator->specifiers;
    Identifier* identifier = declarator->identifier;
    Location identifer_loc = declarator->identifier_location;

    if (specifiers->function_spec != TYPE_FUNCTION_SPECIFIER_NONE)
    {
        diagnostic_error_at(sc->dm, identifer_loc,
                "'%s' can only appear on functions",
                function_specifier_to_name(specifiers->function_spec));
    }

    // Create the typedef and the new type setting up the declaration fully.
    Declaration* tdef = declaration_create_typedef(&sc->ast->ast_allocator,
            identifer_loc, identifier, type);
    Type* new_type = type_create_typedef(&sc->ast->ast_allocator, type, tdef);

    declaration_typedef_set_type(tdef, new_type);
    
    // TODO: check for other identifiers already present!
    Declaration* previous = semantic_checker_lookup_ordinairy(sc, identifier,
            false);
    if (previous != NULL && declaration_is(previous, DECLARATION_TYPEDEF))
    {
        // TODO: must check that they typedef to the same thing!
        diagnostic_error_at(sc->dm, declarator->identifier_location,
                "redefinition of typedef '%s'",
                identifier->string.ptr);
        return tdef;
    }
    else if (previous != NULL)
    {
        diagnostic_error_at(sc->dm, declarator->identifier_location,
                "redefinition of '%s' as a different kind of symbol",
                identifier->string.ptr);
        return tdef;
    }

    semantic_checker_insert_ordinairy(sc, tdef);

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
    
    // The main different types of declarations we will have to handle are below
    if (declarator->specifiers->storage_spec == TYPE_STORAGE_SPECIFIER_TYPEDEF)
    {
        return semantic_checker_process_typedef(sc, declarator, type);        
    }
    else if (qualified_type_is(&type, TYPE_FUNCTION))
    {
        // TODO: handle function
        panic("should not handle functions at this time");
        return NULL;
    }
    else
    {
        return semantic_checker_process_variable(sc, declarator, type);
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
                "illegal initializer (only variable can be initialized)");
        return;
    }

    // Otherwise add the initializer to the declaration
    declaration_variable_add_initializer(declaration, initializer);
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

Declaration* semantic_checker_create_enum(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous)
{
    // NOTE: if the enum is anonymous it is not inserted into the tag symbols

    // Create the enum type and a declaration for the enum itself
    QualifiedType type = type_create_enum(&sc->ast->ast_allocator,
            sc->ast->base_types.type_signed_int);
    Declaration* decl = declaration_create_enum(&sc->ast->ast_allocator, 
            enum_location, name, type);
    type_enum_set_declaration(&type, decl);

    // Only, insert it into the scope if it's not anonymous
    if (!anonymous)
    {
        semantic_checker_insert_tag(sc, decl);
    }

    return decl;
}

Declaration* semantic_checker_create_enum_constant(SemanticChecker* sc,
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
    QualifiedType type = (QualifiedType)
    {
        TYPE_QUALIFIER_NONE, 
        sc->ast->base_types.type_signed_int
    };
    Declaration* new_decl = declaration_create_enum_constant(
            &sc->ast->ast_allocator, location, identifier, type, equals,
            expression, value);
    semantic_checker_insert_ordinairy(sc, new_decl);

    return new_decl;
}

Declaration* semantic_checker_create_struct(SemanticChecker* sc,
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

Declaration* semantic_checker_create_union(SemanticChecker* sc,
        Location enum_location, Identifier* name, bool anonymous);

static Declaration* sematantic_checker_create_tag(SemanticChecker* sc,
        DeclarationType type, Location tag_type_loc, Identifier* identifier,
        Location identifier_loc)
{
    bool anonymous = identifier == NULL;
    Identifier* tag_name = !anonymous
                ? identifier
                : identifier_table_get(sc->identifiers, "<anonymous>");
    Location decl_location = !anonymous ? identifier_loc : tag_type_loc;

    switch (type)
    {
        // NOTE: for an enum declaration this will automatically insert it into
        // NOTE: the tag namespace
        case DECLARATION_ENUM:
            return semantic_checker_create_enum(sc, decl_location,
                    tag_name, anonymous);

        case DECLARATION_STRUCT:
            return semantic_checker_create_struct(sc, decl_location,
                    tag_name, anonymous);

        case DECLARATION_UNION:
            panic("union -> unhandled case in semantic checker create tag");
            return NULL;

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
        // TODO: improve error message to include previous tag type?
        diagnostic_error_at(sc->dm, identifier_location, "use of '%s' with a "
                "tag type that does not match previously declared",
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
                    "forward reference to '%s' type", tag_kind_to_name(type));
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
        bool complete;
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
            // Otherwise create an error and create a completely new tag
            diagnostic_error_at(sc->dm, identifier_location,
                    "redefinition of %s '%s'", tag_kind_to_name(type),
                    identifier->string.ptr);

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

