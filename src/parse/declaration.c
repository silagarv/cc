#include "declaration.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "lex/preprocessor.h"
#include "util/vec.h"

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/ast_allocator.h"

vector_of_impl(Declaration*, Declaration, declaration)
vector_of_impl(DeclaratorPiece, DeclaratorPiece, declarator_piece)

char* tag_kind_to_name(DeclarationType type)
{
    switch (type)
    {
        case DECLARATION_ENUM:
            return "enum";

        case DECLARATION_STRUCT:
            return "struct";

        case DECLARATION_UNION:
            return "union";

        default:
            panic("bad tag type in tag_kind_to_name");
            return NULL;
    }
}

char* declarator_context_to_name(DeclaratorContext context)
{
    switch (context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
            return "type id";

        case DECLARATION_CONTEXT_FUNCTION_PARAM:
            return "function parameter";

        case DECLARATION_CONTEXT_BLOCK:
            return "block";

        case DECLARATION_CONTEXT_FILE:
            return "file";

        case DECLARATION_CONTEXT_STRUCT:
            return "struct";
    }
}

DeclarationSpecifiers declaration_specifiers_create(Location location)
{
    DeclarationSpecifiers specifiers = (DeclarationSpecifiers)
    {
        .type = NULL,
        .storage_spec = TYPE_STORAGE_SPECIFIER_NONE,
        .qualifiers = TYPE_QUALIFIER_NONE,
        .function_spec = TYPE_FUNCTION_SPECIFIER_NONE,
        .type_spec_type = TYPE_SPECIFIER_TYPE_NONE,
        .type_spec_width = TYPE_SPECIFIER_WIDTH_NONE,
        .type_spec_sign = TYPE_SPECIFIER_SIGN_NONE,
        .type_spec_complex = TYPE_SPECIFIER_COMPLEX_NONE,
        .location = location
    };

    return specifiers;
}

Declarator declarator_create(DeclarationSpecifiers* specifiers,
        DeclaratorContext context)
{
    Declarator declarator = (Declarator)
    {
        .context = context,
        .specifiers = specifiers,
        .identifier = NULL,
        .identifier_location = LOCATION_INVALID,
        .pieces = declarator_piece_vector_create(1),
        .invalid = false
    };

    return declarator;
}

void declarator_delete(Declarator* declarator)
{
    declarator_piece_vector_free(&declarator->pieces, NULL);
}

DeclaratorContext declarator_get_context(const Declarator* declarator)
{
    return declarator->context;
}

bool declarator_identifier_allowed(const Declarator* declarator)
{
    switch (declarator->context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
            return false;

        case DECLARATION_CONTEXT_FUNCTION_PARAM:
        case DECLARATION_CONTEXT_BLOCK:
        case DECLARATION_CONTEXT_FILE:
        case DECLARATION_CONTEXT_STRUCT:
            return true;
    }

    panic("bad declarator context");
}

bool declarator_identifier_required(const Declarator* declarator)
{
    switch (declarator->context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
        case DECLARATION_CONTEXT_FUNCTION_PARAM:
            return false;

        case DECLARATION_CONTEXT_BLOCK:
        case DECLARATION_CONTEXT_FILE:
        case DECLARATION_CONTEXT_STRUCT:
            return true;
    }

    panic("bad declarator context");
}

bool declarator_has_identifier(const Declarator* declarator)
{
    return declarator->identifier != NULL;
}

void declarator_set_identifier(Declarator* declarator, Identifier* identifier,
        Location identifier_location)
{
    assert(declarator->identifier == NULL && 
            declarator->identifier_location == LOCATION_INVALID);

    declarator->identifier = identifier;
    declarator->identifier_location = identifier_location;
}

void declarator_set_invalid(Declarator* declarator)
{
    declarator->invalid = true;
}

bool declarator_is_invalid(const Declarator* declarator)
{
    return declarator->invalid;
}

void declarator_push_pointer(Declarator* declarator, TypeQualifiers qualifiers)
{
    DeclaratorPiece piece = (DeclaratorPiece)
    {
        .pointer.base.type = DECLARATOR_PIECE_POINTER,
        .pointer.qualifiers = qualifiers
    };

    declarator_piece_vector_push(&declarator->pieces, piece);
}

void declarator_push_array(Declarator* declarator, Location lbracket,
        Location rbracket, TypeQualifiers qualifiers, Expression* expression,
        bool is_static, bool is_star)
{
    DeclaratorPiece piece = (DeclaratorPiece)
    {
        .array.base.type = DECLARATOR_PIECE_ARRAY,
        .array.lbracket = lbracket,
        .array.rbracket = rbracket,
        .array.qualifiers = qualifiers,
        .array.expression = expression,
        .array.is_static = is_static,
        .array.is_star = is_star
    };

    declarator_piece_vector_push(&declarator->pieces, piece);
}

void declarator_push_function(Declarator* declarator, AstAllocator* allocator,
        Location lparen_loc, Location rparen_loc, DeclarationVector* params,
        bool is_variadic)
{   
    size_t num_params = declaration_vector_size(params);
    Declaration** alloced = ast_allocator_alloc(allocator,
            num_params * sizeof(Declaration*));
    for (size_t i = 0; i < num_params; i++)
    {
        alloced[i] = declaration_vector_get(params, i);
    }

    DeclaratorPiece piece = (DeclaratorPiece)
    {
        .function.base.type = DECLARATOR_PIECE_FUNCTION,
        .function.lparen_loc = lparen_loc,
        .function.rparen_loc = rparen_loc,
        .function.paramaters = alloced,
        .function.num_paramaters = num_params,
        .function.is_variadic = is_variadic
    };

    declarator_piece_vector_push(&declarator->pieces, piece);
}

bool declaration_is(const Declaration* decl, DeclarationType type)
{
    if (!decl)
    {
        return false;
    }

    return decl->base.declaration_type == type;
}

bool declaration_has_identifier(const Declaration* decl)
{
    if (!decl)
    {
        return false;
    }

    return decl->base.identifier != NULL;
}

// This is a function to create a barebones base declaration.
static Declaration* declaration_create_base(AstAllocator* allocator, 
        size_t size, DeclarationType decl_type, Location location, 
        Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, TypeFunctionSpecifier function,
        bool implicit)
{
    Declaration* decl = ast_allocator_alloc(allocator, size);
    decl->base = (DeclarationBase)
    {
        .declaration_type = decl_type,
        .location = location,
        .identifier = identifier,
        .qualified_type = type,
        .storage_class = storage,
        .function_specifier = function,
        .implicit = implicit
    };

    return decl;
}

Declaration* declaration_create_error(AstAllocator* allocator,
        Location location)
{
    return declaration_create_base(allocator, sizeof(DeclarationBase),
            DECLARATION_ERROR, location, NULL, (QualifiedType) {0},
            TYPE_STORAGE_SPECIFIER_NONE, TYPE_FUNCTION_SPECIFIER_NONE, false);
}

Declaration* declaration_create_variable(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationVariable), DECLARATION_VARIABLE, location, 
            identifier, type, storage, TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->variable.initializer = NULL;

    return decl;
}

void declaration_variable_add_initializer(Declaration* declaration,
        Initializer* initializer)
{
    assert(declaration_is(declaration, DECLARATION_VARIABLE));
    declaration->variable.initializer = initializer;
}

Declaration* declaration_create_typedef(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationTypedef), DECLARATION_TYPEDEF, location,
            identifier, type, TYPE_STORAGE_SPECIFIER_TYPEDEF,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    
    return decl;
}

void declaration_typedef_set_type(Declaration* tdef, Type* new_type)
{
    tdef->tdef.new_type = new_type;
}

Declaration* declaration_create_enum(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    assert(identifier != NULL);

    Declaration* declaration = declaration_create_base(allocator,
            sizeof(DeclarationEnum), DECLARATION_ENUM, location, identifier,
            type, TYPE_STORAGE_SPECIFIER_NONE, TYPE_FUNCTION_SPECIFIER_NONE,
            false);

    declaration->enumeration.entries = NULL;
    declaration->enumeration.num_entries = 0;
    declaration->enumeration.complete = false;

    return declaration;
}

bool declaration_enum_has_entries(const Declaration* declaration)
{
    if (declaration == NULL)
    {
        return false;
    }

    if (!declaration_is(declaration, DECLARATION_ENUM))
    {
        return false;
    }

    return declaration->enumeration.complete;
}

void declaration_enum_set_entries(Declaration* declaration,
        Declaration** entries, size_t num_entries)
{
    assert(declaration_is(declaration, DECLARATION_ENUM));
    assert(!declaration_enum_has_entries(declaration));
    assert(declaration->enumeration.entries == NULL);
    assert(declaration->enumeration.num_entries == 0);

    declaration->enumeration.entries = entries;
    declaration->enumeration.num_entries = num_entries;
    declaration->enumeration.complete = true;
}

Declaration* declaration_create_enum_constant(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        Location equals, Expression* expression, int value)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationEnumConstant), DECLARATION_ENUM_CONSTANT,
            location, identifier, type, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->enumeration_constant.equals_loc = equals;
    decl->enumeration_constant.expression = expression;
    decl->enumeration_constant.value = value;

    return decl;
}

int declaration_enum_constant_get_value(const Declaration* enum_constant)
{
    if (declaration_is(enum_constant, DECLARATION_ERROR))
    {
        return 0;
    }

    assert(declaration_is(enum_constant, DECLARATION_ENUM_CONSTANT));

    return enum_constant->enumeration_constant.value;
}

Declaration* declaration_create_struct(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationCompound), DECLARATION_STRUCT, location,
            identifier, type, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, false);

    return decl;
}

void declaration_struct_add_members(Declaration* declaration,
        Declaration** members, size_t num_members)
{
    assert(declaration_is(declaration, DECLARATION_STRUCT));

    declaration->compound.members = members;
    declaration->compound.num_members = num_members;
}

bool declaration_struct_is_complete(const Declaration* declaration)
{
    if (declaration == NULL)
    {
        return false;
    }

    DeclarationType decl_type = declaration->base.declaration_type;
    if (decl_type != DECLARATION_STRUCT)
    {
        return false;
    }

    QualifiedType type = declaration->compound.base.qualified_type;
    return type_struct_is_complete(type.type);
}

Declaration* declaration_create_function(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    return NULL;
}

Declaration* declaration_create_label(AstAllocator* allocator, 
        Identifier* identifier, Location location, bool implicit)
{
    // Note the label is a special case where we do not really have a type
    Declaration* decl = declaration_create_base(allocator, 
            sizeof(DeclarationLabel), DECLARATION_LABEL, location, identifier,
            (QualifiedType) {0}, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, implicit);
    decl->label.used = implicit; // if implicit then used...

    return decl;
}

bool declaration_function_has_body(Declaration* declaraiton)
{
    assert(declaration_is(declaraiton, DECLARATION_FUNCTION));

    return declaraiton->function.function_body != NULL;
}

void declaration_function_set_body(Declaration* declaraiton,
        union Statement* body)
{
    assert(!declaration_function_has_body(declaraiton));

    declaraiton->function.function_body = body;
}
