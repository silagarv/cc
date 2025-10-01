#include "declaration.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "util/vec.h"
#include "util/xmalloc.h"

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/ast_allocator.h"

vector_of_impl(Declaration*, Declaration, declaration)
vector_of_impl(DeclaratorPiece, DeclaratorPiece, declarator_piece)

Declarator declarator_create(void)
{
    Declarator declarator = (Declarator)
    {
        .identifier = NULL,
        .identifier_location = LOCATION_INVALID,
        .pieces = declarator_piece_vector_create(1)
    };

    return declarator;
}

void declarator_delete(Declarator* declarator)
{
    declarator_piece_vector_free(&declarator->pieces, NULL);
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

void declarator_push_array(Declarator* declarator,
        TypeQualifiers qualifiers, Expression* expression, bool is_static,
        bool is_star)
{
    DeclaratorPiece piece = (DeclaratorPiece)
    {
        .array.base.type = DECLARATOR_PIECE_ARRAY,
        .array.qualifiers = qualifiers,
        .array.expression = expression,
        .array.is_static = is_static,
        .array.is_star = is_star
    };

    declarator_piece_vector_push(&declarator->pieces, piece);
}

void declarator_set_identifier(Declarator* declarator, Identifier* identifier,
        Location identifier_location)
{
    assert(declarator->identifier == NULL && 
            declarator->identifier_location == LOCATION_INVALID);

    declarator->identifier = identifier;
    declarator->identifier_location = identifier_location;
}

bool declaration_is(const Declaration* decl, DeclarationType type)
{
    return decl->base.declaration_type == type;
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

Declaration* declaration_create_variable(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, Initializer* initializer)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationVariable), DECLARATION_VARIABLE, location, 
            identifier, type, storage, TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->variable.initializer = initializer;

    return decl;
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
