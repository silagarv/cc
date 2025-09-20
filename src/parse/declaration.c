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

vector_of_impl(Declaration*, Declaration, declaration)

// This is a function to create a barebones base declaration.
static Declaration* declaration_create_base(DeclarationType decl_type, 
        Location location, Identifier* identifier, QualifiedType type,
        TypeStorageSpecifier storage, TypeFunctionSpecifier function,
        bool implicit)
{
    Declaration* decl = xmalloc(sizeof(Declaration));
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

Declaration* declaration_create_variable(Location location,
        Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, Initializer* initializer)
{
    assert(storage != TYPE_STORAGE_SPECIFIER_TYPEDEF);

    Declaration* decl = declaration_create_base(DECLARATION_VARIABLE, location,
            identifier, type, storage, TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->variable.initializer = initializer;

    return decl;
}

Declaration* declaration_create_label(Identifier* identifier, Location location,
        bool implicit)
{
    // Note the label is a special case where we do not really have a type
    Declaration* decl = declaration_create_base(DECLARATION_LABEL, location,
            identifier, (QualifiedType) {0}, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, implicit);
    decl->label.used = implicit; // if implicit then used...

    return decl;    
}
