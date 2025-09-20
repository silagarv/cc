#ifndef DECLARATION_H
#define DECLARATION_H

#include <stddef.h>

#include <util/vec.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/expression.h"
#include "parse/initializer.h"

union Statement;

// A struct to hold all of our declaration specifiers
typedef struct DeclarationSpecifiers {
    Type* type;

    TypeStorageSpecifier storage_spec;

    TypeQualifiers qualifiers;

    TypeFunctionSpecifier function_spec;
} DeclarationSpecifiers;

typedef enum DeclarationType {
    DECLARATION_ERROR = -1, /* an error in a declaration */
    DECLARATION_VARIABLE, /* of any local or otherwise variable */
    DECLARATION_FUNCTION, /* of a function with params */
    DECLARATION_TYPEDEF, /* a typedef to any of the above */
    DECLARATION_FIELD, /* of a struct / union */
    DECLARATION_STRUCT, /* of a struct */
    DECLARATION_UNION, /* of a union */
    DECLARATION_ENUM_CONSTANT, /* constants within an enum */
    DECLARATION_ENUM, /* an enum */
    DECLARATION_LABEL, /* A label within the source e.g. `foo:` */
} DeclarationType;

typedef union Declaration Declaration;

// A structure to hold all of the basics needed in a declaration. This helps us
// to keep track of all of the basic information needed with each subtype of
// the declaration keeping track of a bunch of other things.
typedef struct DeclarationBase {
    // What type of declaration is this?
    DeclarationType declaration_type;

    // Where is this declaration located (just a base location here) for a more,
    // fine grained location and information we will need to traverse the entire
    // declaration.
    Location location;

    // Location of the semicolon in the declaration.
    Location semi_location;

    // What name are we giving this declaration
    Identifier* identifier;

    // The fully qualified type for the symbol
    QualifiedType qualified_type;

    // The storage specifier for this symbol
    TypeStorageSpecifier storage_class;

    // The function specifier for this symbol if needed
    TypeFunctionSpecifier function_specifier;

    // Is this declaration implicit (not found in source).
    bool implicit;
} DeclarationBase;

typedef struct DeclarationVariable {
    // The base declaration of this object
    DeclarationBase base;

    // The initializer for this variable
    Initializer* initializer;
} DeclarationVariable;

// A declaration of a function.
// TODO: how do we deal with multiple delcarations of the same function??? We
// will have to have some kind of check for this...
typedef struct DeclarationFunction {
    // The base declaration for this function. Note it already has all of the
    // type, storage, and inline information here so we do not need to repeat it
    // at all.
    DeclarationBase base;

    // TODO: should this be a scope intead where we have all of our parameters
    // in the normal namespace
    // The paramaters of the function, should all be variable declarations.
    DeclarationVariable* parameters;

    // The number of parameters that we actually got.
    size_t num_parameters;

    // The body of this function or NULL if there are only declarations of this
    // function and no definitions.
    union Statement* function_body;
} DeclarationFunction;

// A declaration of a typedef.
typedef struct DeclarationTypedef {
    // The base declaration for this typedef.
    DeclarationBase base;
} DeclarationTypedef;

// A declaration for a field of a struct or union
typedef struct DeclarationField {
    // the base declaration containing the type of the field
    DeclarationBase base;
} DeclarationField;

// TODO: all our other declarations...

typedef struct DeclarationLabel {
    // The base declaration of this object
    DeclarationBase base;

    // Is this label actually used. This is mainly for warning about unused
    // labels in useres code. Note that this should be set even if this is
    // implicitly constructed since we may need it for later
    bool used;
} DeclarationLabel;

union Declaration {
    // The base declaration. Used by all of the specialised declatation like
    // structs as the first member so that we can do type punning and pointer
    // casting to the correct type.
    DeclarationBase base;

    // The declaration to represent a variable. Note that this variable could
    // also represent a function paramter.
    DeclarationVariable variable;

    // A declaration representing a function. There may be multiple of the same
    // function defined in the source so we will have do deal with that.
    DeclarationFunction function;

    // A typedef to another type.
    DeclarationTypedef tdef;

    // A field of a struct / union
    DeclarationField field;

    // The declaration of a label, note that it might not actually be a real
    // label and could be implicitly constructed. However, this will be checked
    // for at some stage.
    DeclarationLabel label;
};

vector_of_decl(Declaration*, Declaration, declaration);

// TODO: redo this completely with our ast allocator

// Create a declaration of a variable. This declaration will represent a
// variable that we can use within other places.
// TODO: add the initializer into the declaration so that we can represent that
// in the AST.
Declaration* declaration_create_variable(Location location,
        Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, Initializer* initializer);

// Create a declaration of label identifier at the given location, and indicate
// if this label was implictly constructed.
Declaration* declaration_create_label(Identifier* identifier, Location location,
        bool implicit);

#endif /* DECLARATION_H */
