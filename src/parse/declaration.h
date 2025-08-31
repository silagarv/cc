#ifndef DECLARATION_H
#define DECLARATION_H

#include <stddef.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/expression.h"
#include "parse/type.h"

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
    DECLARAITON_LABEL, /* A label within the source e.g. `foo:` */
} DeclarationType;

// A structure to hold all of the basics needed in a declaration. This helps us
// to keep track of all of the basic information needed with each subtype of
// the declaration keeping track of a bunch of other things.
typedef struct DeclarationBase {
    // What type of declaration is this?
    DeclarationType declaration_type; 

    // What name are we giving this declaration
    Identifier* identifier;
    
    // Where is this declaration located (just a base location here) for a more,
    // fine grained location and information we will need to traverse the entire
    // declaration.
    Location location;

    // The fully qualified type for the symbol
    QualifiedType qualified_type;

    // The storage specifier for this symbol
    TypeStorageSpecifier storage_class;

    // The function specifier for this symbol if needed
    TypeFunctionSpecifier function_specifier;
} DeclarationBase;

typedef struct DeclarationVariable {
    // The base declaration of this object
    DeclarationBase base;

    // TODO: add initializer stuff...
} DeclarationVariable;

// TODO: all our other declarations...

typedef struct DeclarationLabel {
    // The base declaration of this object
    DeclarationBase base;

    // goto foo; before label foo. This is true until we actually see the label
    // whilst parsing. If we have label before goto this is never actually true
    bool implicit_construction;

    // Is this label actually used. This is mainly for warning about unused
    // labels in useres code. If implicitly constructed this is also set.
    bool used;
} DeclarationLabel;

typedef union Declaration {
    // The base declaration. Used by all of the specialised declatation like
    // structs as the first member so that we can do type punning and pointer
    // casting to the correct type.
    DeclarationBase base;


    DeclarationVariable variable;

    // The declaration of a label, note that it might not actually be a real
    // label and could be implicitly constructed. However, this will be checked
    // for at some stage.
    DeclarationLabel label;
} Declaration;

// Create a declaration of a variable. This declaration will represent a
// variable that we can use within other places.
// TODO: add the initializer into the declaration so that we can represent that
// in the AST.
Declaration* declaration_create_variable(Identifier* identifier, 
        Location location, QualifiedType type, TypeStorageSpecifier storage);

// Create a declaration of label identifier at the given location, and indicate
// if this label was implictly constructed.
Declaration* declaration_create_label(Identifier* identifier, Location location,
        bool implicit_construction);

#endif /* DECLARATION_H */
