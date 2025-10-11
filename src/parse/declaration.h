#ifndef DECLARATION_H
#define DECLARATION_H

#include <stddef.h>

#include <util/vec.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/ast_allocator.h"

union Statement;

typedef union Declaration Declaration;

// A struct to hold all of our declaration specifiers.
// TODO: eventually we will want to include locations for all of these so that
// TODO: we are more able to accurately report errors here.
typedef struct DeclarationSpecifiers {
    Type* type;
    TypeStorageSpecifier storage_spec;
    TypeQualifiers qualifiers;
    TypeFunctionSpecifier function_spec;
    TypeSpecifierType type_spec_type;
    TypeSpecifierWidth type_spec_width;
    TypeSpecifierSign type_spec_sign;
    TypeSpecifierComplex type_spec_complex;
    Location location;
    Declaration* declaration; // The declaration for a union / struct / enum
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


typedef struct DeclarationEnumConstant {
    // The base declaration
    DeclarationBase base;

    // optional expression used as the enumeration value.
    Expression* expression;
} DeclarationEnumConstant;

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

// Below is declarator pieces. These help us to form declarations whilst we are
// parsing the declaration. 
typedef enum DeclaratorPieceType {
    DECLARATOR_PIECE_POINTER,
    DECLARATOR_PIECE_ARRAY,
    DECLARATOR_PIECE_FUNCTION
} DeclaratorPieceType;

typedef struct DeclaratorPieceBase {
    DeclaratorPieceType type;
} DeclaratorPieceBase;

typedef struct DeclaratorPiecePointer {
    DeclaratorPieceBase base;
    TypeQualifiers qualifiers;
} DeclaratorPiecePointer;

typedef struct DeclaratorPieceArray {
    DeclaratorPieceBase base;
    Location lbracket;
    Location rbracket;
    TypeQualifiers qualifiers;
    Expression* expression;
    bool is_static;
    bool is_star;
} DeclaratorPieceArray;

typedef struct DeclaratorPieceFunction {
    DeclaratorPieceBase base;
    Declaration** paramaters;
    size_t num_paramaters;
    bool is_variadic;
    bool is_star;
} DeclaratorPieceFunction;

typedef union DeclaratorPiece {
    DeclaratorPieceBase base;
    DeclaratorPiecePointer pointer;
    DeclaratorPieceArray array;
    DeclaratorPieceFunction function;
} DeclaratorPiece;

vector_of_decl(DeclaratorPiece, DeclaratorPiece, declarator_piece);

// Here is our declarator vector that we are going to use to help us parse
// declarations correctly. Note that we definitely eventually want this to
// contain more locations and things that we can use.
typedef struct Declarator {
    DeclarationSpecifiers* specifiers;

    Identifier* identifier;
    Location identifier_location;
    DeclaratorPieceVector pieces;
} Declarator;

Declarator declarator_create(DeclarationSpecifiers* specifiers);
void declarator_delete(Declarator* declarator);

void declarator_set_identifier(Declarator* declarator, Identifier* identifier,
        Location identifier_location);

void declarator_push_pointer(Declarator* declarator, TypeQualifiers qualifiers);
void declarator_push_array(Declarator* declarator, Location lbracket,
        Location rbracket, TypeQualifiers qualifiers, Expression* expression,
        bool is_static, bool is_star);

void declarator_push_function_empty(Declarator* declarator, ...);
void declarator_push_function_knr(Declarator* declarator, ...);
void declarator_push_function(Declarator* declarator, ...);

// TODO: this will need some stuff added to it
void declarator_piece_push_function(Declarator* declarator, bool is_variadic);

// TODO: redo this completely with our ast allocator

bool declaration_is(const Declaration* decl, DeclarationType type);

// Create an error declaraiton
Declaration* declaration_create_error(AstAllocator* allocator,
        Location location);

// Create a declaration of a variable. This declaration will represent a
// variable that we can use within other places.
Declaration* declaration_create_variable(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, Initializer* initializer);

// Create a declaration of label identifier at the given location, and indicate
// if this label was implictly constructed.
Declaration* declaration_create_label(AstAllocator* allocator, 
        Identifier* identifier, Location location, bool implicit);

#endif /* DECLARATION_H */
