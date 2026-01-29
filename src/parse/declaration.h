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

// Structs for us to hold a declaration list to represent a list of declarations
// that we might want to use. E.g. for a list of enums, struct members, etc...
typedef struct DeclarationListEntry {
    Declaration* declaration;
    struct DeclarationListEntry* next;
} DeclarationListEntry;

typedef struct DeclarationList {
    // The allocator for this declaration list
    AstAllocator* allocator;

    // The first entry in the list of declarations
    DeclarationListEntry* head;

    // The final entry in the list of declarations
    DeclarationListEntry** tail;
} DeclarationList;

// A struct to hold all of our declaration specifiers.
// TODO: eventually we will want to include locations for all of these so that
// TODO: we are more able to accurately report errors here.
typedef struct DeclarationSpecifiers {
    Type* type;
    StorageSpecifier storage_spec;
    TypeQualifiers qualifiers;
    TypeFunctionSpecifier function_spec;
    TypeSpecifierType type_spec_type;
    TypeSpecifierWidth type_spec_width;
    TypeSpecifierSign type_spec_sign;
    TypeSpecifierComplex type_spec_complex;
    Location location; // the absolute starting location for these
    Declaration* declaration; // The declaration for a union / struct / enum
} DeclarationSpecifiers;

// TODO: what I want to to turn ourdeclarations into a list all of the time
// e.g struct decl then instace of it should become a list of declarations
// TODO: then function parameters should stay the same though. Should also have
// TODO: an empty declaration option to create

// Below is declarator pieces. These help us to form declarations whilst we are
// parsing the declaration. 
typedef enum DeclaratorPieceType {
    DECLARATOR_PIECE_POINTER,
    DECLARATOR_PIECE_ARRAY,
    DECLARATOR_PIECE_FUNCTION
} DeclaratorPieceType;

typedef struct DeclaratorPieceBase {
    // The type of piece this is
    DeclaratorPieceType type;

    // A pointer to the next piece
    union DeclaratorPiece* next;
} DeclaratorPieceBase;

typedef struct DeclaratorPiecePointer {
    DeclaratorPieceBase base;
    TypeQualifiers qualifiers;
} DeclaratorPiecePointer;

typedef struct DeclaratorPieceArray {
    DeclaratorPieceBase base;
    Location lbracket;
    Location rbracket;
    Location static_location;
    TypeQualifiers qualifiers;
    Expression* expression;
    bool is_static;
    bool is_star;
} DeclaratorPieceArray;

typedef struct DeclaratorPieceFunction {
    DeclaratorPieceBase base;
    Location lparen_loc;
    Location rparen_loc;
    Location dots;
    DeclarationList paramaters;
    size_t num_paramaters;
    DeclarationList all_decls;
    bool is_variadic;
    bool knr;
} DeclaratorPieceFunction;

typedef union DeclaratorPiece {
    DeclaratorPieceBase base;
    DeclaratorPiecePointer pointer;
    DeclaratorPieceArray array;
    DeclaratorPieceFunction function;
} DeclaratorPiece;

// Where is this declaration trying to be definied
typedef enum DeclaratorContext {
    DECL_CTX_FILE,
    DECL_CTX_STRUCT,
    DECL_CTX_PARAM,
    DECL_CTX_BLOCK,
    DECL_CTX_TYPE_NAME,
    DECL_CTX_KNR
} DeclaratorContext;

// Here is our declarator vector that we are going to use to help us parse
// declarations correctly. Note that we definitely eventually want this to
// contain more locations and things that we can use.
typedef struct Declarator {
    // The declaration context for this declarator.
    DeclaratorContext context;

    // The declaration specifiers for this declarator.
    DeclarationSpecifiers* specifiers;

    // the identifer and it's location if present. If no identifer was present
    // then the location of the token which would have been the identifier is
    // used. This is so we can get locations for function parameters and such
    Identifier* identifier;
    Location identifier_location;

    // All of the decarlator pieces that have been seen for this declarator
    DeclaratorPiece* piece_stack;

    // The allocator for this declarator. Note this is simply a pointer to the
    // same allocator the parse uses so it does not need freeing.
    AstAllocator* allocator;

    // The colon location and expresion for the bitfield if present and allowed
    Location colon_location;
    Expression* bitfield_expression;

    // true if this is a function definition
    bool function_defn;

    // true if this declarator has an initializer
    bool has_initializer;

    // true if this declarator was found to be invalid during parsing
    bool invalid;
} Declarator;

// The linkage of the declaration. This is only relavent for variables and 
// functions.
typedef enum DeclarationLinkage {
    DECLARATION_LINKAGE_NONE,
    DECLARATION_LINKAGE_INTERNAL,
    DECLARATION_LINKAGE_EXTERNAL
} DeclarationLinkage;

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
    DECLARATION_LABEL /* A label within the source e.g. `foo:` */
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
    StorageSpecifier storage_class;

    // The function specifier for this symbol if needed
    TypeFunctionSpecifier function_specifier;

    // The context in which this declaration was created
    DeclaratorContext ctx;
    
    // Is this an external declaration (top level)
    bool external;

    // Is this declaration implicit (not found in source).
    bool implicit;

    // Is this declaration invalid.
    bool invalid;
} DeclarationBase;

typedef struct DeclarationVariable {
    // The base declaration of this object
    DeclarationBase base;

    // The linkage of this variable if any
    DeclarationLinkage linkage;

    // The initializer for this variable
    Initializer* initializer;

    // A list of all of the declarations for this variable. Note this is only
    // used if we are a declaration with linkage. Otherwise it is unused
    DeclarationList all_decls;

    // The declaration of this variable that is the definition
    Declaration* definition;

    // Is this a tentative definition
    bool tentative;

    // Does this variable have a definition (note this field is only useful if
    // the variable is one with linkage and can be anything otherwise)
    bool has_definition;
} DeclarationVariable;

// A declaration of a function.
// TODO: how do we deal with multiple delcarations of the same function??? We
// will have to have some kind of check for this...
typedef struct DeclarationFunction {
    // The base declaration for this function. Note it already has all of the
    // type, storage, and inline information here so we do not need to repeat it
    // at all.
    DeclarationBase base;

    // The linkage of this function
    DeclarationLinkage linkage;

    // All the declarations of this function
    DeclarationList all_decls;

    // All of the declarations present in the function paramater list (structs)
    DeclarationList paramaters;

    // The declaration of this function that had the body (if present)
    Declaration* definition;

    // The body of this function or NULL if there are only declarations of this
    // function and no definitions.
    union Statement* function_body;

    bool is_inline;
    bool all_inline;
} DeclarationFunction;

// A declaration of a typedef.
typedef struct DeclarationTypedef {
    // The base declaration for this typedef.
    DeclarationBase base;

    // The typecreate by this typedef
    Type* new_type;
} DeclarationTypedef;

// A declaration for a field of a struct or union
typedef struct DeclarationField {
    // the base declaration containing the type of the field
    DeclarationBase base;

    // The colon location for this field
    Location colon_location;

    // The bitfield expression if present
    Expression* bitfield;

    // The size in bits of the bitfield
    size_t bitfield_size;

    // True if we are a fexible array member
    bool is_flexible;

    // True if we have a bitfield, false otherwise
    bool has_bitfield;
} DeclarationField;

typedef struct DeclarationCompound {
    // Base declaration
    DeclarationBase base;

    // The members of the structure.
    DeclarationList members;

    // Do we have a flexible array member
    bool flexible_array;
} DeclarationCompound;

typedef struct DeclarationEnumConstant {
    // The base declaration
    DeclarationBase base;

    // Location of the equals sign
    Location equals_loc;

    // optional expression used as the enumeration value.
    Expression* expression;

    // Since enum types are all integer anyways
    int value;
} DeclarationEnumConstant;

typedef struct DeclarationEnum {
    // Identifier is already in the base.
    DeclarationBase base;

    Declaration** entries;
    size_t num_entries;

    // Did we get any entries?
    bool complete;

    // Was this an anonymous enum
    bool anonymous;
} DeclarationEnum;

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

    // A struct / union declaration
    DeclarationCompound compound;

    // An enumeration
    DeclarationEnum enumeration;

    // An enumeration constant
    DeclarationEnumConstant enumeration_constant;

    // The declaration of a label, note that it might not actually be a real
    // label and could be implicitly constructed. However, this will be checked
    // for at some stage.
    DeclarationLabel label;
};

vector_of_decl(Declaration*, Declaration, declaration);

DeclarationSpecifiers declaration_specifiers_create(Location location);
bool declaration_specifiers_has_declaration(const DeclarationSpecifiers* d);
Declaration* declaration_specifiers_get_declaration(
        const DeclarationSpecifiers* decl_spec);
bool declaration_specifiers_allow_typename(const DeclarationSpecifiers* d);

StorageSpecifier declaration_specifiers_storage(DeclarationSpecifiers* d);
void declaration_specifiers_remove_storage(DeclarationSpecifiers* d);

char* tag_kind_to_name(DeclarationType type);
char* declarator_context_to_name(DeclaratorContext context);

Declarator declarator_create(DeclarationSpecifiers* specifiers,
        DeclaratorContext context, AstAllocator* allocator);

DeclarationSpecifiers* declarator_get_specifiers(const Declarator* declarator);

Identifier* declarator_get_identifier(const Declarator* declarator);
Location declarator_get_location(const Declarator* declarator);

DeclaratorContext declarator_get_context(const Declarator* declarator);

bool declarator_identifier_allowed(const Declarator* declarator);
bool declarator_identifier_required(const Declarator* declarator);

bool declarator_has_identifier(const Declarator* declarator);
void declarator_set_identifier(Declarator* declarator, Identifier* identifier,
        Location identifier_location);

bool declarator_has_initializer(const Declarator* declarator);
void declarator_set_initializer(Declarator* declarator);

bool declarator_is_func_defn(const Declarator* declarator);
void declarator_set_func_defn(Declarator* declarator);

bool declarator_is_invalid(const Declarator* declarator);
void declarator_set_invalid(Declarator* declarator);

bool declarator_allowed_bitfields(Declarator* declarator);
Location declarator_get_colon_location(Declarator* declarator);
Expression* declarator_get_bitfield_expression(Declarator* declarator);

DeclaratorPiece* declarator_get_function_piece(const Declarator* declarator);
bool declarator_piece_is_knr_function(const DeclaratorPiece* piece);
size_t declarator_piece_function_num_params(const DeclaratorPiece* piece);
Location declarator_piece_function_get_lparen(const DeclaratorPiece* piece);
DeclarationList declarator_function_piece_get_all_decls(
        const DeclaratorPiece* piece);
void declarator_function_piece_set_all_decls(DeclaratorPiece* piece,
        DeclarationList decls);
bool declarator_is_function(const Declarator* declarator);

void declarator_push_pointer(Declarator* declarator, TypeQualifiers qualifiers);
void declarator_push_array(Declarator* declarator, Location lbracket, 
        Location rbracket, Location static_location, TypeQualifiers qualifiers,
        Expression* expression, bool is_static, bool is_star);
void declarator_push_function(Declarator* declarator, Location lparen_loc,
        Location rparen_loc, DeclarationList params, size_t num_params,
        DeclarationList all_decls, Location dots, bool knr);
void declarator_add_bitfield(Declarator* declarator, Location colon_location,
        Expression* expression);

// Functions for our declaration list
DeclarationList declaration_list_create(AstAllocator* allocator);
void declaration_list_push(DeclarationList* list, Declaration* decl);
Declaration* declaration_list_entry_get(const DeclarationListEntry* entry);
DeclarationListEntry* declaration_list_iter(const DeclarationList* list);
DeclarationListEntry* declaration_list_next(const DeclarationListEntry* curr);
bool declaration_list_has_identifier(const DeclarationList* list,
        const Identifier* identifier);

// Functions for declarations
DeclarationType declaration_get_kind(const Declaration *decl);
bool declaration_is(const Declaration* decl, DeclarationType type);
bool declaration_is_tag(const Declaration* decl);
bool declaration_is_compound(const Declaration* decl);
bool declaration_has_identifier(const Declaration* decl);
Identifier* declaration_get_identifier(const Declaration* decl);
bool declaration_is_external(const Declaration* decl);
bool declaration_is_valid(const Declaration* decl);
void declaration_set_invalid(Declaration* decl);
void declaration_set_type(Declaration* decl, QualifiedType type);
QualifiedType declaration_get_type(const Declaration* decl);
StorageSpecifier declaration_get_storage_class(const Declaration* decl);
Location declaration_get_location(const Declaration* decl);
void declaration_set_next(Declaration* decl, Declaration* next);
Declaration* declaration_get_next(Declaration* decl);
Declaration** declaration_get_next_ptr(Declaration* decl);

// Create an error declaraiton
Declaration* declaration_create_error(AstAllocator* allocator,
        Location location);

// Create a declaration of a variable. This declaration will represent a
// variable that we can use within other places.
Declaration* declaration_create_variable(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type, 
        StorageSpecifier storage, DeclarationLinkage linkage,
        bool maybe_tentative);
void declaration_variable_add_initializer(Declaration* declaration,
        Initializer* initializer);
bool declaration_variable_has_initializer(const Declaration* declaration);
bool declaration_variable_has_linkage(const Declaration* declaration);
bool declaration_variable_is_tentative(const Declaration* declaration);
void declaration_variable_set_definition(Declaration* decl, Declaration* defn);
bool declaration_variable_has_definition(const Declaration* declaration);
Declaration* declaration_variable_get_definition(const Declaration* decl);
bool declaration_variable_is_extern(const Declaration* declaration);
DeclarationLinkage declaration_variable_get_linkage(const Declaration* decl);
void declaration_variable_add_decl(Declaration* prev, Declaration* new_var);

Declaration* declaration_create_typedef(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type);
void declaration_typedef_set_type(Declaration* tdef, Type* new_type);

Declaration* declaration_create_enum(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        bool anonymous);
bool declaration_enum_has_entries(const Declaration* declaration);
void declaration_enum_set_entries(Declaration* declaration,
        Declaration** entries, size_t num_entries);

Declaration* declaration_create_enum_constant(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        Location equals, Expression* expression, int value);
int declaration_enum_constant_get_value(const Declaration* enum_constant);

Declaration* declaration_create_field(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        Location colon_location, Expression* expression, uint64_t bitfield_size,
        bool is_flexible);
bool declaration_field_has_bitfield(const Declaration* decl);
uint64_t  declaration_field_get_bitfield(const Declaration* decl);
bool declaration_field_is_fexible_array(const Declaration* decl);

Declaration* declaration_create_struct(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type);
void declaration_struct_add_member(Declaration* declaration,
        Declaration* member);
DeclarationList declaration_struct_get_members(const Declaration* declaration);
bool declaration_struct_is_complete(const Declaration* declaration);
void declaration_struct_set_complete(Declaration* declaration);
void declaration_struct_set_flexible_array(Declaration* decalration);
bool declaration_struct_get_flexible_array(Declaration* decalration);

Declaration* declaration_create_union(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type);
void declaration_union_add_member(Declaration* declaration,
        Declaration* members);
bool declaration_union_is_complete(const Declaration* declaration);

Declaration* declaration_create_function(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        StorageSpecifier storage, TypeFunctionSpecifier function_spec,
        DeclarationList all_decls, DeclarationLinkage linkage);
DeclarationLinkage declaration_function_get_linkage(const Declaration* func);
bool declaration_function_is_inline(const Declaration* function);
void declaration_function_add_decl(Declaration* function, Declaration* decl);
bool declaration_function_has_body(const Declaration* declaration);
void declaration_function_set_body(Declaration* declaraiton,
        union Statement* body);
union Statement* declaration_function_get_body(const Declaration* declaration);
void declaration_function_set_definition(Declaration* declaration,
        Declaration* definition);
Declaration* declaration_function_get_definition(Declaration* declaration);
bool declaration_function_has_definition(Declaration* declaration);
bool declaration_function_is_knr(const Declaration* declaration);
DeclarationList declaration_function_get_paramaters(const Declaration* func);

// Create a declaration of label identifier at the given location, and indicate
// if this label was implictly constructed.
Declaration* declaration_create_label(AstAllocator* allocator, 
        Identifier* identifier, Location location, bool implicit);

// Functions for creating and managing a declaration list


#endif /* DECLARATION_H */
