#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stddef.h>

#include "parse/ast_allocator.h"
#include "files/location.h"
#include "lex/identifier_table.h"

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_CHAR,
    TYPE_S_CHAR,
    TYPE_U_CHAR,
    TYPE_S_SHORT,
    TYPE_U_SHORT,
    TYPE_S_INT,
    TYPE_U_INT,
    TYPE_S_LONG,
    TYPE_U_LONG,
    TYPE_S_LONG_LONG,
    TYPE_U_LONG_LONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_LONG_DOUBLE,
    TYPE_IMAGINARY,
    TYPE_COMPLEX,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNCTION_PARAMETER,
    TYPE_FUNCTION,
    TYPE_POINTER,
    TYPE_TYPEDEF,
    TYPE_ERROR
} TypeKind;

typedef enum TypeQualifiers {
    TYPE_QUALIFIER_NONE = 0,
    TYPE_QUALIFIER_CONST = 1 << 0,
    TYPE_QUALIFIER_RESTRICT = 1 << 1,
    TYPE_QUALIFIER_VOLATILE = 1 << 2
} TypeQualifiers;

typedef enum TypeStorageSpecifier {
    TYPE_STORAGE_SPECIFIER_NONE,
    TYPE_STORAGE_SPECIFIER_TYPEDEF,
    TYPE_STORAGE_SPECIFIER_EXTERN,
    TYPE_STORAGE_SPECIFIER_STATIC,
    TYPE_STORAGE_SPECIFIER_AUTO,
    TYPE_STORAGE_SPECIFIER_REGISTER
} TypeStorageSpecifier;

typedef enum TypeFunctionSpecifier {
    TYPE_FUNCTION_SPECIFIER_NONE = 0,
    TYPE_FUNCTION_SPECIFIER_INLINE = 1 << 0
} TypeFunctionSpecifier;

typedef enum TypeSpecifierType {
    TYPE_SPECIFIER_TYPE_NONE,
    TYPE_SPECIFIER_TYPE_VOID,
    TYPE_SPECIFIER_TYPE_CHAR,
    TYPE_SPECIFIER_TYPE_INT,
    TYPE_SPECIFIER_TYPE_FLOAT,
    TYPE_SPECIFIER_TYPE_DOUBLE,
    TYPE_SPECIFIER_TYPE_BOOL,
    TYPE_SPECIFIER_TYPE_ENUM,
    TYPE_SPECIFIER_TYPE_STRUCT,
    TYPE_SPECIFIER_TYPE_UNION,
    TYPE_SPECIFIER_TYPE_TYPENAME
} TypeSpecifierType;

typedef enum TypeSpecifierWidth {
    TYPE_SPECIFIER_WIDTH_NONE = 0,
    TYPE_SPECIFIER_WIDTH_SHORT = 1 << 0,
    TYPE_SPECIFIER_WIDTH_LONG = 1 << 1,
    TYPE_SPECIFIER_WIDTH_LONG_LONG = 1 << 2
} TypeSpecifierWidth;

typedef enum TypeSpecifierSign {
    TYPE_SPECIFIER_SIGN_NONE,
    TYPE_SPECIFIER_SIGN_SIGNED,
    TYPE_SPECIFIER_SIGN_UNSIGNED,
} TypeSpecifierSign;

typedef enum TypeSpecifierComplex {
    TYPE_SPECIFIER_COMPLEX_NONE,
    TYPE_SPECIFIER_COMPLEX_COMPLEX,
    TYPE_SPECIFIER_COMPLEX_IMAGINAIRY
} TypeSpecifierComplex;

typedef union Type Type;

// TODO: i think it would be nice to merge the qualifiers into the type base
// TODO: itself to make it a bit more ergonomic to use.
typedef struct QualifiedType {
    TypeQualifiers qualifiers;
    Type* type;
} QualifiedType;

// The base type telling us how to interpret it
typedef struct TypeBase {
    // What kind of type this is
    TypeKind type;

    // Is this type considered complete (very important)...
    bool is_complete;

    // What is the size of this type?
    size_t type_size;

    // What is the alignment for this type?
    size_t type_alignment;
} TypeBase;

// Both imaginairy and complex types have an underlying type which is either
// float, double, or long double
typedef struct TypeUnreal {
    TypeBase base;
    Type* underlying_type;
} TypeUnreal;

// An array has an underlying element type and the length of an array may either
// be known or unknown and we may have done all of the counting ourselves...
typedef struct TypeArray {
    TypeBase base;
    QualifiedType element_type; // the individual element type
    size_t length; // Set if known otherwise 0

    // TODO: what do I do about the absolute mess below
    bool is_static; // is it declared with [static a] for example
    bool is_star; // is it declared with [*] (why... so many options)
    bool is_vla; // is it a variable length array
} TypeArray;

// Struct and union types are both compound types which both have members. Each
// of these members has themselves a type. 
typedef struct TypeCompoundMember {
    TypeBase base;
    bool is_bitfield; // is the member a bitfield
    union Expression* bitfield_expr; // the constant expression for the bitfield
    size_t bitfield_size; // Size of the members bitfield if needed
} TypeCompoundMember;

typedef struct TypeCompound {
    TypeBase base;

    TypeCompoundMember** members; // the members of the compound type
    size_t num_members;

    union Declaration* decl; // the name given or compiler generated name
} TypeCompound;

// Note: in this all enum types have real_type = int
typedef struct TypeEnum {
    TypeBase base;
    Type* real_type; // the `actual` type we consider this (int...)
    union Declaration* enum_decl; // the enumeration declaration
} TypeEnum;

typedef struct TypeFunctionParameter {
    QualifiedType type;
    struct TypeFunctionParameter* next;
} TypeFunctionParameter;

typedef struct TypeFunction {
    TypeBase base;
    QualifiedType return_type; // the return type of the function
    // TypeFunctionParameter* paramaters; // the parameters of the function
    TypeFunctionParameter* paramaters;
    size_t num_paramaters; // Number of params if known otherwise 0

    bool unspecified_paramters; // e.g. int foo()
    bool is_variadic; // e.g. int foo(int, ...)
} TypeFunction;

// A type to a pointer containing a qualifier type and the base.
typedef struct TypePointer {
    TypeBase base;
    QualifiedType underlying_type;
} TypePointer;

// And a typedef
typedef struct TypeTypedef {
    TypeBase base;
    QualifiedType underlying_type; // the type directly below
    QualifiedType real_type; // the type all the way down in the typedef chain
    union Declaration* tdef; // the typedef that introduced this
} TypeTypedef;

typedef struct TypeError {
    TypeBase base;
} TypeError;

union Type {
    TypeBase type_base;
    TypeUnreal type_imaginary;
    TypeUnreal type_complex;
    TypeArray type_array;
    TypeCompound type_struct;
    TypeCompound type_union;
    TypeEnum type_enum;
    TypeFunction type_function;
    TypePointer type_pointer;
    TypeTypedef type_typedef;
    TypeError type_error;
};

// TODO: should I instead implement some kind of way where types can be compared
// via pointer equality instead of having to possible walk a large type tree
typedef struct TypeBuiltins {
    Type* type_error;
    Type* type_void;
    Type* type_bool;
    Type* type_char;
    Type* type_signed_char;
    Type* type_unsigned_char;
    Type* type_signed_short;
    Type* type_unsigned_short;
    Type* type_signed_int;
    Type* type_unsigned_int;
    Type* type_signed_long;
    Type* type_unsigned_long;
    Type* type_signed_long_long;
    Type* type_unsigned_long_long;
    Type* type_float;
    Type* type_double;
    Type* type_long_double;
} TypeBuiltins;

const char* storage_specifier_to_name(TypeStorageSpecifier specifier);
const char* type_qualifier_to_name(TypeQualifiers qualifier);
const char* function_specifier_to_name(TypeFunctionSpecifier function);
const char* width_specifier_to_name(TypeSpecifierWidth width);
const char* sign_specifier_to_name(TypeSpecifierSign sign);
const char* complex_specifier_to_name(TypeSpecifierComplex complex);
const char* type_specifier_to_name(TypeSpecifierType type);

// Some functions to check for specific type qualifiers
bool type_qualifier_has_any(TypeQualifiers qualifiers);
bool type_qualifier_is_const(TypeQualifiers qualifiers);
bool type_qualifier_is_restrict(TypeQualifiers qualifiers);
bool type_qualifier_is_volatile(TypeQualifiers qualifiers);
bool type_qualifier_already_has(TypeQualifiers qualifiers, TypeQualifiers has);

TypeBuiltins type_builtins_initialise(AstAllocator* allocator);

// Functions for creating and maniputing types
QualifiedType type_create_pointer(AstAllocator* allocator,
        QualifiedType base_type, TypeQualifiers qualifiers);
QualifiedType type_pointer_get_pointee(const QualifiedType* pointer);

QualifiedType type_create_array(AstAllocator* allocator,
        QualifiedType element_type, size_t length, bool is_static,
        bool is_star, bool is_vla);
QualifiedType type_array_get_element_type(const QualifiedType* type);

TypeFunctionParameter* type_create_function_parameter(AstAllocator* allocator,
        QualifiedType type);
void type_function_parameter_set_next(TypeFunctionParameter* param,
        TypeFunctionParameter* next);
TypeFunctionParameter* type_function_parameter_get_next(
        TypeFunctionParameter* param);
QualifiedType type_function_parameter_get_type(TypeFunctionParameter* param);

QualifiedType type_create_function(AstAllocator* allocator,
        QualifiedType return_type, TypeFunctionParameter* paramaters,
        size_t num_paramaters, bool unspecified_paramters, bool variadic);

TypeQualifiers qualified_type_get_quals(const QualifiedType* type);
QualifiedType qualified_type_remove_quals(const QualifiedType* type);

QualifiedType type_create_enum(AstAllocator* allocator, Type* base);
void type_enum_set_declaration(QualifiedType* enum_type,
        union Declaration* decl);
void type_enum_set_complete(QualifiedType* enum_type);
bool type_enum_is_complete(const QualifiedType* enum_type);

Type* type_create_struct(AstAllocator* allocator);
void type_struct_set_declaration(Type* type, union Declaration* declaration);
union Declaration* qualified_type_struct_get_declaration(QualifiedType* type);
bool type_struct_is_complete(Type* type);
void type_struct_set_complete(Type* type);
// TODO: make sure we set the members

Type* type_create_union(AstAllocator* allocator);
void type_union_set_declaration(Type* type, union Declaration* declaration);
bool type_union_is_complete(Type* type);
void type_union_set_complete(Type* type);
// TODO: make sure we set the members

Type* type_create_typedef(AstAllocator* allocator, QualifiedType type,
        union Declaration* decl);

Type* qualified_type_get_raw(const QualifiedType* type);
TypeKind qualified_type_get_kind(const QualifiedType* type);
bool type_is(const Type* type, TypeKind kind);
bool qualified_type_is(const QualifiedType* type, TypeKind kind);
bool qualified_type_is_integer(const QualifiedType* type);
bool qualified_type_is_compound(const QualifiedType* type);
QualifiedType type_get_canonical(const Type* type);
QualifiedType qualified_type_get_canonical(const QualifiedType* type);

bool qualified_type_is_equal(const QualifiedType* t1, const QualifiedType* t2);
bool qualifier_type_is_equal_canonical(const QualifiedType* t1, 
        const QualifiedType* t2);

// Functions to compare types and test if they are equal...
bool type_is_builtin(const Type* t1);
bool type_is_equal(const Type* t1, const Type* t2);
bool qualified_type_is_equal(const QualifiedType* t1, const QualifiedType* t2);
bool qualifier_type_is_equal_canonical(const QualifiedType* t1, 
        const QualifiedType* t2);
bool qualified_type_is_complete(const QualifiedType* type);
bool type_is_object_type(const Type* type);
bool qualifier_type_is_object_type(const QualifiedType* type);

void type_print(const QualifiedType* t1);

#endif /* TYPE_H */
