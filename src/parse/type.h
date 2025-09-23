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
    TYPE_FUNCTION_SPECIFIER_NONE,
    TYPE_FUNCTION_SPECIFIER_INLINE
} TypeFunctionSpecifier;

// This is only used while parsing a declaration with an inbuilt type
typedef enum TypeSpecifier {
    // All of the base specifiers are listed here
    TYPE_SPECIFIER_NONE = 0,
    TYPE_SPECIFIER_VOID = 1 << 0,
    TYPE_SPECIFIER_CHAR = 1 << 1,
    TYPE_SPECIFIER_SHORT = 1 << 2,
    TYPE_SPECIFIER_INT = 1 << 3,
    TYPE_SPECIFIER_LONG = 1 << 4,
    TYPE_SPECIFIER_FLOAT = 1 << 5,
    TYPE_SPECIFIER_DOUBLE = 1 << 6,
    TYPE_SPECIFIER_SIGNED = 1 << 7,
    TYPE_SPECIFIER_UNSIGNED = 1 << 8,
    TYPE_SPECIFIER_BOOL = 1 << 9,
    TYPE_SPECIFIER_COMPLEX = 1 << 10,
    TYPE_SPECIFIER_IMAGINAIRY = 1 << 11,

    // These are ones which we need to actually be able to use the information
    TYPE_SPECIFIER_LONG_LONG = 1 << 12,
} TypeSpecifier;

typedef union Type Type;

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
    Type* element_type; // The individual element type
    size_t length; // Set if known otherwise 0

    // TODO: what do I do about the absolute mess below
    bool is_size_known; // Do we know a size or at least a minimum size of it
    bool is_static; // is it declared with [static a] for example
    bool is_variable; // is it declared with [*] (why... so many options)
    bool is_size_implicit; // Do we know cause we counted all elements or what?
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
    union Declaration* decl; // the name given or compiler generated name

    TypeCompoundMember* members; // the members of the compound type
    size_t num_members;

    bool is_anonamous; // e.g. struct { int x; };
} TypeCompound;

// TODO: should we leave enums stuck with `int` or do we do something about this
typedef struct TypeEnum {
    TypeBase base;
    Type* real_type; // the `actual` type we consider this (int...)
    union Declaration* decl; // the enum declaration itself
} TypeEnum;

typedef struct TypeFunction {
    TypeBase base;
    QualifiedType return_type; // the return type of the function
    QualifiedType* paramaters; // the parameters of the function
    size_t num_paramaters; // Number of params if known otherwise 0

    bool unspecified_paramters; // e.g. int foo()
    bool is_variadic; // e.g. int foo(int, ...)
} TypeFunction;

// A type to a pointer containing a qualifier type and the base.
typedef struct TypePointer {
    TypeBase base;
    QualifiedType underlying_type; // this could be a pointer type as well
} TypePointer;

// And a typedef
typedef struct TypeTypedef {
    TypeBase base;
    Type* underlying_type;
    union Declaration* tdef; // the typedef that introduced this
} TypeTypedef;

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
};

// TODO: should I instead implement some kind of way where types can be compared
// via pointer equality instead of having to possible walk a large type tree
typedef struct TypeBuiltins {
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

// Some functions to check for specific type qualifiers
bool type_qualifier_is_const(TypeQualifiers qualifiers);
bool type_qualifier_is_restrict(TypeQualifiers qualifiers);
bool type_qualifier_is_volatile(TypeQualifiers qualifiers);
bool type_qualifier_already_has(TypeQualifiers qualifiers, TypeQualifiers has);
bool type_specifier_has(TypeSpecifier current, TypeSpecifier new);

TypeBuiltins type_builtins_initialise(AstAllocator* allocator);

// Functions to compare types and test if they are equal...
bool qualified_type_is_equal(const QualifiedType* t1, const QualifiedType* t2);
bool qualifier_type_is_equal_canonical(const QualifiedType* t1, 
        const QualifiedType* t2);

#endif /* TYPE_H */
