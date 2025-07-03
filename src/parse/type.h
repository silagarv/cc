#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stddef.h>

#include "util/str.h"

typedef enum TypeKind {
    TYPE_ERROR,
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
    TYPE_FUNCTION,
    TYPE_POINTER,
    TYPE_TYPEDEF
} TypeKind;

typedef union Type Type;

// Type qualifiers note that only a pointer type has the possibility to have
// the restrict section set
typedef struct TypeQualifiers {
    bool is_const;
    bool is_restrict;
    bool is_volatile;
} TypeQualifiers;

typedef struct TypeBase {
    TypeKind type;
    TypeQualifiers qualifiers;
} TypeBase;

// Both imaginairy and complex types have an underlying type which is either
// float, double, or long double
typedef struct TypeUnreal {
    TypeKind type;
    Type* underlying_type;
} TypeUnreal;

// An array has an underlying element type and the length of an array may either
// be known or unknown and we may have done all of the counting ourselves...
typedef struct TypeArray {
    TypeKind type;
    Type* element_type; // The individual element type
    size_t length; // Set if known otherwise 0

    bool is_size_known; // Do we know a size or at least a minimum size of it
    bool is_static; // is it declared with [static a] for example
    bool is_variable; // is it declared with [*] (why... so many options)
    bool is_size_implicit; // Do we know cause we counted all elements or what?
    bool is_vla; // is it a variable length array
} TypeArry;

// Struct and union types are both compound types which both have members. Each
// of these members has themselves a type. 
typedef struct TypeCompoundMember {
    String name; // The members name
    Type* member_type; // The type of the member

    size_t bitfield_size; // Size of the members bitfield if needed
    bool is_bitfield; // is the member a bitfield

    struct TypeCompoundMember* next; // the next member in the struct or NULL
} TypeCompoundMember;

typedef struct TypeCompound {
    TypeKind type;
    String name; // name may be given or could be anonymous but we give it one

    TypeCompoundMember* first_member; // the members of the compound type
    TypeCompoundMember* last_member; // the last member in the struct

    bool is_anonamous; // e.g. struct { int x;} a;
} TypeCompound;

// A function type

// TODO: finish all of the below
typedef struct TypeFunction {
    TypeBase type_base;
} TypeFunction;

typedef struct TypePointer {
    TypeBase type_base;
    Type* underlying_type; // the type beneath could also be a pointer
} TypePointer;

typedef struct TypeTypedef {
    TypeBase type_base;
    String name; // name of the typedef
    Type* underlying_type; // the underlying type aka. real type it is
} TypeTypedef;

union Type {
    TypeBase type_base;
    TypeUnreal type_imaginary;
    TypeUnreal type_complex;
    TypeArry type_array;
    TypeCompound type_struct;
    TypeCompound type_union;
    TypeFunction type_function;
    TypePointer type_pointer;
    TypeTypedef type_typedef;
};

// Could possibly remove later once we have the type??? as this is more for
// how a declaration works
typedef struct TypeStorageSpecifier {
    bool is_typedef;
    bool is_extern;
    bool is_static;
    bool is_auto;
    bool is_register;
} TypeStorageSpecifier;

// Any type that is not user defined. (TODO: Should I include typedefs?)
bool is_builtin_type(Type* type);

bool is_arithmetic_type(Type* type);
bool is_pointer(Type* type);

bool is_type_equal_no_qualifiers(Type* type);
bool is_type_equal(Type* type);

#endif /* TYPE_H */
