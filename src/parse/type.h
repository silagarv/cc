#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stddef.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/ast_allocator.h"

union Expression;

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
    QUALIFIER_NONE = 0,
    QUALIFIER_CONST = 1 << 0,
    QUALIFIER_RESTRICT = 1 << 1,
    QUALIFIER_VOLATILE = 1 << 2
} TypeQualifiers;

typedef enum StorageSpecifier {
    STORAGE_NONE,
    STORAGE_TYPEDEF,
    STORAGE_EXTERN,
    STORAGE_STATIC,
    STORAGE_AUTO,
    STORAGE_REGISTER
} StorageSpecifier;

typedef enum TypeFunctionSpecifier {
    FUNCTION_SPECIFIER_NONE = 0,
    FUNCTION_SPECIFIER_INLINE = 1 << 0
} TypeFunctionSpecifier;

typedef enum TypeSpecifierType {
    TYPE_SPECIFIER_NONE,
    TYPE_SPECIFIER_VOID,
    TYPE_SPECIFIER_CHAR,
    TYPE_SPECIFIER_INT,
    TYPE_SPECIFIER_FLOAT,
    TYPE_SPECIFIER_DOUBLE,
    TYPE_SPECIFIER_BOOL,
    TYPE_SPECIFIER_ENUM,
    TYPE_SPECIFIER_STRUCT,
    TYPE_SPECIFIER_UNION,
    TYPE_SPECIFIER_TYPENAME,
    TYPE_SPECIFIER_ERROR
} TypeSpecifierType;

typedef enum TypeSpecifierWidth {
    WIDTH_SPECIFIER_NONE = 0,
    WIDTH_SPECIFIER_SHORT = 1 << 0,
    WIDTH_SPECIFIER_LONG = 1 << 1,
    WIDTH_SPECIFIER_LONG_LONG = 1 << 2
} TypeSpecifierWidth;

typedef enum TypeSpecifierSign {
    SIGN_SPECIFIER_NONE,
    SIGN_SPECIFIER_SIGNED,
    SIGN_SPECIFIER_UNSIGNED,
} TypeSpecifierSign;

typedef enum TypeSpecifierComplex {
    COMPLEX_SPECIFIER_NONE,
    COMPLEX_SPECIFIER_COMPLEX,
    COMPLEX_SPECIFIER_IMAGINAIRY
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

    union Expression* expression; // the array size expression

    size_t length; // Set if known otherwise 0

    bool is_static; // is it declared with [static a] for example
    bool is_star; // is it declared with [*] (why... so many options)
    bool is_vla; // is it a variable length array
} TypeArray;

// Struct and union types are both compound types which both have members. Each
// of these members has themselves a type. 
typedef struct TypeCompoundMember {
    TypeBase base; // the base type

    union Declaration* declaration; // the declaration  of this member

    size_t field_offset; // The offset of the field (0 in bitfields)

    size_t bitfield_size; // Size of the members bitfield. Note anonymous 
                          // bitfields can have a size of 0 so need is_bitfield

    bool is_bitfield; // is the member a bitfield
    bool is_flex_array; // is the member a flexible array member
    bool offset_valid; // Is the value of the offset a valid number

    struct TypeCompoundMember* next; // the next compound member
} TypeCompoundMember;

typedef struct TypeCompound {
    TypeBase base; // The base type that we have
    TypeCompoundMember* members; // the members of the compound type
    union Declaration* decl; // the declaration of this type
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

const char* storage_specifier_to_name(StorageSpecifier specifier);
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
TypeQualifiers type_qualifier_combine(TypeQualifiers this, TypeQualifiers that);
bool type_qualifiers_discards_quals(TypeQualifiers to, TypeQualifiers from);

TypeBuiltins type_builtins_initialise(AstAllocator* allocator);

// Functions for creating and maniputing types
QualifiedType type_create_pointer(AstAllocator* allocator,
        QualifiedType base_type, TypeQualifiers qualifiers);
QualifiedType type_pointer_get_pointee(const QualifiedType* pointer);

QualifiedType type_create_array(AstAllocator* allocator,
        QualifiedType element_type, union Expression* expression, size_t length,
        bool is_static, bool is_star, bool is_vla);
QualifiedType type_array_get_element_type(const QualifiedType* type);
size_t type_array_get_length(const QualifiedType* type);
union Expression* type_array_get_expression(const QualifiedType* type);
bool type_array_is_complete(const QualifiedType* type);

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
QualifiedType type_function_get_return(const QualifiedType* function);
size_t type_function_get_param_count(const QualifiedType* function);
TypeFunctionParameter* type_function_get_params(const QualifiedType* function);
bool type_function_is_variadic(const QualifiedType* function);
bool type_function_get_knr(const QualifiedType* type);

TypeQualifiers qualified_type_get_quals(const QualifiedType* type);
QualifiedType qualified_type_remove_quals(const QualifiedType* type);
QualifiedType qualified_type_add_quals(const QualifiedType* type,
        TypeQualifiers quals);

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
void type_struct_set_size(Type* type, size_t size, size_t align);
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

size_t qualified_type_get_size(const QualifiedType* type);
size_t qualified_type_get_align(const QualifiedType* type);

bool qualified_type_is_integer(const QualifiedType* type);
size_t qualified_type_get_rank(const QualifiedType* type);
bool qualified_type_is_signed(const QualifiedType* type);
bool qualified_type_is_unsigned(const QualifiedType* type);
bool qualified_type_is_arithmetic(const QualifiedType* type);
bool qualified_type_is_scaler(const QualifiedType* type);
bool qualified_type_is_pointer(const QualifiedType* type);
bool qualified_type_is_array(const QualifiedType* type);
bool qualified_type_is_compound(const QualifiedType* type);

QualifiedType type_get_canonical(const Type* type);
QualifiedType qualified_type_get_canonical(const QualifiedType* type);

bool qualified_type_is_compatible(const QualifiedType* t1,
        const QualifiedType* t2);
bool qualified_type_is_compatible_no_quals(const QualifiedType* t1,
        const QualifiedType* t2);

bool qualified_type_builtin_equal(const QualifiedType* t1,
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
