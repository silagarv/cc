#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stddef.h>

#include "util/str.h"

#include "driver/target.h"

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
    TYPE_ENUM,
    TYPE_FUNCTION,
    TYPE_POINTER,
    TYPE_TYPEDEF
} TypeKind;

typedef union Type Type;

// The base type telling us how to interpret it
typedef struct TypeBase {
    TypeKind type;

    size_t type_size;
    size_t type_alignment;
} TypeBase;

// Both imaginairy and complex types have an underlying type which is either
// float, double, or long double
typedef struct TypeUnreal {
    TypeBase type;
    Type* underlying_type;
} TypeUnreal;

// An array has an underlying element type and the length of an array may either
// be known or unknown and we may have done all of the counting ourselves...
typedef struct TypeArray {
    TypeBase type;
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

    bool is_bitfield; // is the member a bitfield
    size_t bitfield_size; // Size of the members bitfield if needed
    
    struct TypeCompoundMember* next; // the next member in the struct or NULL
} TypeCompoundMember;

typedef struct TypeCompound {
    TypeBase type_base;
    String name; // name may be given or could be anonymous but we give it one

    TypeCompoundMember* first_member; // the members of the compound type
    TypeCompoundMember* last_member; // the last member in the struct

    bool is_anonamous; // e.g. struct { int x;} a;
} TypeCompound;

// A struct to represent an enum type
typedef struct TypeEnumConstant {
    String name; // the name of the member
    /* TODO: need to have some kind of value? but is this the right place... */

    struct TypeEnumConstant* next;
} TypeEnumConstant;

typedef struct TypeEnum {
    TypeBase type_base;
    String name; // the name of the member of mangled if anon
    Type* underlying_type; // the underlying type of the enum

    TypeEnumConstant* first; // the first member of enum
    TypeEnumConstant* last; // The last member of the enum

    bool is_anonamous; // if it is ananonamous enum
} TypeEnum;

// A function type
typedef struct TypeFunctionParam {
    String name;
    Type* type;
    struct TypeFunctionParam* next;
} TypeFunctionParam;

typedef struct TypeFunction {
    TypeBase type_base;
    Type* return_type; // Functions return type

    TypeFunctionParam* first_param; // the first paramter
    TypeFunctionParam* last_param; // the last memeber 

    size_t num_params; // Number of params if known otherwise 0

    bool unspecified_paramters; // e.g. int foo()
    bool is_variadic; // e.g. int foo(int, ...)
} TypeFunction;

// A type to a pointer
typedef struct TypePointer {
    TypeBase type_base;
    Type* underlying_type; // the type beneath could also be a pointer
} TypePointer;

// And a typedef
typedef struct TypeTypedef {
    TypeBase type_base;
    String name; // name of the typedef for looking up
    Type* resolved_type; // the fully underlying type resolved all the way
} TypeTypedef;

union Type {
    TypeBase type_base;
    TypeUnreal type_imaginary;
    TypeUnreal type_complex;
    TypeArry type_array;
    TypeCompound type_struct;
    TypeCompound type_union;
    TypeEnum type_enum;
    TypeFunction type_function;
    TypePointer type_pointer;
    TypeTypedef type_typedef;
};

typedef enum TypeQualifiers {
    TYPE_QUALIFIER_NONE = 0,
    TYPE_QUALIFIER_CONST = 1 << 0,
    TYPE_QUALIFIER_RESTRICT = 1 << 1,
    TYPE_QUALIFIER_VOLATILE = 1 << 2
} TypeQualifiers;

// Only one storage specifier can be applied to a variable
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

typedef struct QualifiedType {
    TypeQualifiers qualifiers;
    Type* type;
} QualifiedType;

bool type_qualifier_is_const(TypeQualifiers qualifiers);
bool type_qualifier_is_restrict(TypeQualifiers qualifiers);
bool type_qualifier_is_volatile(TypeQualifiers qualifiers);
bool type_qualifier_already_has(TypeQualifiers qualifiers, TypeQualifiers has);

// Functions to create our basic types
Type* type_create_error(void);
Type* type_create_void(void);
Type* type_create_bool(void);
Type* type_create_char(void);
Type* type_create_signed_char(void);
Type* type_create_unsigned_char(void);
Type* type_create_signed_short(void);
Type* type_create_unsigned_short(void);
Type* type_create_signed_int(void);
Type* type_create_unsigned_int(void);
Type* type_create_signed_long(void);
Type* type_create_unsigned_long(void);
Type* type_create_signed_long_long(void);
Type* type_create_unsigned_long_long(void);
Type* type_create_float(void);
Type* type_create_double(void);
Type* type_create_long_double(void);
Type* type_create_complex(Type* base_type);
Type* type_create_imaginary(Type* base_type);

void type_free(Type* type);

// Function to transform a type to a string for when it is needed for diagnostics
String type_to_string(Type* type);

// Any type that is not user defined. (TODO: Should I include typedefs?)
bool is_builtin_type(Type* type);

bool is_arithmetic_type(Type* type);
bool is_pointer(Type* type);

bool is_type_equal_no_qualifiers(Type* type);
bool is_type_equal(Type* type);

#endif /* TYPE_H */
