#ifndef AST_TYPE_H
#define AST_TYPE_H

#include "ast/ast_fwd.h"

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_CHAR,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LONG_LONG,
    TYPE_ULONG_LONG,
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
    QUALIFIER_VOLATILE = 1 << 2,
    QUALIFIER_ATOMIC = 1 << 3
} TypeQualifiers;

typedef enum StorageSpecifier {
    STORAGE_NONE,
    STORAGE_TYPEDEF,
    STORAGE_EXTERN,
    STORAGE_STATIC,
    STORAGE_AUTO,
    STORAGE_REGISTER
} StorageSpecifier;

typedef enum FunctionSpecifier {
    FUNCTION_SPECIFIER_NONE = 0,
    FUNCTION_SPECIFIER_INLINE = 1 << 0
} FunctionSpecifier;

typedef enum TypeSpecifier {
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
} TypeSpecifier;

typedef enum WidthSpecifier {
    WIDTH_SPECIFIER_NONE = 0,
    WIDTH_SPECIFIER_SHORT = 1 << 0,
    WIDTH_SPECIFIER_LONG = 1 << 1,
    WIDTH_SPECIFIER_LONG_LONG = 1 << 2
} WidthSpecifier;

typedef enum SignSpecifier {
    SIGN_SPECIFIER_NONE,
    SIGN_SPECIFIER_SIGNED,
    SIGN_SPECIFIER_UNSIGNED,
} SignSpecifier;

typedef enum ComplexSpecifier {
    COMPLEX_SPECIFIER_NONE,
    COMPLEX_SPECIFIER_COMPLEX,
    COMPLEX_SPECIFIER_IMAGINAIRY
} ComplexSpecifier;

// Simple structure to hold a qualified type stores is as the qualifiers and the
// base type itself. Meant to be fast to create and able to just be stored on
// the stack.
typedef struct QualifierType {
    TypeQualifiers quals;
    Type* type;
} QualifiedType;

#endif /* AST_TYPE_H */
