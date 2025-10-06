#include "type.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parse/ast_allocator.h"
#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"
#include "util/xmalloc.h"

const char* storage_specifier_to_name(TypeStorageSpecifier specifier)
{
    switch (specifier)
    {
        case TYPE_STORAGE_SPECIFIER_NONE: return "<internal-error>";
        case TYPE_STORAGE_SPECIFIER_AUTO: return "auto";
        case TYPE_STORAGE_SPECIFIER_EXTERN: return "extern";
        case TYPE_STORAGE_SPECIFIER_REGISTER: return "register";
        case TYPE_STORAGE_SPECIFIER_STATIC: return "static";
        case TYPE_STORAGE_SPECIFIER_TYPEDEF: return "typedef";
    }
}

const char* type_qualifier_to_name(TypeQualifiers qualifier)
{
    switch (qualifier)
    {
        case TYPE_QUALIFIER_NONE: return "<internal-error>";
        case TYPE_QUALIFIER_CONST: return "const";
        case TYPE_QUALIFIER_RESTRICT: return "restrict";
        case TYPE_QUALIFIER_VOLATILE: return "volatile";
    }
}

const char* function_specifier_to_name(TypeFunctionSpecifier function)
{
    switch (function)
    {
        case TYPE_FUNCTION_SPECIFIER_NONE: return "<internal-error>";
        case TYPE_FUNCTION_SPECIFIER_INLINE: return "inline";
    }
}

const char* width_specifier_to_name(TypeSpecifierWidth width)
{
    switch (width)
    {
        case TYPE_SPECIFIER_WIDTH_NONE: return "<internal-error>";
        case TYPE_SPECIFIER_WIDTH_SHORT: return "short";
        case TYPE_SPECIFIER_WIDTH_LONG: return "long";
        case TYPE_SPECIFIER_WIDTH_LONG_LONG: return "long long";
    }
}

const char* sign_specifier_to_name(TypeSpecifierSign sign)
{
    switch (sign)
    {
        case TYPE_SPECIFIER_SIGN_NONE: return "<internal-error>";
        case TYPE_SPECIFIER_SIGN_SIGNED: return "signed";
        case TYPE_SPECIFIER_SIGN_UNSIGNED: return "unsigned";
    }
}

const char* complex_specifier_to_name(TypeSpecifierComplex complex)
{
    switch (complex)
    {
        case TYPE_SPECIFIER_COMPLEX_NONE: return "<internal-error>";
        case TYPE_SPECIFIER_COMPLEX_COMPLEX: return "complex";
        case TYPE_SPECIFIER_COMPLEX_IMAGINAIRY: return "imaginairy";
    }
}

const char* type_specifier_to_name(TypeSpecifierType type)
{
    switch (type)
    {
        case TYPE_SPECIFIER_TYPE_NONE: return "<internal-error>";
        case TYPE_SPECIFIER_TYPE_VOID: return "void";
        case TYPE_SPECIFIER_TYPE_CHAR: return "char";
        case TYPE_SPECIFIER_TYPE_INT: return "int";
        case TYPE_SPECIFIER_TYPE_FLOAT: return "float";
        case TYPE_SPECIFIER_TYPE_DOUBLE: return "double";
        case TYPE_SPECIFIER_TYPE_BOOL: return "bool";
        case TYPE_SPECIFIER_TYPE_ENUM: return "enum";
        case TYPE_SPECIFIER_TYPE_STRUCT: return "struct";
        case TYPE_SPECIFIER_TYPE_UNION: return "union";
        case TYPE_SPECIFIER_TYPE_TYPENAME: return "type-name";
    }
}

bool type_qualifier_is_const(TypeQualifiers qualifiers)
{
    return (qualifiers & TYPE_QUALIFIER_CONST) != 0;
}

bool type_qualifier_is_restrict(TypeQualifiers qualifiers)
{
    return (qualifiers & TYPE_QUALIFIER_RESTRICT) != 0;
}

bool type_qualifier_is_volatile(TypeQualifiers qualifiers)
{
    return (qualifiers & TYPE_QUALIFIER_VOLATILE) != 0;
}

bool type_qualifier_already_has(TypeQualifiers qualifiers, TypeQualifiers has)
{
    return (qualifiers & has) != 0;
}

static Type* type_create_builtin(AstAllocator* allocator, TypeKind kind,
        size_t size, size_t align, bool complete)
{
    Type* type = ast_allocator_alloc(allocator, sizeof(TypeBase));
    type->type_base.type = kind;
    type->type_base.type_size = size;
    type->type_base.type_alignment = align;
    type->type_base.is_complete = complete;

    return type;
}

TypeBuiltins type_builtins_initialise(AstAllocator* allocator)
{
    TypeBuiltins builtins;
    builtins.type_void = type_create_builtin(allocator, TYPE_VOID, 0, 0, false);    
    builtins.type_char = type_create_builtin(allocator, TYPE_CHAR, 1, 1, true);
    builtins.type_unsigned_char = type_create_builtin(allocator, TYPE_U_CHAR, 1,
            1, true);
    builtins.type_signed_char = builtins.type_char;
    builtins.type_unsigned_short = type_create_builtin(allocator, TYPE_U_SHORT,
            2, 2, true);
    builtins.type_signed_short = type_create_builtin(allocator, TYPE_S_SHORT,
            2, 2, true);
    builtins.type_unsigned_int = type_create_builtin(allocator, TYPE_U_INT,
            4, 4, true);
    builtins.type_signed_int = type_create_builtin(allocator, TYPE_S_INT,
            4, 4, true);
    builtins.type_unsigned_long = type_create_builtin(allocator, TYPE_U_LONG,
            8, 8, true);
    builtins.type_signed_long = type_create_builtin(allocator, TYPE_S_LONG,
            8, 8, true);
    builtins.type_unsigned_long_long = type_create_builtin(allocator, 
            TYPE_U_LONG_LONG, 8, 8, true);
    builtins.type_signed_long_long = type_create_builtin(allocator, 
            TYPE_S_LONG_LONG, 8, 8, true);
    builtins.type_float = type_create_builtin(allocator, TYPE_FLOAT, 4, 4, true);
    builtins.type_double = type_create_builtin(allocator, TYPE_DOUBLE, 8, 8, true);
    builtins.type_long_double = type_create_builtin(allocator, TYPE_LONG_DOUBLE,
            16, 16, true);

    return builtins;
}

Type* type_create_pointer(AstAllocator* allocator, Type* base_type,
        TypeQualifiers qualifiers);

bool qualified_type_is_equal(const QualifiedType* t1, const QualifiedType* t2)
{
    return t1->type == t2->type && t1->qualifiers == t2->qualifiers;
}

bool qualifier_type_is_equal_canonical(const QualifiedType* t1, 
        const QualifiedType* t2)
{
    return t1->type == t2->type;
}

