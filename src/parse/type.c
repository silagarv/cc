#include "type.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#include "parse/ast_allocator.h"
#include "parse/declaration.h"
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
        case TYPE_SPECIFIER_COMPLEX_COMPLEX: return "_Complex";
        case TYPE_SPECIFIER_COMPLEX_IMAGINAIRY: return "_Imaginairy";
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
    builtins.type_bool = type_create_builtin(allocator, TYPE_BOOL, 1, 1, true);

    return builtins;
}

static Type* type_create_base(AstAllocator* allocator, size_t alloc_size,
        TypeKind kind, size_t size, size_t align, bool complete)
{
    Type* type = ast_allocator_alloc(allocator, alloc_size);
    type->type_base.type = kind;
    type->type_base.type_size = size;
    type->type_base.type_alignment = align;
    type->type_base.is_complete = complete;

    return type;
}

QualifiedType type_create_pointer(AstAllocator* allocator,
        QualifiedType* base_type, TypeQualifiers qualifiers)
{
    // Simply create a pointer type and set the underlying type
    Type* type = type_create_base(allocator, sizeof(TypePointer), TYPE_POINTER,
            8, 8, true);
    type->type_pointer.underlying_type = *base_type;

    return (QualifiedType) {qualifiers, type};
}

QualifiedType type_create_array(AstAllocator* allocator,
        QualifiedType* element_type, size_t length, bool is_static,
        bool is_star, bool is_vla)
{
    // Get the size of the array if the length if known
    size_t size = length * element_type->type->type_base.type_size;

    // Array of unknown length are incomplete types
    bool is_complete = (length == 0) ? false : true;

    // Need to determine the size and align or the array.
    Type* type = type_create_base(allocator, sizeof(TypeArray), TYPE_ARRAY,
            size, element_type->type->type_base.type_alignment, is_complete);
    type->type_array.element_type = *element_type;
    type->type_array.length = length;
    type->type_array.is_static = is_static;
    type->type_array.is_star = is_star;
    type->type_array.is_vla = is_vla;

    // Cannot really add qualifiers to arrays
    return (QualifiedType) {TYPE_QUALIFIER_NONE, type};
}

QualifiedType type_create_function(AstAllocator* allocator,
        QualifiedType* return_type, QualifiedType** paramaters,
        size_t num_paramaters, bool unspecified_paramters, bool variadic)
{


    return (QualifiedType) {0};
}

QualifiedType type_create_enum(AstAllocator* allocator, Type* base)
{
    size_t base_size = base->type_base.type_size;
    size_t align_size = base->type_base.type_alignment;
    bool complete = base->type_base.is_complete;
    Type* type = type_create_base(allocator, sizeof(TypeEnum), TYPE_ENUM,
            base_size, align_size, complete);
    type->type_enum.enum_decl = NULL;

    return (QualifiedType) {TYPE_QUALIFIER_NONE, type};
}

void type_enum_set_declaration(QualifiedType* enum_type,
        union Declaration* decl)
{
    assert(enum_type->type->type_enum.enum_decl == NULL);

    enum_type->type->type_enum.enum_decl = decl;
}

Type* type_create_struct(AstAllocator* allocator)
{
    Type* type = type_create_base(allocator, sizeof(TypeCompound),
            TYPE_STRUCT, 0, 0, false);
    type->type_struct.decl = NULL;
    type->type_struct.members = NULL;
    type->type_struct.num_members = 0;

    return type;
}

void type_struct_set_declaration(Type* type, union Declaration* declaration)
{
    type->type_struct.decl = declaration;
}

bool type_struct_is_complete(Type* type)
{
    return type->type_struct.base.is_complete;
}

// TODO: make sure we set the members

Type* type_create_union(AstAllocator* allocator);
void type_union_set_declaration(Type* type, union Declaration* declaration);
bool type_union_is_complete(Type* type);
// TODO: make sure we set the members

Type* type_create_typedef(AstAllocator* allocator, QualifiedType type,
        union Declaration* decl)
{
    size_t type_size = type.type->type_base.type_size;
    size_t type_align = type.type->type_base.type_alignment;
    bool complete = type.type->type_base.is_complete;

    Type* new_type = type_create_base(allocator, sizeof(TypeTypedef),
            TYPE_TYPEDEF, type_size, type_align, complete);
    new_type->type_typedef.underlying_type = type;
    new_type->type_typedef.tdef = decl;

    return new_type;
}

bool qualified_type_is(const QualifiedType* type, TypeKind kind)
{
    if (type->type == NULL)
    {
        return false;
    }

    return type->type->type_base.type == kind;
}

bool type_is_builtin(const Type* t1)
{
    return false;
}

bool type_is_equal(const Type* t1, const Type* t2)
{
    // If different types then obviously not equal
    if (t1->type_base.type != t2->type_base.type)
    {
        return false;
    }

    // If they are both the same builtin type then they are equal
    if (type_is_builtin(t1))
    {
        return true;
    }

    // Otherwise for the non-builtin types, we need to check they refer to the
    // same thing.
    switch (t1->type_base.type)
    {
        default:
            break;
    }

    return false;
}

bool qualified_type_is_equal(const QualifiedType* t1, const QualifiedType* t2)
{
    return t1->type == t2->type && t1->qualifiers == t2->qualifiers;
}

bool qualifier_type_is_equal_canonical(const QualifiedType* t1, 
        const QualifiedType* t2)
{
    return t1->type == t2->type;
}

void type_print(const QualifiedType* t1)
{
    if (t1->qualifiers & TYPE_QUALIFIER_CONST)
    {
        printf("const ");
    }

    if (t1->qualifiers & TYPE_QUALIFIER_VOLATILE)
    {
        printf("volatile ");
    }

    if (t1->qualifiers & TYPE_QUALIFIER_RESTRICT)
    {
        printf("restrict ");
    }

    switch (t1->type->type_base.type)
    {
        case TYPE_VOID:
            printf("void ");
            break;

        case TYPE_S_INT:
            printf("int ");
            break;

        case TYPE_U_INT:
            printf("unsigned int ");
            break;

        case TYPE_S_SHORT:
            printf("short ");
            break;

        case TYPE_U_SHORT:
            printf("unsigned short");
            break;

        case TYPE_CHAR:
            printf("char");
            break;

        case TYPE_ENUM:
            printf("enum %s",
                    t1->type->type_enum.enum_decl->base.identifier->string.ptr);
            break;

        case TYPE_POINTER:
            printf("pointer to ");
            type_print(&t1->type->type_pointer.underlying_type);
            break;

        case TYPE_ARRAY:
            printf("array of ");
            type_print(&t1->type->type_array.element_type);
            break;

        case TYPE_TYPEDEF:
            printf("typename '%s'",
                    t1->type->type_typedef.tdef->base.identifier->string.ptr);
            break;
    }
}

