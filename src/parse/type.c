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

const char* storage_specifier_to_name(StorageSpecifier specifier)
{
    switch (specifier)
    {
        case STORAGE_NONE: return "<internal-error>";
        case STORAGE_AUTO: return "auto";
        case STORAGE_EXTERN: return "extern";
        case STORAGE_REGISTER: return "register";
        case STORAGE_STATIC: return "static";
        case STORAGE_TYPEDEF: return "typedef";
    }
}

const char* type_qualifier_to_name(TypeQualifiers qualifier)
{
    switch (qualifier)
    {
        case QUALIFIER_NONE: return "<internal-error>";
        case QUALIFIER_CONST: return "const";
        case QUALIFIER_RESTRICT: return "restrict";
        case QUALIFIER_VOLATILE: return "volatile";
    }
}

const char* function_specifier_to_name(TypeFunctionSpecifier function)
{
    switch (function)
    {
        case FUNCTION_SPECIFIER_NONE: return "<internal-error>";
        case FUNCTION_SPECIFIER_INLINE: return "inline";
    }
}

const char* width_specifier_to_name(TypeSpecifierWidth width)
{
    switch (width)
    {
        case WIDTH_SPECIFIER_NONE: return "<internal-error>";
        case WIDTH_SPECIFIER_SHORT: return "short";
        case WIDTH_SPECIFIER_LONG: return "long";
        case WIDTH_SPECIFIER_LONG_LONG: return "long long";
    }
}

const char* sign_specifier_to_name(TypeSpecifierSign sign)
{
    switch (sign)
    {
        case SIGN_SPECIFIER_NONE: return "<internal-error>";
        case SIGN_SPECIFIER_SIGNED: return "signed";
        case SIGN_SPECIFIER_UNSIGNED: return "unsigned";
    }
}

const char* complex_specifier_to_name(TypeSpecifierComplex complex)
{
    switch (complex)
    {
        case COMPLEX_SPECIFIER_NONE: return "<internal-error>";
        case COMPLEX_SPECIFIER_COMPLEX: return "_Complex";
        case COMPLEX_SPECIFIER_IMAGINAIRY: return "_Imaginairy";
    }
}

const char* type_specifier_to_name(TypeSpecifierType type)
{
    switch (type)
    {
        case TYPE_SPECIFIER_NONE: return "<internal-error>";
        case TYPE_SPECIFIER_VOID: return "void";
        case TYPE_SPECIFIER_CHAR: return "char";
        case TYPE_SPECIFIER_INT: return "int";
        case TYPE_SPECIFIER_FLOAT: return "float";
        case TYPE_SPECIFIER_DOUBLE: return "double";
        case TYPE_SPECIFIER_BOOL: return "bool";
        case TYPE_SPECIFIER_ENUM: return "enum";
        case TYPE_SPECIFIER_STRUCT: return "struct";
        case TYPE_SPECIFIER_UNION: return "union";
        case TYPE_SPECIFIER_TYPENAME: return "type-name";
    }
}

bool type_qualifier_has_any(TypeQualifiers qualifiers)
{
    return (qualifiers != QUALIFIER_NONE);
}

bool type_qualifier_is_const(TypeQualifiers qualifiers)
{
    return (qualifiers & QUALIFIER_CONST) != 0;
}

bool type_qualifier_is_restrict(TypeQualifiers qualifiers)
{
    return (qualifiers & QUALIFIER_RESTRICT) != 0;
}

bool type_qualifier_is_volatile(TypeQualifiers qualifiers)
{
    return (qualifiers & QUALIFIER_VOLATILE) != 0;
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

static Type* type_create_error(AstAllocator* allocator)
{
    assert(sizeof(TypeBase) == sizeof(TypeError));

    Type* type = type_create_builtin(allocator, TYPE_ERROR, 0, 0, false);

    return type;
}

TypeBuiltins type_builtins_initialise(AstAllocator* allocator)
{
    TypeBuiltins builtins;
    builtins.type_error = type_create_error(allocator);
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
        QualifiedType base_type, TypeQualifiers qualifiers)
{
    // Simply create a pointer type and set the underlying type
    Type* type = type_create_base(allocator, sizeof(TypePointer), TYPE_POINTER,
            8, 8, true);
    type->type_pointer.underlying_type = base_type;

    return (QualifiedType) {qualifiers, type};
}

QualifiedType type_pointer_get_pointee(const QualifiedType* pointer)
{
    assert(qualified_type_is(pointer, TYPE_POINTER));

    return pointer->type->type_pointer.underlying_type;
}

QualifiedType type_create_array(AstAllocator* allocator,
        QualifiedType element_type, size_t length, bool is_static,
        bool is_star, bool is_vla)
{
    // Get the size of the array if the length if known
    size_t size = length * element_type.type->type_base.type_size;

    // Array of unknown length are incomplete types
    bool is_complete = (length == 0) ? false : true;

    // Need to determine the size and align or the array.
    Type* type = type_create_base(allocator, sizeof(TypeArray), TYPE_ARRAY,
            size, element_type.type->type_base.type_alignment, is_complete);
    type->type_array.element_type = element_type;
    type->type_array.length = length;
    type->type_array.is_static = is_static;
    type->type_array.is_star = is_star;
    type->type_array.is_vla = is_vla;

    // Cannot really add qualifiers to arrays
    return (QualifiedType) {QUALIFIER_NONE, type};
}

QualifiedType type_array_get_element_type(const QualifiedType* type)
{
    assert(qualified_type_is(type, TYPE_ARRAY));

    return type->type->type_array.element_type;
}

TypeFunctionParameter* type_create_function_parameter(AstAllocator* allocator,
        QualifiedType type)
{
    TypeFunctionParameter* param = ast_allocator_alloc(allocator,
            sizeof(TypeFunctionParameter));
    param->type = type;
    param->next = NULL;

    return param;
}

void type_function_parameter_set_next(TypeFunctionParameter* param,
        TypeFunctionParameter* next)
{
    param->next = next;
}

TypeFunctionParameter* type_function_parameter_get_next(
        TypeFunctionParameter* param)
{
    return param->next;
}

QualifiedType type_function_parameter_get_type(TypeFunctionParameter* param)
{
    return param->type;
}

TypeQualifiers qualified_type_get_quals(const QualifiedType* type)
{
    return type->qualifiers;
}

QualifiedType qualified_type_remove_quals(const QualifiedType* type)
{
    return (QualifiedType) {QUALIFIER_NONE, type->type};
}

QualifiedType type_create_function(AstAllocator* allocator,
        QualifiedType return_type, TypeFunctionParameter* paramaters,
        size_t num_paramaters, bool unspecified_paramters, bool variadic)
{
    Type* type = type_create_base(allocator, sizeof(TypeFunction),
            TYPE_FUNCTION, 0, 0, true);
    type->type_function.return_type = return_type;
    type->type_function.paramaters = paramaters;
    type->type_function.num_paramaters = num_paramaters;
    type->type_function.unspecified_paramters = unspecified_paramters;
    type->type_function.is_variadic = variadic;

    return (QualifiedType) {QUALIFIER_NONE, type};
}

QualifiedType type_function_get_return(const QualifiedType* function)
{
    assert(qualified_type_is(function, TYPE_FUNCTION));

    return function->type->type_function.return_type;
}

QualifiedType type_create_enum(AstAllocator* allocator, Type* base)
{
    size_t base_size = base->type_base.type_size;
    size_t align_size = base->type_base.type_alignment;
    bool complete = false;
    Type* type = type_create_base(allocator, sizeof(TypeEnum), TYPE_ENUM,
            base_size, align_size, complete);
    type->type_enum.enum_decl = NULL;

    return (QualifiedType) {QUALIFIER_NONE, type};
}

void type_enum_set_declaration(QualifiedType* enum_type,
        union Declaration* decl)
{
    assert(enum_type->type->type_enum.enum_decl == NULL);

    enum_type->type->type_enum.enum_decl = decl;
}

void type_enum_set_complete(QualifiedType* enum_type)
{
    assert(qualified_type_is(enum_type, TYPE_ENUM));

    enum_type->type->type_base.is_complete = true;
}

bool type_enum_is_complete(const QualifiedType* enum_type)
{
    assert(qualified_type_is(enum_type, TYPE_ENUM));

    return enum_type->type->type_base.is_complete;
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

union Declaration* qualified_type_struct_get_declaration(QualifiedType* type)
{
    assert(qualified_type_is_compound(type));

    QualifiedType real_type = qualified_type_get_canonical(type);
    return real_type.type->type_struct.decl;
}

bool type_struct_is_complete(Type* type)
{
    return type->type_struct.base.is_complete;
}

void type_struct_set_complete(Type* type)
{
    type->type_struct.base.is_complete = true;
}

Type* type_create_union(AstAllocator* allocator)
{
    Type* type = type_create_base(allocator, sizeof(TypeCompound),
            TYPE_UNION, 0, 0, false);
    type->type_union.decl = NULL;
    type->type_union.members = NULL;
    type->type_union.num_members = 0;

    return type;
}

void type_union_set_declaration(Type* type, union Declaration* declaration)
{
    type->type_union.decl = declaration;
}

bool type_union_is_complete(Type* type)
{
    return type->type_union.base.is_complete;
}

void type_union_set_complete(Type* type)
{
    type->type_union.base.is_complete = true;
}

// TODO: make sure we set the members

Type* type_create_typedef(AstAllocator* allocator, QualifiedType type,
        union Declaration* decl)
{
    size_t type_size = type.type->type_base.type_size;
    size_t type_align = type.type->type_base.type_alignment;
    bool complete = type.type->type_base.is_complete;

    // Here even with multiple typedef's we will get the real type very fast
    // since we don't let a long chain form.
    QualifiedType real_type = type;
    if (qualified_type_is(&real_type, TYPE_TYPEDEF))
    {
        // If it's a typedef just go down the chain. Otherwise, we will need to
        // check specifically for anonymous structs and unions so redo their
        // type properly
        if (qualified_type_is(&real_type, TYPE_TYPEDEF))
        {
            real_type = type.type->type_typedef.real_type;
        }
        else
        {
            // TODO:
            real_type = type.type->type_typedef.real_type;
        }
    }

    Type* new_type = type_create_base(allocator, sizeof(TypeTypedef),
            TYPE_TYPEDEF, type_size, type_align, complete);
    new_type->type_typedef.real_type = real_type;   
    new_type->type_typedef.underlying_type = type;
    new_type->type_typedef.tdef = decl;

    return new_type;
}

QualifiedType qualified_type_typedef_get_underlying_type(
        const QualifiedType* type)
{
    assert(qualified_type_is(type, TYPE_TYPEDEF));

    return type->type->type_typedef.underlying_type;
}

QualifiedType qualified_type_typedef_get_real_type(const QualifiedType* type)
{
    return type->type->type_typedef.real_type;
}

Type* qualified_type_get_raw(const QualifiedType* type)
{
    return type->type;
}

TypeKind qualified_type_get_kind(const QualifiedType* type)
{
    if (type == NULL)
    {
        return TYPE_ERROR;
    }

    return type->type->type_base.type;
}

bool type_is(const Type* type, TypeKind kind)
{
    if (type == NULL)
    {
        return false;
    }

    return type->type_base.type == kind;
}

bool qualified_type_is(const QualifiedType* type, TypeKind kind)
{
    if (type->type == NULL)
    {
        return false;
    }

    return type->type->type_base.type == kind;
}

size_t qualified_type_get_size(const QualifiedType* type)
{
    return type->type->type_base.type_size;
}

bool qualified_type_is_integer(const QualifiedType* type)
{
    if (type->type == NULL)
    {
        return false;
    }

    QualifiedType real_type = qualified_type_get_canonical(type);
    switch (qualified_type_get_kind(&real_type))
    {
        case TYPE_BOOL: // TODO: is this correct
        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_U_CHAR:
        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
        case TYPE_S_INT:
        case TYPE_U_INT:
        case TYPE_S_LONG:
        case TYPE_U_LONG:
        case TYPE_S_LONG_LONG:
        case TYPE_U_LONG_LONG:
            return true;

        // From clang we only allow complete enum declarations to be considered
        // pointers.
        case TYPE_ENUM:
        {
            Declaration* enum_decl = type->type->type_enum.enum_decl;
            return declaration_enum_has_entries(enum_decl);
        }

        default:
            return false;
    }
}

size_t qualified_type_get_rank(const QualifiedType* type)
{
    assert(qualified_type_is_integer(type));

    switch (qualified_type_get_kind(type))
    {
        case TYPE_BOOL:
            return 1;

        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_U_CHAR:
            return 2;

        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
            return 3;

        case TYPE_S_INT:
        case TYPE_U_INT:
        case TYPE_ENUM:
            return 4;

        case TYPE_S_LONG:
        case TYPE_U_LONG:
            return 5;

        case TYPE_S_LONG_LONG:
        case TYPE_U_LONG_LONG:
            return 6;

        default:
            panic("unreachable");
            return 0;
    }
}

bool qualified_type_is_signed(const QualifiedType* type)
{
    assert(qualified_type_is_integer(type));

    QualifiedType real_type = qualified_type_get_canonical(type);
    switch (qualified_type_get_kind(&real_type))
    {
        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_S_SHORT:
        case TYPE_S_INT:
        case TYPE_S_LONG:
        case TYPE_S_LONG_LONG:
            return true;

        default:
            return false;
    }
}

bool qualified_type_is_unsigned(const QualifiedType* type)
{
    assert(qualified_type_is_integer(type));

    QualifiedType real_type = qualified_type_get_canonical(type);
    switch (qualified_type_get_kind(&real_type))
    {
        case TYPE_U_CHAR:
        case TYPE_U_SHORT:
        case TYPE_U_INT:
        case TYPE_U_LONG:
        case TYPE_U_LONG_LONG:
            return true;

        default:
            return false;
    }
}

bool qualified_type_is_arithmetic(const QualifiedType* type)
{
    QualifiedType real_type = qualified_type_get_canonical(type);
    if (qualified_type_is_integer(&real_type))
    {
        return true;
    }

    if (qualified_type_is(&real_type, TYPE_FLOAT))
    {
        return true;
    }

    if (qualified_type_is(&real_type, TYPE_DOUBLE))
    {
        return true;
    }

    if (qualified_type_is(&real_type, TYPE_LONG_DOUBLE))
    {
        return true;
    }

    // NOTE: enum case is already covered

    return false;
}

bool qualified_type_is_scaler(const QualifiedType* type)
{
    QualifiedType real_type = qualified_type_get_canonical(type);
    if (qualified_type_is_arithmetic(&real_type))
    {
        return true;
    }

    if (qualified_type_is(&real_type, TYPE_POINTER))
    {
        return true;
    }

    return false;
}

bool qualified_type_is_compound(const QualifiedType* type)
{
    QualifiedType real_type = qualified_type_get_canonical(type);

    return qualified_type_is(&real_type, TYPE_STRUCT) ||
            qualified_type_is(&real_type, TYPE_UNION);
}

QualifiedType type_get_canonical(const Type* type)
{
    if (type == NULL)
    {
        return (QualifiedType) {0};
    }

    if (type_is(type, TYPE_TYPEDEF))
    {
        return type->type_typedef.real_type;
    }

    return (QualifiedType) {QUALIFIER_NONE, (Type*) type};
}

QualifiedType qualified_type_get_canonical(const QualifiedType* type)
{
    QualifiedType qual_type = type_get_canonical(type->type);

    // Get all of the possible type qualifiers
    TypeQualifiers qualifiers = QUALIFIER_NONE;
    qualifiers |= type->qualifiers;
    qualifiers |= qual_type.qualifiers;
    
    qual_type.qualifiers = qualifiers;

    return qual_type;
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

bool qualified_type_is_compatible(const QualifiedType* t1,
        const QualifiedType* t2)
{
    QualifiedType real_t1 = qualified_type_get_canonical(t1);
    QualifiedType real_t2 = qualified_type_get_canonical(t2);

    // If the qualifiers aren't compatible then the types arent
    if (real_t1.qualifiers != real_t2.qualifiers)
    {
        return false;
    }

    TypeKind kind_t1 = real_t1.type->type_base.type;
    TypeKind kind_t2 = real_t2.type->type_base.type;
    if (kind_t1 != kind_t2)
    {
        return false;
    }

    switch (kind_t1)
    {
        case TYPE_VOID:
        case TYPE_BOOL:
        case TYPE_CHAR:
        case TYPE_S_CHAR:
        case TYPE_U_CHAR:
        case TYPE_S_SHORT:
        case TYPE_U_SHORT:
        case TYPE_S_INT:
        case TYPE_U_INT:
        case TYPE_S_LONG:
        case TYPE_U_LONG:
        case TYPE_S_LONG_LONG:
        case TYPE_U_LONG_LONG:
        case TYPE_FLOAT:
        case TYPE_DOUBLE:
        case TYPE_LONG_DOUBLE:
            return true;

        case TYPE_IMAGINARY:
        case TYPE_COMPLEX:
            panic("unimplemented");
            return false;

        case TYPE_ARRAY:
        {
            TypeArray arr1 = real_t1.type->type_array;
            TypeArray arr2 = real_t2.type->type_array;

            // If the element types of the arrays aren't compatible then they
            // cannot be compatible
            if (!qualified_type_is_compatible(&arr1.element_type,
                    &arr2.element_type))
            {
                return false;
            }

            if (arr1.length != arr2.length)
            {
                return false;
            }

            // TODO: finish this properly

            return true;
        }

        // struct, union, and enum types are only compatible when they come from
        // the exact same defn.
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ENUM:
            return real_t1.type == real_t2.type;

        case TYPE_FUNCTION:
        {
            TypeFunction f_t1 = real_t1.type->type_function;
            TypeFunction f_t2 = real_t2.type->type_function;

            // Check for variadic
            if (f_t1.is_variadic != f_t2.is_variadic)
            {
                return false;
            }

            // Check for unspecified parameters
            if (f_t1.unspecified_paramters != f_t2.unspecified_paramters)
            {
                return false;
            }

            // Check for num parameters
            if (f_t1.num_paramaters != f_t2.num_paramaters)
            {
                return false;
            }
            
            // Check return types are compatible
            if (!qualified_type_is_compatible(&f_t1.return_type,
                    &f_t2.return_type))
            {
                return false;
            }

            TypeFunctionParameter* p1 = f_t1.paramaters;
            TypeFunctionParameter* p2 = f_t2.paramaters;

            while (p1 != NULL)
            {
                if (!qualified_type_is_compatible(&p1->type, &p2->type))
                {
                    return false;
                }

                p1 = p1->next;
                p2 = p2->next;
            }

            return true;            
        }

        case TYPE_POINTER:
        {
            QualifiedType p_t1 = real_t1.type->type_pointer.underlying_type;
            QualifiedType p_t2 = real_t2.type->type_pointer.underlying_type;

            return qualified_type_is_compatible(&p_t1, &p_t2);
        }

        case TYPE_TYPEDEF:
            panic("typedef should have been resolved");
            return false;

        case TYPE_ERROR:
            return false;
    }

    return true;
}

bool qualified_type_builtin_equal(const QualifiedType* t1,
        const QualifiedType* t2)
{
    return t1->type == t2->type;
}

bool qualified_type_is_complete(const QualifiedType* qual_type)
{
    QualifiedType real_type = qualified_type_get_canonical(qual_type);
    
    switch (qualified_type_get_kind(&real_type))
    {
        case TYPE_ENUM:
            return type_enum_is_complete(&real_type);

        // TODO: struct and union types
        case TYPE_STRUCT:
            return type_struct_is_complete(real_type.type);

        default:
            return real_type.type->type_base.is_complete;
    }
}

bool type_is_object_type(const Type* type)
{
    // Object types is anything but a function
    return !type_is(type, TYPE_FUNCTION);
}

bool qualifier_type_is_object_type(const QualifiedType* type)
{
    return type_is_object_type(type->type);
}

void type_print(const QualifiedType* t1)
{
    if (t1->type == NULL)
    {
        printf("<invalid-type>");
        return;
    }

    if (t1->qualifiers & QUALIFIER_CONST)
    {
        printf("const ");
    }

    if (t1->qualifiers & QUALIFIER_VOLATILE)
    {
        printf("volatile ");
    }

    if (t1->qualifiers & QUALIFIER_RESTRICT)
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
            printf("typename '%s' : ",
                    t1->type->type_typedef.tdef->base.identifier->string.ptr);
            type_print(&t1->type->type_typedef.real_type);
            break;

        case TYPE_STRUCT:
        {
            Declaration* decl = t1->type->type_struct.decl;
            printf("struct %s ", decl->base.identifier->string.ptr);    
            break;
        }

        case TYPE_FUNCTION:
            printf("function returning ");
            type_print(&t1->type->type_function.return_type);
            printf("with %zu parameters ", t1->type->type_function.num_paramaters);
            break;
    }
}

