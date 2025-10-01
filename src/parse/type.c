#include "type.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parse/ast_allocator.h"
#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"
#include "util/xmalloc.h"

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

