#include "type.h"

#include <stdlib.h>

#include "util/buffer.h"
#include "util/panic.h"
#include "util/str.h"
#include "util/xmalloc.h"

static Type* type_create_simple(TypeKind kind)
{
    Type* t = xmalloc(sizeof(Type));
    t->type_base.qualifiers = (TypeQualifiers) {0};
    t->type_base.type = kind;

    return t;
}

Type* type_create_error(void)
{
    return type_create_simple(TYPE_ERROR);
}

Type* type_create_void(void)
{
    return type_create_simple(TYPE_VOID);
}

Type* type_create_bool(void)
{
    return type_create_simple(TYPE_BOOL);
}

Type* type_create_char(void)
{
    return type_create_simple(TYPE_CHAR);
}

Type* type_create_signed_char(void)
{
    return type_create_simple(TYPE_S_CHAR);
}

Type* type_create_unsigned_char(void)
{
    return type_create_simple(TYPE_U_CHAR);
}

Type* type_create_signed_short(void)
{
    return type_create_simple(TYPE_S_SHORT);
}

Type* type_create_unsigned_short(void)
{
    return type_create_simple(TYPE_U_SHORT);
}

Type* type_create_signed_int(void)
{
    return type_create_simple(TYPE_S_INT);
}

Type* type_create_unsigned_int(void)
{
    return type_create_simple(TYPE_U_INT);
}

Type* type_create_signed_long(void)
{
    return type_create_simple(TYPE_S_LONG);
}

Type* type_create_unsigned_long(void)
{
    return type_create_simple(TYPE_U_LONG);
}

Type* type_create_signed_long_long(void)
{
    return type_create_simple(TYPE_S_LONG_LONG);
}

Type* type_create_unsigned_long_long(void)
{
    return type_create_simple(TYPE_U_LONG_LONG);
}

Type* type_create_float(void)
{
    return type_create_simple(TYPE_FLOAT);
}

Type* type_create_double(void)
{
    return type_create_simple(TYPE_DOUBLE);
}

Type* type_create_long_double(void)
{
    return type_create_simple(TYPE_LONG_DOUBLE);
}

Type* type_create_complex(Type* base_type)
{
    Type* t = type_create_simple(TYPE_COMPLEX);
    t->type_complex.underlying_type = base_type;

    return t;
}

Type* type_create_imaginary(Type* base_type)
{
    Type* t = type_create_simple(TYPE_IMAGINARY);
    t->type_imaginary.underlying_type = base_type;

    return t;
}

Type* type_create_imaginary(Type* base_type);

void type_free(Type* type)
{
    switch (type->type_base.type)
    {
        case TYPE_ERROR:
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
            // Nothing extra to do for these simple types...
            break;

        default:
            panic("type_free unimplemented for this type");
            break;
    }

    free(type);
}

// Declaration of function to print types to a buffer
static void type_to_string_internal(Type* type, Buffer* buff);

// NOTE: from testing with clangd the type printing order is:
//      -> const
//      -> volatile
//      -> restrict
static void type_print_qualifiers(TypeQualifiers* qualifiers, Buffer* buff)
{
    if (qualifiers->is_const)
    {
        buffer_printf(buff, "const ");
    }

    if (qualifiers->is_volatile)
    {
        buffer_printf(buff, "volatile ");
    }

    if (qualifiers->is_restrict)
    {
        buffer_printf(buff, "restrict ");
    }
}

static void type_print_rest(Type* type, Buffer* buff)
{
    switch (type->type_base.type)
    {
        case TYPE_ERROR: 
        // Don't reset since it might be helpful to see if we got any qualifiers
            buffer_printf(buff, "<error-type>");
            break;

        case TYPE_VOID:
            buffer_printf(buff, "void");
            break;

        case TYPE_BOOL:
            buffer_printf(buff, "bool");
            break;

        case TYPE_CHAR:
            buffer_printf(buff, "char");
            break;

        case TYPE_S_CHAR:
            buffer_printf(buff, "signed char");
            break;

        case TYPE_U_CHAR:
            buffer_printf(buff, "unsigned char");
            break;

        case TYPE_S_SHORT:
            buffer_printf(buff, "short");
            break;

        case TYPE_U_SHORT:
            buffer_printf(buff, "unisgned short");
            break;

        case TYPE_S_INT:
            buffer_printf(buff, "int");
            break;

        case TYPE_U_INT:
            buffer_printf(buff, "unsigned int");
            break;

        case TYPE_S_LONG:
            buffer_printf(buff, "long");
            break;

        case TYPE_U_LONG:
            buffer_printf(buff, "unsigned long");
            break;

        case TYPE_S_LONG_LONG:
            buffer_printf(buff, "long long");
            break;

        case TYPE_U_LONG_LONG:
            buffer_printf(buff, "unsigned long long");
            break;

        case TYPE_FLOAT:
            buffer_printf(buff, "float");
            break;

        case TYPE_DOUBLE:
            buffer_printf(buff, "double");
            break;

        case TYPE_LONG_DOUBLE:
            buffer_printf(buff, "long double");
            break;

        case TYPE_COMPLEX:
            buffer_printf(buff, "_Complex ");
            type_to_string_internal(type->type_complex.underlying_type, buff);
            break;

        case TYPE_IMAGINARY:
            buffer_printf(buff, "_Imaginary ");
            type_to_string_internal(type->type_complex.underlying_type, buff);
            break;
            
        // TODO: do the rest of the types

        default:
            buffer_reset(buff);
            buffer_printf(buff, "<type printing not implemented for this type>");
            break;
    }
}

static void type_to_string_internal(Type* type, Buffer* buff)
{
    type_print_qualifiers(&type->type_base.qualifiers, buff);

    type_print_rest(type, buff);
}

String type_to_string(Type* type)
{
    // NOTE: some arbitrary size is used here...
    Buffer type_buffer = buffer_new_size(20);

    type_to_string_internal(type, &type_buffer);

    String string = string_from_buffer(&type_buffer);

    return string;
}

