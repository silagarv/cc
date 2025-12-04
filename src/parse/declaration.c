#include "declaration.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "lex/preprocessor.h"
#include "parse/ast.h"
#include "util/vec.h"

#include "files/location.h"

#include "lex/identifier_table.h"

#include "parse/type.h"
#include "parse/expression.h"
#include "parse/initializer.h"
#include "parse/ast_allocator.h"

vector_of_impl(Declaration*, Declaration, declaration)

char* tag_kind_to_name(DeclarationType type)
{
    switch (type)
    {
        case DECLARATION_ENUM:
            return "enum";

        case DECLARATION_STRUCT:
            return "struct";

        case DECLARATION_UNION:
            return "union";

        default:
            panic("bad tag type in tag_kind_to_name");
            return NULL;
    }
}

char* declarator_context_to_name(DeclaratorContext context)
{
    switch (context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
            return "type id";

        case DECLARATION_CONTEXT_FUNCTION_PARAM:
            return "function parameter";

        case DECLARATION_CONTEXT_BLOCK:
            return "block";

        case DECLARATION_CONTEXT_FILE:
            return "file";

        case DECLARATION_CONTEXT_STRUCT:
            return "struct";
    }
}

DeclarationSpecifiers declaration_specifiers_create(Location location)
{
    DeclarationSpecifiers specifiers = (DeclarationSpecifiers)
    {
        .type = NULL,
        .storage_spec = TYPE_STORAGE_SPECIFIER_NONE,
        .qualifiers = TYPE_QUALIFIER_NONE,
        .function_spec = TYPE_FUNCTION_SPECIFIER_NONE,
        .type_spec_type = TYPE_SPECIFIER_TYPE_NONE,
        .type_spec_width = TYPE_SPECIFIER_WIDTH_NONE,
        .type_spec_sign = TYPE_SPECIFIER_SIGN_NONE,
        .type_spec_complex = TYPE_SPECIFIER_COMPLEX_NONE,
        .location = location,
        .declaration = NULL
    };

    return specifiers;
}

bool declaration_specifiers_has_declaration(const DeclarationSpecifiers* d)
{
    return d->declaration != NULL;
}

Declaration* declaration_specifiers_get_declaration(
        const DeclarationSpecifiers* decl_spec)
{
    return decl_spec->declaration;
}

bool declaration_specifiers_allow_typename(const DeclarationSpecifiers* d)
{
    if (d->type_spec_sign != TYPE_SPECIFIER_SIGN_NONE)
    {
        return false;
    }

    if (d->type_spec_width != TYPE_SPECIFIER_WIDTH_NONE)
    {
        return false;
    }

    if (d->type_spec_complex != TYPE_SPECIFIER_COMPLEX_NONE)
    {
        return false;
    }

    if (d->type_spec_type != TYPE_SPECIFIER_TYPE_NONE)
    {
        return false;
    }

    return true;
}

TypeStorageSpecifier declaration_specifiers_storage(DeclarationSpecifiers* d)
{
    return d->storage_spec;
}

void declaration_specifiers_remove_storage(DeclarationSpecifiers* d)
{
    d->storage_spec = TYPE_STORAGE_SPECIFIER_NONE;
}

Declarator declarator_create(DeclarationSpecifiers* specifiers,
        DeclaratorContext context, AstAllocator* allocator)
{
    Declarator declarator = (Declarator)
    {
        .context = context,
        .specifiers = specifiers,
        .identifier = NULL,
        .identifier_location = LOCATION_INVALID,
        .piece_stack = NULL,
        .allocator = allocator,
        .function_defn = false,
        .invalid = false
    };

    return declarator;
}

DeclarationSpecifiers* declarator_get_specifiers(const Declarator* declarator)
{
    return declarator->specifiers;
}

Identifier* declarator_get_identifier(const Declarator* declarator)
{
    return declarator->identifier;
}

Location declarator_get_location(const Declarator* declarator)
{
    return declarator->identifier_location;
}

DeclaratorContext declarator_get_context(const Declarator* declarator)
{
    return declarator->context;
}

bool declarator_identifier_allowed(const Declarator* declarator)
{
    switch (declarator->context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
            return false;

        case DECLARATION_CONTEXT_FUNCTION_PARAM:
        case DECLARATION_CONTEXT_BLOCK:
        case DECLARATION_CONTEXT_FILE:
        case DECLARATION_CONTEXT_STRUCT:
            return true;
    }

    panic("bad declarator context");
}

bool declarator_identifier_required(const Declarator* declarator)
{
    switch (declarator->context)
    {
        case DECLARATION_CONTEXT_TYPE_NAME:
        case DECLARATION_CONTEXT_FUNCTION_PARAM:
            return false;

        case DECLARATION_CONTEXT_BLOCK:
        case DECLARATION_CONTEXT_FILE:
        case DECLARATION_CONTEXT_STRUCT:
            return true;
    }

    panic("bad declarator context");
}

bool declarator_is_func_defn(const Declarator* declarator)
{
    return declarator->function_defn;
}

void declarator_set_func_defn(Declarator* declarator)
{
    declarator->function_defn = true;
}

bool declarator_has_identifier(const Declarator* declarator)
{
    return declarator->identifier != NULL;
}

void declarator_set_identifier(Declarator* declarator, Identifier* identifier,
        Location identifier_location)
{
    assert(declarator->identifier == NULL && 
            declarator->identifier_location == LOCATION_INVALID);

    declarator->identifier = identifier;
    declarator->identifier_location = identifier_location;
}

void declarator_set_invalid(Declarator* declarator)
{
    declarator->invalid = true;
}

bool declarator_is_invalid(const Declarator* declarator)
{
    return declarator->invalid;
}

bool declarator_allowed_bitfields(Declarator* declarator)
{
    return declarator->context == DECLARATION_CONTEXT_STRUCT;
}

Location declarator_get_colon_location(Declarator* declarator)
{
    return declarator->colon_location;
}

Expression* declarator_get_bitfield_expression(Declarator* declarator)
{
    return declarator->bitfield_expression;
}

DeclaratorPiece* declarator_get_function_piece(const Declarator* declarator)
{
    DeclaratorPiece* lowest = NULL;

    DeclaratorPiece* piece = declarator->piece_stack;
    while (piece != NULL)
    {
        switch (piece->base.type)
        {
            case DECLARATOR_PIECE_FUNCTION:
                lowest = piece;
                break;

            case DECLARATOR_PIECE_ARRAY:
            case DECLARATOR_PIECE_POINTER:
                break;

            default:
                panic("unexpected declarator piece type");
                break;
        }
        
        piece = piece->base.next;
    }
    
    return lowest;
}

Declaration* declarator_function_piece_get_decls(const DeclaratorPiece* piece)
{
    assert(piece->base.type == DECLARATOR_PIECE_FUNCTION);

    return piece->function.all_decls;
}

bool declarator_has_function(const Declarator* declarator)
{
    return declarator_get_function_piece(declarator) != NULL;
}

void declarator_push_pointer(Declarator* declarator, TypeQualifiers qualifiers)
{
    DeclaratorPiece* piece = ast_allocator_alloc(declarator->allocator,
            sizeof(DeclaratorPiece));
    piece->base.type = DECLARATOR_PIECE_POINTER;
    piece->base.next = declarator->piece_stack;
    piece->pointer.qualifiers = qualifiers;

    declarator->piece_stack = piece;
}

void declarator_push_array(Declarator* declarator, Location lbracket, 
        Location rbracket, Location static_location, TypeQualifiers qualifiers,
        Expression* expression, bool is_static, bool is_star)
{
    DeclaratorPiece* piece = ast_allocator_alloc(declarator->allocator,
            sizeof(DeclaratorPiece));
    piece->base.type = DECLARATOR_PIECE_ARRAY;
    piece->base.next = declarator->piece_stack;
    piece->array.lbracket = lbracket;
    piece->array.rbracket = rbracket;
    piece->array.static_location = static_location;
    piece->array.qualifiers = qualifiers;
    piece->array.expression = expression;
    piece->array.is_static = is_static;
    piece->array.is_star = is_star;

    declarator->piece_stack = piece;
}

void declarator_push_function(Declarator* declarator, Location lparen_loc,
        Location rparen_loc, DeclarationList params, size_t num_params,
        Declaration* all_decls, Location dots)
{   

    DeclaratorPiece* piece = ast_allocator_alloc(declarator->allocator,
            sizeof(DeclaratorPiece));
    piece->base.type = DECLARATOR_PIECE_FUNCTION;
    piece->base.next = declarator->piece_stack;
    piece->function.lparen_loc = lparen_loc;
    piece->function.rparen_loc = rparen_loc;
    piece->function.dots = dots;
    piece->function.paramaters = params;
    piece->function.num_paramaters = num_params;
    piece->function.all_decls = all_decls;
    piece->function.is_variadic = dots != LOCATION_INVALID;

    declarator->piece_stack = piece;
}

void declarator_add_bitfield(Declarator* declarator, Location colon_location,
        Expression* expression)
{
    declarator->colon_location = colon_location;
    declarator->bitfield_expression = expression;
}

DeclarationList declaration_list_create(AstAllocator* allocator)
{
    DeclarationList list = (DeclarationList)
    {
        .allocator = allocator,
        .head = NULL,
        .tail = NULL
    };

    return list;
}

void declaration_list_push(DeclarationList* list, Declaration* decl)
{
    // Create the entry
    DeclarationListEntry* entry = ast_allocator_alloc(list->allocator,
            sizeof(DeclarationListEntry));
    entry->declaration = decl;
    entry->next = NULL;

    if (list->head == NULL)
    {
        list->head = entry;
    }
    else
    {
        *list->tail = entry;
    }
    list->tail = &entry->next;
}

Declaration* declaration_list_entry_get(const DeclarationListEntry* entry)
{
    return entry->declaration;
}

DeclarationListEntry* declaration_list_iter(const DeclarationList* list)
{
    return list->head;
}

DeclarationListEntry* declaration_list_next(const DeclarationListEntry* curr)
{
    return curr->next;
}

DeclarationType declaration_get_kind(const Declaration *decl)
{
    return decl->base.declaration_type;
}

bool declaration_is(const Declaration* decl, DeclarationType type)
{
    if (!decl)
    {
        return false;
    }

    return decl->base.declaration_type == type;
}

bool declaration_is_tag(const Declaration* decl)
{
    return declaration_is(decl, DECLARATION_ENUM)
            || declaration_is(decl, DECLARATION_STRUCT)
            || declaration_is(decl, DECLARATION_UNION);
}

bool declaration_has_identifier(const Declaration* decl)
{
    if (!decl)
    {
        return false;
    }

    return decl->base.identifier != NULL;
}

Identifier* declaration_get_identifier(const Declaration* decl)
{
    assert(declaration_has_identifier(decl));

    return decl->base.identifier;
}

bool declaration_is_external(const Declaration* decl)
{
    return decl->base.external;
}

bool declaration_is_valid(const Declaration* decl)
{
    if (!decl)
    {
        return false;
    }

    return !decl->base.invalid;
}

void declaration_set_invalid(Declaration* decl)
{
    if (!decl)
    {
        return;
    }

    decl->base.invalid = true;
}

void declaration_set_type(Declaration* decl, QualifiedType type)
{
    decl->base.qualified_type = type;
}

QualifiedType declaration_get_type(const Declaration* decl)
{
    return decl->base.qualified_type;
}

TypeStorageSpecifier declaration_get_storage_class(const Declaration* decl)
{
    return decl->base.storage_class;
}

Location declaration_get_location(const Declaration* decl)
{
    return decl->base.location;
}

void declaration_set_next(Declaration* decl, Declaration* next)
{
    assert(decl->base.next == NULL);

    decl->base.next = next;
}

Declaration* declaration_get_next(Declaration* decl)
{
    return decl->base.next;
}

Declaration** declaration_get_next_ptr(Declaration* decl)
{
    return &decl->base.next;
}

// This is a function to create a barebones base declaration.
static Declaration* declaration_create_base(AstAllocator* allocator, 
        size_t size, DeclarationType decl_type, Location location, 
        Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage, TypeFunctionSpecifier function,
        bool implicit)
{
    Declaration* decl = ast_allocator_alloc(allocator, size);
    decl->base = (DeclarationBase)
    {
        .declaration_type = decl_type,
        .location = location,
        .identifier = identifier,
        .qualified_type = type,
        .storage_class = storage,
        .function_specifier = function,
        .external = false, /*implicit=TODO: make sure this field is added*/
        .implicit = implicit,
        .invalid = false
    };

    return decl;
}

Declaration* declaration_create_error(AstAllocator* allocator,
        Location location)
{
    return declaration_create_base(allocator, sizeof(DeclarationBase),
            DECLARATION_ERROR, location, NULL, (QualifiedType) {0},
            TYPE_STORAGE_SPECIFIER_NONE, TYPE_FUNCTION_SPECIFIER_NONE, false);
}

Declaration* declaration_create_variable(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type, 
        TypeStorageSpecifier storage)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationVariable), DECLARATION_VARIABLE, location, 
            identifier, type, storage, TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->variable.initializer = NULL;

    return decl;
}

void declaration_variable_add_initializer(Declaration* declaration,
        Initializer* initializer)
{
    assert(declaration_is(declaration, DECLARATION_VARIABLE));
    declaration->variable.initializer = initializer;
}

Declaration* declaration_create_typedef(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationTypedef), DECLARATION_TYPEDEF, location,
            identifier, type, TYPE_STORAGE_SPECIFIER_TYPEDEF,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    
    return decl;
}

void declaration_typedef_set_type(Declaration* tdef, Type* new_type)
{
    tdef->tdef.new_type = new_type;
}

Declaration* declaration_create_enum(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        bool anonymous)
{
    assert(identifier != NULL);

    Declaration* declaration = declaration_create_base(allocator,
            sizeof(DeclarationEnum), DECLARATION_ENUM, location, identifier,
            type, TYPE_STORAGE_SPECIFIER_NONE, TYPE_FUNCTION_SPECIFIER_NONE,
            false);

    declaration->enumeration.entries = NULL;
    declaration->enumeration.num_entries = 0;
    declaration->enumeration.complete = false;
    declaration->enumeration.anonymous = true;

    return declaration;
}

bool declaration_enum_has_entries(const Declaration* declaration)
{
    if (declaration == NULL)
    {
        return false;
    }

    if (!declaration_is(declaration, DECLARATION_ENUM))
    {
        return false;
    }

    return declaration->enumeration.complete;
}

void declaration_enum_set_entries(Declaration* declaration,
        Declaration** entries, size_t num_entries)
{
    assert(declaration_is(declaration, DECLARATION_ENUM));
    assert(!declaration_enum_has_entries(declaration));
    assert(declaration->enumeration.entries == NULL);
    assert(declaration->enumeration.num_entries == 0);

    declaration->enumeration.entries = entries;
    declaration->enumeration.num_entries = num_entries;
    declaration->enumeration.complete = true;

    type_enum_set_complete(&declaration->enumeration.base.qualified_type);
}

Declaration* declaration_create_enum_constant(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        Location equals, Expression* expression, int value)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationEnumConstant), DECLARATION_ENUM_CONSTANT,
            location, identifier, type, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->enumeration_constant.equals_loc = equals;
    decl->enumeration_constant.expression = expression;
    decl->enumeration_constant.value = value;

    return decl;
}

int declaration_enum_constant_get_value(const Declaration* enum_constant)
{
    if (declaration_is(enum_constant, DECLARATION_ERROR))
    {
        return 0;
    }

    assert(declaration_is(enum_constant, DECLARATION_ENUM_CONSTANT));

    return enum_constant->enumeration_constant.value;
}

Declaration* declaration_create_field(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        Location colon_location, Expression* expression)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationField), DECLARATION_FIELD, location, identifier,
            type, TYPE_STORAGE_SPECIFIER_NONE, TYPE_FUNCTION_SPECIFIER_NONE,
            false);
    decl->field.colon_location = colon_location;
    decl->field.bitfield = expression;
    decl->field.has_bitfield = expression != NULL;

    return decl;
}

Declaration* declaration_create_struct(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationCompound), DECLARATION_STRUCT, location,
            identifier, type, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->compound.members = declaration_list_create(allocator);

    return decl;
}

void declaration_struct_add_member(Declaration* declaration,
        Declaration* member)
{
    assert(declaration_is(declaration, DECLARATION_STRUCT) ||
            declaration_is(declaration, DECLARATION_UNION));
    assert(member);

    declaration_list_push(&declaration->compound.members, member);
}

DeclarationList declaration_struct_get_members(const Declaration* declaration)
{
    return declaration->compound.members;
}

bool declaration_struct_is_complete(const Declaration* declaration)
{
    if (declaration == NULL)
    {
        return false;
    }

    DeclarationType decl_type = declaration->base.declaration_type;
    if (decl_type != DECLARATION_STRUCT && decl_type != DECLARATION_UNION)
    {
        return false;
    }

    QualifiedType type = declaration->compound.base.qualified_type;
    return type_struct_is_complete(type.type);
}

void declaration_struct_set_complete(Declaration* declaration)
{
    QualifiedType type = declaration->compound.base.qualified_type;
    type.type->type_struct.base.is_complete = true;
}

Declaration* declaration_create_union(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type)
{
    Declaration* decl = declaration_create_base(allocator,
            sizeof(DeclarationCompound), DECLARATION_UNION, location,
            identifier, type, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, false);
    decl->compound.members = declaration_list_create(allocator);

    return decl;
}

// TODO: below
void declaration_union_add_member(Declaration* declaration,
        Declaration* members);
bool declaration_union_is_complete(const Declaration* declaration);

Declaration* declaration_create_function(AstAllocator* allocator,
        Location location, Identifier* identifier, QualifiedType type,
        TypeStorageSpecifier storage, TypeFunctionSpecifier function_spec,
        Declaration* paramaters)
{
    Declaration* declaration = declaration_create_base(allocator,
            sizeof(DeclarationFunction), DECLARATION_FUNCTION, location,
            identifier, type, storage, function_spec, false);
    declaration->function.all_decls = declaration_list_create(allocator);
    declaration->function.definition = NULL;
    declaration->function.function_body = NULL;
    declaration->function.paramaters = paramaters;

    return declaration;
}

void declaration_function_add_decl(Declaration* function, Declaration* decl)
{
    declaration_list_push(&function->function.all_decls, decl);
}

bool declaration_function_has_body(const Declaration* declaration)
{
    assert(declaration_is(declaration, DECLARATION_FUNCTION));

    return declaration->function.function_body != NULL;
}

void declaration_function_set_body(Declaration* declaraiton,
        union Statement* body)
{
    assert(!declaration_function_has_body(declaraiton));

    declaraiton->function.function_body = body;
}

void declaration_function_set_definition(Declaration* declaration,
        Declaration* definition)
{
    declaration->function.definition = definition;
}

Declaration* declaration_function_get_paramaters(const Declaration* function)
{
    return function->function.paramaters;
}

Declaration* declaration_create_label(AstAllocator* allocator, 
        Identifier* identifier, Location location, bool implicit)
{
    // Note the label is a special case where we do not really have a type
    Declaration* decl = declaration_create_base(allocator, 
            sizeof(DeclarationLabel), DECLARATION_LABEL, location, identifier,
            (QualifiedType) {0}, TYPE_STORAGE_SPECIFIER_NONE,
            TYPE_FUNCTION_SPECIFIER_NONE, implicit);
    decl->label.used = implicit; // if implicit then used...

    return decl;
}

