#ifndef AST_DECLARATION_H
#define AST_DECLARATION_H

#include <stddef.h>
#include <stdint.h>

#include "files/location.h"

#include "lex/identifier_table.h"

#include "ast/ast_fwd.h"
#include "ast/allocator.h"
#include "ast/type.h"

typedef enum DeclarationType {
    DECLARATION_ERROR,
    DECLARATION_VARIABLE,
    DECLARATION_FUNCTION,
    DECLARATION_TYPEDEF,
    DECLARATION_FIELD,
    DECLARATION_COMPOUND, // Either a struct or union
    DECLARATION_ENUM_CONSTANT,
    DELCARATION_ENUM,
    DECLARATION_LABEL,
    DECLARATION_STATIC_ASSERT,
    DECLARATION_TRANSLATION_UNIT
} DeclarationType;

typedef enum DeclarationLinkage {
    DECLARATION_LINKAGE_NONE,
    DECLARATION_LINKAGE_INTERNAL,
    DECLARATION_LINKAGE_EXTERNAL
} DeclarationLinkage;

// TODO: fix the base to only contain the type and make other things that are
// TODO: more relavent to other declarations.
// TODO: Possible only keeping `kind` and `next`
typedef struct DeclarationBase {
    DeclarationType kind;

    Identifier* identifier;
    Location location;

    QualifiedType type;

    bool external;
    bool implicit;
    bool invalid;

    Declaration* next;
} DeclarationBase;

DeclarationType declaration_kind(const Declaration* decl);
Identifier* declaration_name(const Declaration* decl);
Location declaration_location(const Declaration* decl);
QualifiedType declaration_type(const Declaration* decl);
Declaration* declaration_next(const Declaration* decl);
bool declaration_external(const Declaration* decl);
bool declataion_implicit(const Declaration* decl);
bool declaration_invalid(const Declaration* decl);

bool declaration_valid(const Declaration* decl);
bool declaration_is(const Declaration* decl);

typedef struct DeclarationVariable {
    DeclarationBase base;

    StorageSpecifier storage;

    DeclarationLinkage linkage;

    Initializer* initializer;

    Declaration* definition;

    Declaration* next;
    Declaration* recent;

    bool tentative;
} DeclarationVariable;

Declaration* declaration_variable_create(AstAllocator* ast, Identifier* name,
        Location loc, QualifiedType type, StorageSpecifier storage,
        DeclarationLinkage linkage);
StorageSpecifier declaration_variable_storage(const Declaration* decl);
DeclarationLinkage declaration_variable_linkage(const Declaration* decl);
Initializer* declaration_variable_initializer(const Declaration* decl);
Declaration* declaration_variable_next(const Declaration* decl);
Declaration* declaration_variable_recent(const Declaration* decl);
Declaration* declaration_variable_definition(const Declaration* decl);
bool declaration_variable_tentative(const Declaration* decl);

void declaration_variable_set_initializer(Declaration* decl, Initializer* init);
void declaration_variable_set_definition(Declaration* decl, Declaration* defn);
void declaration_variable_add_next(Declaration* decl, Declaration* next);

bool declaration_variable_has_linkage(const Declaration* decl);
bool declaration_variable_has_definition(const Declaration* decl);
bool declaration_variable_has_initializer(const Declaration* decl);

typedef struct DeclarationFunction {
    DeclarationBase base;

    StorageSpecifier storage;
    FunctionSpecifier function;

    DeclarationLinkage linkage;

    Declaration* params;

    Statement* body;

    Declaration* definition;
    Declaration* next;
    Declaration* recent;

    bool all_inline;
} DeclarationFunction;

Declaration* declaration_function_create(AstAllocator* ast, Identifier* name,
        Location loc, QualifiedType type, StorageSpecifier storage,
        FunctionSpecifier function, DeclarationLinkage linkage,
        Declaration* params);
StorageSpecifier declaration_function_storage(const Declaration* decl);
FunctionSpecifier declaration_function_specifier(const Declaration* decl);
DeclarationLinkage declaration_function_linkage(const Declaration* decl);
Declaration* declaration_function_params(const Declaration* decl);
Statement* declaration_funciton_statement(const Declaration* decl);
Declaration* declaration_function_next(const Declaration* decl);
Declaration* declaration_function_recent(const Declaration* decl);
Declaration* declaration_function_definition(const Declaration* decl);
bool declaration_function_all_inline(const Declaration* decl);

void declaration_function_set_body(Declaration* decl, Statement* body);
void declaration_function_set_definition(Declaration* decl, Declaration* defn);
void declaration_function_add_next(Declaration* decl, Declaration* next);

bool declaration_function_has_linkage(const Declaration* decl);
bool declaration_function_has_definition(const Declaration* decl);
bool declaration_function_has_body(const Declaration* decl);

typedef struct DeclarationTypedef {
    DeclarationBase base;
} DeclarationTypedef;

Declaration* declaration_typedef_create(AstAllocator* ast, Identifier* name,
        Location loc, QualifiedType type);

typedef struct DeclarationField {
    DeclarationBase base;
    
    Location colon;

    Expression* bitfield;
    uint64_t bitfield_size;

    bool flexible;
} DeclarationField;

Declaration* declaration_field_create(AstAllocator* ast, Identifier* name,
        Location loc, QualifiedType type, Location colon, Expression* bitfield,
        uint64_t bitfield_size, bool flexible);
Location declaration_field_colon(const Declaration* decl);
Expression* declaration_field_bitfield(const Declaration* decl);
uint64_t declaration_field_bitfield_size(const Declaration* decl);
bool declaration_field_flexible(const Declaration* decl);

typedef struct DeclarationCompound {
    DeclarationBase base;

    Declaration** members;
    size_t num_members;

    Declaration* definition;
    Declaration* next;
    Declaration* recent;
    
    bool is_struct;
    bool flexible; // Do we have a flexible array member?
} DeclarationCompound;

Declaration* declaration_compound_create(AstAllocator* ast, Identifier* name,
        Location loc, QualifiedType type, bool is_struct);
Declaration* declaration_compound_next(const Declaration* decl);
Declaration* declaration_compound_recent(const Declaration* decl);
Declaration* declaration_compound_definition(const Declaration* decl);
Declaration* declaration_compound_members(const Declaration* decl);
bool declaration_compound_struct(const Declaration* decl);
bool declaration_compound_union(const Declaration* decl);
bool declaration_compound_flexible(const Declaration* decl);

void declaration_compound_set_members(Declaration* decl, Declaration* members);
void declaration_compound_set_definition(Declaration* decl, Declaration* defn);
void declaration_compound_add_next(Declaration* decl, Declaration* next);

bool declaration_compound_has_definition(const Declaration* decl);

typedef struct DeclarationEnumConstant {
    DeclarationBase base;

    Location equal;
    Expression* expr;
    int64_t value;
} DeclarationEnumConstant;

Declaration* declaration_enum_constant_create(AstAllocator* ast,
        Identifier* name, Location loc, QualifiedType type, Location equal,
        Expression* expr, int64_t value);
Location declaratio_enum_constant_equal(const Declaration* decl);
Expression* declaratio_enum_constant_expression(const Declaration* decl);
int64_t declaratio_enum_constant_value(const Declaration* decl);

typedef struct DeclarationEnum {
    DeclarationBase base;

    Declaration* definition;
    Declaration* next;
    Declaration* recent;

    Declaration** members;
    size_t num_members;
} DeclarationEnum;

Declaration* declaration_enum_create(AstAllocator* ast, Identifier* name,
        Location loc);
Declaration* declaration_enum_next(const Declaration* decl);
Declaration* declaration_enum_recent(const Declaration* decl);
Declaration* declaration_enum_definition(const Declaration* decl);
Declaration* declaration_enum_members(const Declaration* decl);

void declaration_enum_add_next(Declaration* decl, Declaration* next);
void declaration_enum_set_definition(Declaration* decl, Declaration* defn);
void declaration_enum_set_members(Declaration* decl, Declaration* members);

typedef struct DeclarationLabel {
    DeclarationBase base;
} DeclarationLabel;

Declaration* declaration_label_create(AstAllocator* ast, Identifier* name,
        Location loc, bool implicit);

void declaration_label_set_location(Declaration* decl, Location loc);

typedef struct DeclarationStaticAssert {
    DeclarationBase base;

    Location static_assert_loc;
    Location lparen;
    Location rparen;

    Expression* expr;
    int64_t value;
    Expression* string;
} DeclarationStaticAssert;

Declaration* declaration_create_static_assert(AstAllocator* allocator,
        Location static_assert_loc, Location lparen_loc, Location rparen_loc,
        Expression* expr, int64_t value, Expression* string);
Location declaration_static_assert_location(const Declaration* decl);
Location declaration_static_assert_lparen(const Declaration* decl);
Location declaration_static_assert_rparen(const Declaration* decl);
Expression* declaration_static_assert_expression(const Declaration* decl);
int64_t declaration_static_assert_value(const Declaration* decl);
Expression* declaration_static_assert_string(const Declaration* decl);

bool declaration_static_assert_failed(const Declaration* decl);

typedef struct DeclarationTranslationUnit {
    DeclarationBase base;
    Declaration* first;
    Declaration* recent;
} DeclarationTranslationUnit;

union Declaration {
    DeclarationBase base;
    DeclarationVariable variable;
    DeclarationFunction function;
    DeclarationTypedef tdef;
    DeclarationField field;
    DeclarationCompound compound;
    DeclarationEnumConstant enum_constant;
    DeclarationEnum enumeration;
    DeclarationLabel label;
    DeclarationStaticAssert sa;
    DeclarationTranslationUnit tu;
};

#endif /* AST_DECLARATION_H */
