#ifndef INITIALIZER_H
#define INITIALIZER_H

#include <stddef.h>

#include "files/location.h"

#include "lex/identifier_table.h"
#include "parse/ast_allocator.h"
#include "parse/expression.h"

// The type of designators that we might have e.g. array [10] = ... or field
// e.g. .foo = ... We might might also have nested designators which we will
// need to check for with our designator list.
typedef enum DesignatorType {
    DESIGNATOR_ERROR,
    DESIGNATOR_MEMBER,
    DESIGNATOR_ARRAY
} DesignatorType;

typedef struct DesignatorBase {
    DesignatorType type;
} DesignatorBase;

typedef struct DesignatorMember {
    DesignatorBase base;
    Location dot_loc;
    Location identifier_loc;
    Identifier* identifier;
} DesignatorMember;

typedef struct DesignatorArray {
    DesignatorBase base;
    Location l_bracket_loc;
    Location r_bracket_loc;
    Expression* constant_expr;
} DesignatorArray;

typedef union Designator {
    DesignatorBase base;
    DesignatorMember member;
    DesignatorArray array;
} Designator;

typedef struct DesignatorList {
    Designator* designator;
    struct DesignatorList* next;
} DesignatorList;

// Represents the type of initializer that we have
typedef enum InitialzerType {
    INITIALIZER_ERROR,
    INITIALIZER_EXPRESSION,
    INITIALIZER_LIST
} InitializerType;

typedef union Initializer Initializer;

// Represents a base initializer
typedef struct InitializerBase {
    InitializerType type;
} InitializerBase;

// Represents and initializer expression
typedef struct InitializerExpression {
    InitializerBase base;
    Expression* expr;        
} InitializerExpression;

typedef struct InitializerListMember {
    DesignatorList* designation;
    Location equals_loc;
    Initializer* initializer;
    struct InitializerListMember* next;
} InitializerListMember;

typedef struct InitializerList {
    InitializerType base;

    Location left_backet;
    Location right_bracket;

    InitializerListMember* head;
} InitializerList;

// This represents an initializer that we might have.
union Initializer {
    InitializerBase base;
    InitializerExpression expr;
    InitializerList list;
};

// TODO: we will need to create functions for initializer creation and stuff

// Functions to create designators
Designator* designator_create_member(AstAllocator* allocator, Location dot_loc,
        Location identifier_loc, Identifier* identifier);
Designator* designator_create_array(AstAllocator* allocator, Location lbracket,
        Expression* expression, Location rbractet);
    
DesignatorList* designator_list_create(AstAllocator* allocator,
        Designator* designator);

bool designator_is(const Designator* designsotr, DesignatorType type);

Designator* designator_list_get_member(const DesignatorList* list);
DesignatorList* designator_list_set_next(DesignatorList* list,
        DesignatorList* next);
DesignatorList* designator_list_get_next(const DesignatorList* list);

// Function to create our initializer list member
InitializerListMember* initializer_list_member_create(AstAllocator* allocator,
        DesignatorList* designator, Location equals_loc, Initializer* init);

DesignatorList* initializer_list_member_get_designator(
        const InitializerListMember* member);
Location initializer_list_member_get_equals(const InitializerListMember* mem);
Initializer* initializer_list_member_get_initializer(
        const InitializerListMember* member);

InitializerListMember* initializer_list_member_set_next(
        InitializerListMember* member, InitializerListMember* next);
InitializerListMember* initializer_list_member_get_next(
        InitializerListMember* member);

// Functions to create initializer types
Initializer* initializer_create_expression(AstAllocator* allocator,
        Expression* expression);
Initializer* initializer_create_list(AstAllocator* allocator, Location lcurly,
        InitializerListMember* first_member, Location rcurly);

bool initializer_is(const Initializer* init, InitializerType type);
Expression* initializer_expression_get(const Initializer* init);
InitializerListMember* initializer_list_member_get(const Initializer* init);
Location initializer_list_lcurly(const Initializer* init);
Location initializer_list_rcurly(const Initializer* init);

// Non-recursively check if this initializer list has a designator.
// {[2] = 3}        => true
// {3}              => false
// {{.filed = 4}}   => false
// {}               => false
bool initializer_list_has_designator(const Initializer* init);

// True if this is a completely empty braced initializer
// {} => true
// otherwise => false
bool initializer_is_empty(const Initializer* init);

#endif /* INITIALIZER_H */
