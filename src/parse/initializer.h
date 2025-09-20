#ifndef INITIALIZER_H
#define INITIALIZER_H

#include <stddef.h>

#include "files/location.h"

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
    String* identifier;
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
    Designator designator;
    struct DesignatorList* next;
} DesignatorList;

typedef struct Designation {
    DesignatorList designators;
    Location equals_loc;
} Designation;

// Represents the type of initializer that we have
typedef enum InitialzerType {
    INITIALIZER_ERROR,
    INITIALIZER_EXPRESSION,
    INITIALIZER_LIST
} InitialzerType;

typedef union Initializer Initializer;

// Represents a base initializer
typedef struct InitializerBase {
    InitialzerType type;
} InitializerBase;

// Represents and initializer expression
typedef struct InitializerExpression {
    InitializerBase base;
    Expression* expr;        
} InitializerExpression;

typedef struct InitializerListMember {
    Designation* designation;
    Initializer* initializer;
    struct InitializerListMember* next;
} InitializerListMember;

typedef struct InitializerList {
    InitialzerType base;

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

// TODO: this will potentially be one of the trickiest parts of the whole
// TODO: compiler. Since I am quite lost at the moment on how to initialize
// TODO: these things

#endif /* INITIALIZER_H */
