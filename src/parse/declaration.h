#ifndef DECLARATION_H
#define DECLARATION_H

#include "lex/location.h"

#include "parse/expression.h"
#include "parse/type.h"
#include <stddef.h>

typedef enum DeclarationType {
    DECLARATION_ERROR = -1, /* an error in a declaration */

    DECLARATION_VARIABLE, /* of any local or otherwise variable */
    DECLARATION_FUNCTION_PARAM, /* parameters of a function */
    DECLARATION_FUNCTION, /* of a function with params */
    DECLARATION_ENUM_CONSTANT, /* constants within an enum */
    DECLARATION_ENUM, /* an enum */
    DECLARATION_FIELD, /* of a struct / union */
    DECLARATION_STRUCT, /* of a struct */
    DECLARATION_UNION, /* of a union */
    DECLARATION_TYPEDEF /* a typedef to any of the above */
} DeclarationType;

typedef struct DeclarationBase {
    DeclarationType decl_type; /* what type of declaration is this? */
    Location loc; /* where is this declaraiton located */
    Type* type; /* the overall type of the declaration */
} DeclarationBase;

typedef struct DeclarationVariable {
    DeclarationBase base;

    String name; /* the name of the variable */

    bool initialized; /* is it initialised */
    bool used; /* was the variable used at all? */
} DeclarationVariable;

typedef struct DeclarationFunctionParamater DeclarationFunctionParamater;
typedef struct DeclarationFunction DeclarationFunction;

// NOTE: in c99 enum constants always have underlying type of int. Though this
// may not be true in c23 or with extensions so we will include the underlying
// type there too
typedef struct DeclarationEnumConstant DeclarationEnumConstant;
typedef struct DeclarationEnum DeclarationEnum;

/* Since enum and enum constant need to reference each other */
struct DeclarationEnumConstant {
    DeclarationBase base;

    String name; /* the name e.g. GREEN */
    Expression* expr; /* the value given if any */
    DeclarationEnum* parent_enum; /* the parent enum */
};

struct DeclarationEnum {
    DeclarationBase base;

    String name; /* enum name if any... */

    DeclarationEnumConstant* members; /* the memebers of an enum declaration */
    size_t num_members; /* the number of members in the declaration */
};

typedef struct DeclarationField DeclarationField;
typedef struct DeclarationStruct DeclarationStruct;
typedef struct DeclarationUnion DeclarationUnion;

typedef struct DeclarationTypedef DeclarationTypedef;

typedef union Declaration {
    DeclarationBase base;

    DeclarationVariable var;

    DeclarationEnum enumeration;
    DeclarationEnumConstant enumeration_constant;

} Declaration;

#endif /* DECLARATION_H */
