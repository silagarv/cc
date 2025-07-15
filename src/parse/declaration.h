#ifndef DECLARATION_H
#define DECLARATION_H

#include "lex/location.h"

#include "parse/type.h"

typedef enum DeclarationType {
    DECLARATION_ERROR = -1, /* an error in a declaration */

    DECLARATION_VARIABLE, /* of any local or otherwise variable */
    DECLARATION_FUNCTION, /* of a function with params */
    DECLARATION_FUNCTION_PARAM, /* parameters of a function */
    DECLARATION_ENUM, /* an enum */
    DECLARATION_ENUM_CONSTANT, /* constants within an enum */
    DECLARATION_STRUCT, /* of a struct */
    DECLARATION_UNION, /* of a union */
    DECLARATION_FIELD, /* of a struct / union */
    DECLARATION_TYPEDEF /* a typedef to any of the above */
} DeclarationType;

typedef struct DeclarationBase {
    DeclarationType decl_type; /* what type of declaration is this? */
    Location loc; /* where is this declaraiton located */
    Type* type; /* the overall type of the declaration */
} DeclarationBase;

typedef struct DeclarationVariable DeclarationVariable;
typedef struct DeclarationFunctionParamater DeclarationFunctionParamater;
typedef struct DeclarationFunction DeclarationFunction;
typedef struct DeclarationEnumConstant DeclarationEnumConstant;
typedef struct DeclarationEnum DeclarationEnum;
typedef struct DeclarationField DeclarationField;
typedef struct DeclarationStruct DeclarationStruct;
typedef struct DeclarationUnion DeclarationUnion;
typedef struct DeclarationTypedef DeclarationTypedef;

typedef union Declaration Declaration;

#endif /* DECLARATION_H */
