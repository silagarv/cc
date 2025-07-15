#ifndef DECLARATION_H
#define DECLARATION_H

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

typedef struct DeclarationBase DeclarationBase;

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
