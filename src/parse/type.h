#ifndef TYPE_H
#define TYPE_H

enum TypeType {
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_UNKNOWN
};
typedef enum TypeType TypeType;

typedef struct TypeBase TypeBase;

typedef struct TypeVoid TypeVoid;
typedef struct TypeChar TypeChar;
typedef struct TypeBool TypeBool;
typedef struct TypeShort TypeShort;
typedef struct TypeInt TypeInt;
typedef struct TypeLong TypeLong;
typedef struct TypeLongLong TypeLongLong;
typedef struct TypeFloat TypeFloat;
typedef struct TypeDouble TypeDouble;
typedef struct TypeStruct TypeStruct;
typedef struct TypeUnion TypeUnion;

struct TypeBase {
    TypeType type;
};

union Type {
    TypeBase base;
};

#endif /* TYPE_H */
