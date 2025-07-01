#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>

enum TypeType {
    TYPE_ERROR,



    TYPE_VOID,
};
typedef enum TypeType TypeType;

struct TypeBase {
    TypeType type;
    bool builtin;
};

#endif /* TYPE_H */
