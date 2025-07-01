#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>
#include <stddef.h>

enum TypeType {
    TYPE_ERROR,


    TYPE_INT,
    TYPE_VOID,
};
typedef enum TypeType TypeType;

typedef struct TypeStructMember {
    char* name;
} TypeStructMember;

typedef struct TypeStruct {
    TypeStructMember* members;
    size_t num_members;
} TypeStruct;

struct TypeBase {
    TypeType type;
    bool is_builtin;
    bool is_implicitly_created; /* was this created implicitly (int) */
};

#endif /* TYPE_H */
