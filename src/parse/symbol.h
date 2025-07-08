#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/str.h"

#include "parse/type.h"

// 6.2.3 identifier namespaces
typedef enum SymbolNameSpace {
    SYMBOL_NAME_SPACE_ERROR, // Should never be used
    SYMBOL_NAME_SPACE_NONE, // The normal namespace ordinairy declarators or enum constants
    SYMBOL_NAME_SPACE_LABELS, // e.g. label: ...
    SYMBOL_NAME_SPACE_TAG, // struct, union, enum
    SYMBOL_NAME_SPACE_MEMBERS // members of struct / unions
} SymbolNameSpace;

typedef struct Symbol {
    SymbolNameSpace namespace;
    String name;
    Type* type;
} Symbol;

// An array of Symbol pointers since we don't want the symbol pointer itself
// to change once created
typedef struct SymbolTable {
    Symbol** symbols;
    size_t count;
    size_t cap;

    struct SymbolTable* prev;
} SymbolTable;

SymbolTable* symbol_table_new(SymbolTable* prev);

Symbol* symbol_table_lookup(SymbolTable* table);

#endif /* SYMBOL_H */
