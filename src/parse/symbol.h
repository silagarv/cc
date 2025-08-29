#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "parse/declaration.h"
#include "parse/type.h"

// 6.2.3 identifier namespaces
typedef enum SymbolNameSpace {
    SYMBOL_NAME_SPACE_ERROR, // Should never be used
    SYMBOL_NAME_SPACE_NONE, // The normal namespace ordinairy declarators or enum constants
    SYMBOL_NAME_SPACE_LABELS, // e.g. label: ...
    SYMBOL_NAME_SPACE_TAG, // struct, union, enum
    SYMBOL_NAME_SPACE_MEMBERS // members of struct / unions
} SymbolNameSpace;

typedef enum SymbolLinkage {
    SYMBOL_LINKAGE_NONE, /* if the symbols linkage is not relavent e.g. labels */
    SYMBOL_LINKAGE_INTERNAL,
    SYMBOL_LINKAGE_EXTERNAL
} SymbolLinkage;

typedef struct Symbol {
    // What is the actual name that we are going to refer to this symbol by
    String name;

    // Were was this symbol declared.
    Location declaration_location;

    // What is the type of this symbol
    Type type;
} Symbol;

// An array of Symbol pointers since we don't want the symbol pointer itself
// to change once created
typedef struct SymbolTable {
    Symbol** symbols;
    size_t count;
    size_t cap;
} SymbolTable;

SymbolTable* symbol_table_new(void);

bool symbol_table_insert(SymbolTable* table, Symbol* sym);

Symbol* symbol_table_lookup(SymbolTable* table, SymbolNameSpace ns);

#endif /* SYMBOL_H */
