#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/str.h"

#include "parse/type.h"

typedef struct Symbol {
    String name;
    Type type;
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

#endif /* SYMBOL_H */
