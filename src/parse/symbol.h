#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/str.h"

#include "parse/type.h"

typedef struct Symbol {
    String name;
    Type type;
    // Some symbol stuff here
} Symbol;

typedef struct SymbolTable {
    Symbol* symbols;
    size_t count;
    size_t cap;

    struct SymbolTable* prev;
} SymbolTable;

SymbolTable* symbol_table_new(SymbolTable* prev);

#endif /* SYMBOL_H */
