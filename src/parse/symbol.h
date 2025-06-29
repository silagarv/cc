#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/str.h"

struct Symbol {
    String name;
    // Some symbol stuff here
};
typedef struct Symbol Symbol;

struct SymbolTable {
    Symbol* symbols;
    size_t count;
    size_t cap;
};
typedef struct SymbolTable SymbolTable;

#endif /* SYMBOL_H */
