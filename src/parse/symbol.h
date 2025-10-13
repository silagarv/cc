#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/hash_map.h"
#include "lex/identifier_table.h"
#include "parse/declaration.h"

typedef struct SymbolTable {
    HashMap symbols;
} SymbolTable;

SymbolTable symbol_table_create(void);
void symbol_table_delete(SymbolTable* table);

Declaration* symbol_table_lookup(SymbolTable* table, Identifier* identifier);
bool symbol_table_contains(SymbolTable* table, Identifier* identifier);

void symbol_table_insert(SymbolTable* table, Declaration* symbol);

#endif /* SYMBOL_H */
