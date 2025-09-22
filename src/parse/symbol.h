#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/hash_map.h"
#include "lex/identifier_table.h"
#include "parse/declaration.h"

// The different namespaces that symbols exist in. Although not directly needed
// in the type of symbol, this is important when considering the symbol tables
// themselves in order to ensure that we implement the standard correctly.
typedef enum SymbolNamespace {
    SYMBOL_NAMESPACE_NONE, // The normal namespace ordinairy declarators or enum constants
    SYMBOL_NAMESPACE_LABELS, // e.g. label: ...
    SYMBOL_NAMESPACE_TAG, // struct, union, enum names
    SYMBOL_NAMESPACE_MEMBERS // members of struct / unions
} SymbolNamespace;

typedef struct SymbolTable {
    SymbolNamespace ns;
    HashMap symbols;
} SymbolTable;

SymbolTable symbol_table_create(SymbolNamespace ns);
void symbol_table_delete(SymbolTable* table);

Declaration* symbol_table_lookup(SymbolTable* table, Identifier* identifier);
bool symbol_table_contains(SymbolTable* table, Identifier* identifier);

void symbol_table_insert(SymbolTable* table, Declaration* symbol);

#endif /* SYMBOL_H */
