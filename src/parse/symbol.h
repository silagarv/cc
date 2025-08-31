#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/hash_map.h"
#include "lex/identifier_table.h"
#include "parse/declaration.h"

// TODO: linkage maybe?

// The different namespaces that symbols exist in. Although not directly needed
// in the type of symbol, this is important when considering the symbol tables
// themselves in order to ensure that we implement the standard correctly.
typedef enum SymbolNamespace {
    SYMBOL_NAMESPACE_NONE, // The normal namespace ordinairy declarators or enum constants
    SYMBOL_NAMESPACE_LABELS, // e.g. label: ...
    SYMBOL_NAMESPACE_TAG, // struct, union, enum names
    SYMBOL_NAMESPACE_MEMBERS // members of struct / unions
} SymbolNamespace;

typedef struct Symbol {
    // What is the actual name that we are going to refer to this symbol by.
    Identifier* identifier;

    // What is the declaration of this symbol
    Declaration* decl;
} Symbol;

// An array of Symbol pointers since we don't want the symbol pointer itself
// to change once created. This is quite a simple structure and only contains
// the parent of this symbol table, the list of symbols within the table, and
// the symbol namespace of this table.
typedef struct SymbolTable {
    // The namespace all of these symbols reside in
    SymbolNamespace ns;
    
    // The actual symbols within this symbol table.
    HashMap symbols;
} SymbolTable;

Symbol* symbol_create(Identifier* ident, Declaration* declaration);

SymbolTable* symbol_table_create(SymbolTable* parent);
void symbol_table_delete(SymbolTable* table);

Symbol* symbol_table_lookup(SymbolTable* table, Identifier* identifier);

#endif /* SYMBOL_H */
