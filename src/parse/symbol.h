#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

#include "util/hash_map.h"

#include "files/location.h"

#include "parse/type.h"

// TODO: linkage maybe?

// The type of symbol that we are dealing with
typedef enum SymbolType {
    SYMBOL_VARIABLE,
    SYMBOL_FUNCTION,
    SYMBOL_TYPEDEF,
    SYMBOL_STRUCT,
    SYMBOL_UNION,
    SYMBOL_ENUM,
    SYMBOL_ENUM_CONSTANT,
    SYMBOL_LABEL
} SymbolType;

// 6.2.3 identifier namespaces
typedef enum SymbolNamespace {
    SYMBOL_NAMESPACE_NONE, // The normal namespace ordinairy declarators or enum constants
    SYMBOL_NAMESPACE_LABELS, // e.g. label: ...
    SYMBOL_NAMESPACE_TAG, // struct, union, enum names
    SYMBOL_NAMESPACE_MEMBERS // members of struct / unions
} SymbolNamespace;

typedef struct Symbol {
    // What is the actual name that we are going to refer to this symbol by.
    String name;

    // What type of symbol is this?
    SymbolType symbol_type;

    // The location this symbol was declared at.
    Location location;

    // The fully qualified type of the symbol
    QualifiedType qualified_type;
 
    // Storage specifier for the symbol
    TypeStorageSpecifier storage_specifier;

    // Function specifier for the symbol if relavent
    TypeFunctionSpecifier function_specifier;
} Symbol;

// An array of Symbol pointers since we don't want the symbol pointer itself
// to change once created.
typedef struct SymbolTable {
    // The parent symbol table for looking up symbols.
    struct SymbolTable* parent;

    // The map of symbols in the table. From String to symbol...
    HashMap symbols;
} SymbolTable;

SymbolTable* symbol_table_create(SymbolTable* parent);
void symbol_table_delete(SymbolTable* table);

bool symbol_table_insert(SymbolTable* table, Symbol* sym);

Symbol* symbol_table_lookup(SymbolTable* table, String name);

#endif /* SYMBOL_H */
