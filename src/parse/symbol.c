#include "symbol.h"

#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "lex/identifier_table.h"
#include "util/hash_map.h"

#include "lex/token.h"

#include "parse/type.h"
#include "parse/declaration.h"

static uint32_t symbol_table_hash(const void* sym)
{
    return ((Identifier*) sym)->hash;
}

static bool symbol_table_equal(const void* sym1, const void* sym2)
{
    const Identifier* decl1 = sym1;
    const Identifier* decl2 = sym2;

    return decl1 == decl2;
}

SymbolTable symbol_table_create(void)
{
    SymbolTable table;
    table.symbols = hash_map_create(symbol_table_hash, 
            symbol_table_equal, NULL);

    return table;
}

void symbol_table_delete(SymbolTable* table)
{
    hash_map_delete(&table->symbols);
}

Declaration* symbol_table_lookup(SymbolTable* table, Identifier* identifier)
{
    if (identifier == NULL)
    {
        return NULL;
    }

    return hash_map_get(&table->symbols, identifier);
}

bool symbol_table_contains(SymbolTable* table, Identifier* identifier)
{
    return symbol_table_lookup(table, identifier) != NULL;
}

void symbol_table_insert(SymbolTable* table, Declaration* symbol)
{
    // Only go to try to add if the declaration has an identifier
    if (!declaration_has_identifier(symbol))
    {
        return;
    }

    assert(!symbol_table_contains(table, symbol->base.identifier));

    hash_map_insert(&table->symbols, symbol->base.identifier, symbol);
}
