#ifndef IDENTIFIER_TABLE_H
#define IDENTIFIER_TABLE_H

// This file will contain prototypes for our identifier table so that
// we are able to easily intern identifiers to save 

#include "util/arena.h"
#include "util/str.h"
#include "util/hash_map.h"
#include "util/vec.h"

#include "lex/token.h"

// An identifier within the source.
typedef struct Identifier {
    // the actual string for this identifier which has been normalised. I.e. all
    // the ucn's have been converted to UTF-8. (TODO: this...)
    String string;

    // The underlying token type that we have for this identifier (if relavent)
    // this will not include any preprocessing tokens and will only include
    // tokens which will be used by the later stages of the compiler.
    TokenType type;

    // The hash for this identifier. We precompute this in order to save a bit
    // of time when trying to find the right identifier in the identifier table.
    uint32_t hash;
} Identifier;

vector_of_decl(Identifier*, Identifier, identifier);

// TODO: change identifier allocation to use an arena instead of normal 
// allocations

// Structure to represent the identifier table
typedef struct IdentifierTable {
    Arena ident_allocator;
    HashMap ident_table;
} IdentifierTable;

// Create a malloced identifier structure with an owned string inside of it
Identifier* identifier_create(String str);

// Create a malloced identifier from the string with the given token type.
Identifier* identifier_create_keyword(const char* kw, TokenType type);

// Delete an identifer.
void identifier_delete(Identifier* identifier);

// Some helpful methods for identifiers that we might need to use when lexing
// or parsing.
bool identifier_is_keyword(const Identifier* identifier);
bool identifier_is_reserved(const Identifier* identifier);
bool identifier_is_equal(const Identifier* ident1, const Identifier* ident2);

TokenType identifier_get_keyword(const Identifier* identifier);
String* identifier_get_string(Identifier* identifier);
uint32_t identifier_get_hash(const void* identifier);

// Create an identifier table to intern all of our identifiers
IdentifierTable identifier_table_create(void);

// Delete our identifier table and free all of the memory asociated with it
void identifier_table_delete(IdentifierTable* table);

// Lookup an identifier in the table inserting it into the table if it is not
// already a part of the table.
Identifier* identifier_table_lookup(IdentifierTable* table, String* string);

#endif /* IDENTIFIER_TABLE_H */
