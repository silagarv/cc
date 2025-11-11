#include "identifier_table.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "util/xmalloc.h"
#include "util/str.h"
#include "util/hash.h"
#include "util/hash_map.h"
#include "util/vec.h"

#include "lex/char_help.h"
#include "lex/token.h"

vector_of_impl(Identifier*, Identifier, identifier)

// Create a malloced identifier structure with an owned string inside of it
Identifier* identifier_create(String str)
{
    String str_copy;
    string_copy(&str, &str_copy);

    Identifier* ident = xmalloc(sizeof(Identifier));
    *ident = (Identifier)
    {
        .string = str_copy,
        .hash = string_get_hash(&str),
        .type = TOKEN_IDENTIFIER
    };

    return ident;
}

// Create a malloced identifier from the string with the given token type.
Identifier* identifier_create_keyword(const char* kw, TokenType type)
{
    String str;
    string_init_copy(&str, kw);

    Identifier* ident = xmalloc(sizeof(Identifier));
    *ident = (Identifier)
    {
        .string = str,
        .hash = string_get_hash(&str),
        .type = type
    };

    return ident;
}

// Delete an identifer.
void identifier_delete(Identifier* identifier)
{
    string_free(&identifier->string);
    free(identifier);
}

bool identifier_is_keyword(const Identifier* identifier)
{
    return identifier->type != TOKEN_IDENTIFIER;
}

bool identifier_is_reserved(const Identifier* identifier)
{
    if (identifier_is_keyword(identifier))
    {
        return true;
    }

    const String* str = &identifier->string;
    if (string_get(str, 0) == '_' && string_get(str, 1) == '_')
    {
        return true;
    }

    if (string_get(str, 0) == '_' && is_uppercase(string_get(str, 1)))
    {
        return true;
    }

    return false;
}

bool identifier_is_equal(const Identifier* ident1, const Identifier* ident2)
{
    if (ident1 == ident2)
    {
        return true;
    }

    assert(!string_equal_string(&ident1->string, &ident2->string));

    return false;
}

TokenType identifier_get_keyword(const Identifier* identifier)
{
    assert(identifier_is_keyword(identifier));

    return identifier->type;
}

String* identifier_get_string(Identifier* identifier)
{
    return &identifier->string;
}

uint32_t identifier_get_hash(const void* identifier)
{
    return ((Identifier*) identifier)->hash;
}

static uint32_t identifier_table_get_hash(const void* key)
{
    return string_get_hash(key);
}

bool identifier_table_compare_function(const void* key1, const void* key2)
{
    return string_equal_string(key1, key2);
}

void identifier_table_free(void* key, void* data)
{
    identifier_delete(data);
}

static void identifier_table_insert_keyword(IdentifierTable* table, 
        const char* kw, TokenType type)
{
    Identifier* ident = identifier_create_keyword(kw, type);
    hash_map_insert(&table->ident_table, &ident->string, ident);
}

// Create an identifier table to intern all of our identifiers
IdentifierTable identifier_table_create(void)
{
    // Create the table which is just a hashmap wrapped in a nice structure to
    // help add some safety to it.
    IdentifierTable table = (IdentifierTable)
    {
        .ident_table = hash_map_create(identifier_table_get_hash, 
                identifier_table_compare_function, identifier_table_free)
    };

    identifier_table_insert_keyword(&table, "auto", TOKEN_AUTO);
    identifier_table_insert_keyword(&table, "break", TOKEN_BREAK);
    identifier_table_insert_keyword(&table, "case", TOKEN_CASE);
    identifier_table_insert_keyword(&table, "char", TOKEN_CHAR);
    identifier_table_insert_keyword(&table, "const", TOKEN_CONST);
    identifier_table_insert_keyword(&table, "continue", TOKEN_CONTINUE);
    identifier_table_insert_keyword(&table, "default", TOKEN_DEFAULT);
    identifier_table_insert_keyword(&table, "do", TOKEN_DO);
    identifier_table_insert_keyword(&table, "double", TOKEN_DOUBLE);
    identifier_table_insert_keyword(&table, "else", TOKEN_ELSE);
    identifier_table_insert_keyword(&table, "enum", TOKEN_ENUM);
    identifier_table_insert_keyword(&table, "extern", TOKEN_EXTERN);
    identifier_table_insert_keyword(&table, "float", TOKEN_FLOAT);
    identifier_table_insert_keyword(&table, "for", TOKEN_FOR);
    identifier_table_insert_keyword(&table, "goto", TOKEN_GOTO);
    identifier_table_insert_keyword(&table, "if", TOKEN_IF);
    identifier_table_insert_keyword(&table, "inline", TOKEN_INLINE);
    identifier_table_insert_keyword(&table, "int", TOKEN_INT);
    identifier_table_insert_keyword(&table, "long", TOKEN_LONG);
    identifier_table_insert_keyword(&table, "register", TOKEN_REGISTER);
    identifier_table_insert_keyword(&table, "restrict", TOKEN_RESTRICT);
    identifier_table_insert_keyword(&table, "return", TOKEN_RETURN);
    identifier_table_insert_keyword(&table, "short", TOKEN_SHORT);
    identifier_table_insert_keyword(&table, "signed", TOKEN_SIGNED);
    identifier_table_insert_keyword(&table, "sizeof", TOKEN_SIZEOF);
    identifier_table_insert_keyword(&table, "static", TOKEN_STATIC);
    identifier_table_insert_keyword(&table, "struct", TOKEN_STRUCT);
    identifier_table_insert_keyword(&table, "switch", TOKEN_SWITCH);
    identifier_table_insert_keyword(&table, "typedef", TOKEN_TYPEDEF);
    identifier_table_insert_keyword(&table, "union", TOKEN_UNION);
    identifier_table_insert_keyword(&table, "unsigned", TOKEN_UNSIGNED);
    identifier_table_insert_keyword(&table, "void", TOKEN_VOID);
    identifier_table_insert_keyword(&table, "volatile", TOKEN_VOLATILE);
    identifier_table_insert_keyword(&table, "while", TOKEN_WHILE);
    identifier_table_insert_keyword(&table, "_Bool", TOKEN__BOOL);
    identifier_table_insert_keyword(&table, "_Complex", TOKEN__COMPLEX);
    identifier_table_insert_keyword(&table, "_Imaginary", TOKEN__IMAGINARY);

    // Predefined __func__
    identifier_table_insert_keyword(&table, "__func__", TOKEN___FUNC__);

    return table;
}

// Delete our identifier table and free all of the memory asociated with it
void identifier_table_delete(IdentifierTable* table)
{
    hash_map_delete(&table->ident_table);
}

Identifier* identifier_table_lookup(IdentifierTable* table, String* string)
{
    Identifier* ident = hash_map_get(&table->ident_table, string);
    if (ident)
    {
        return ident;
    }

    ident = identifier_create(*string);
    hash_map_insert(&table->ident_table, &ident->string, ident);

    return ident;
}

Identifier* identifier_table_get(IdentifierTable* table, const char* str)
{
    return identifier_table_lookup(table, &(String){(char*) str, strlen(str)});
}
