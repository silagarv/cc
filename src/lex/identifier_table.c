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
        .type = TOK_IDENTIFIER
    };

    return ident;
}

// Create a malloced identifier from the string with the given token type.
Identifier* identifier_create_keyword(const char* kw, TokenType type,
        TokenType pp_type)
{
    String str;
    string_init_copy(&str, kw);

    Identifier* ident = xmalloc(sizeof(Identifier));
    *ident = (Identifier)
    {
        .string = str,
        .hash = string_get_hash(&str),
        .type = type,
        .pp_type = pp_type
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
    return identifier->type != TOK_IDENTIFIER;
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

TokenType identifier_get_pp_keyword(const Identifier* identifier)
{
    return identifier->pp_type;
}

String* identifier_get_string(Identifier* identifier)
{
    return &identifier->string;
}

uint32_t identifier_get_hash(const void* identifier)
{
    return ((Identifier*) identifier)->hash;
}

uint32_t identifier_table_get_hash(const void* key)
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
        const char* kw, TokenType type, TokenType pp_type)
{
    Identifier* ident = identifier_create_keyword(kw, type, pp_type);
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

    identifier_table_insert_keyword(&table, "auto", TOK_auto, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "break", TOK_break, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "case", TOK_case, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "char", TOK_char, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "const", TOK_const, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "continue", TOK_continue,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "default", TOK_default,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "do", TOK_do, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "double", TOK_double,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "else", TOK_else, TOK_PP_else);
    identifier_table_insert_keyword(&table, "enum", TOK_enum, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "extern", TOK_extern,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "float", TOK_float, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "for", TOK_for, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "goto", TOK_goto, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "if", TOK_if, TOK_PP_if);
    identifier_table_insert_keyword(&table, "inline", TOK_inline,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "int", TOK_int, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "long", TOK_long, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "register", TOK_register,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "restrict", TOK_restrict,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "return", TOK_return,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "short", TOK_short,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "signed", TOK_signed,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "sizeof", TOK_sizeof,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "static", TOK_static,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "struct", TOK_struct,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "switch", TOK_switch,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "typedef", TOK_typedef,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "union", TOK_union,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "unsigned", TOK_unsigned,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "void", TOK_void,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "volatile", TOK_volatile,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "while", TOK_while,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "_Bool", TOK__Bool, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "_Complex", TOK__Complex,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "_Imaginary", TOK__Imaginary,
            TOK_IDENTIFIER);

    // Predefined identifier for functions
    identifier_table_insert_keyword(&table, "__func__", TOK___func__,
            TOK_IDENTIFIER);

    // Tokens which are used for builtins / extensions to the language
    identifier_table_insert_keyword(&table, "__attribute__", TOK___attribute__,
            TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "__extension__", TOK___extension__,
            TOK_IDENTIFIER);

    // Different variations of 'asm' for embedded assembly
    identifier_table_insert_keyword(&table, "asm", TOK_asm, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "__asm", TOK_asm, TOK_IDENTIFIER);
    identifier_table_insert_keyword(&table, "__asm__", TOK_asm, TOK_IDENTIFIER);

    // all of our pp tokens that haven't already been covered.
    identifier_table_insert_keyword(&table, "define", TOK_IDENTIFIER,
            TOK_PP_define);
    identifier_table_insert_keyword(&table, "undef", TOK_IDENTIFIER,
            TOK_PP_undef);
    identifier_table_insert_keyword(&table, "include", TOK_IDENTIFIER,
            TOK_PP_include);
    identifier_table_insert_keyword(&table, "ifdef", TOK_IDENTIFIER,
            TOK_PP_ifdef);
    identifier_table_insert_keyword(&table, "ifndef", TOK_IDENTIFIER,
            TOK_PP_ifndef);
    identifier_table_insert_keyword(&table, "elif", TOK_IDENTIFIER,
            TOK_PP_elif);
    identifier_table_insert_keyword(&table, "endif", TOK_IDENTIFIER,
            TOK_PP_endif);
    identifier_table_insert_keyword(&table, "line", TOK_IDENTIFIER,
            TOK_PP_line);
    identifier_table_insert_keyword(&table, "error", TOK_IDENTIFIER,
            TOK_PP_error);
    identifier_table_insert_keyword(&table, "pragma", TOK_IDENTIFIER,
            TOK_PP_pragma);
    identifier_table_insert_keyword(&table, "defined", TOK_IDENTIFIER,
            TOK_PP_defined);

    return table;
}

// Delete our identifier table and free all of the memory asociated with it
void identifier_table_delete(IdentifierTable* table)
{
    hash_map_delete(&table->ident_table);
}

Identifier* identifier_table_lookup(IdentifierTable* table, String* string)
{
    assert(table != NULL);

    // printf("1: %p\n", (void*)table->ident_table.hash_func);
    Identifier* ident = hash_map_get(&table->ident_table, string);
    // printf("2: %p\n", (void*)table->ident_table.hash_func);
    if (ident)
    {
        return ident;
    }

    ident = identifier_create(*string);
    // printf("3: %p\n", (void*)table->ident_table.hash_func);
    hash_map_insert(&table->ident_table, &ident->string, ident);
    // printf("4: %p\n", (void*)table->ident_table.hash_func);

    return ident;
}

Identifier* identifier_table_get(IdentifierTable* table, const char* str)
{
    return identifier_table_lookup(table, &(String){(char*) str, strlen(str)});
}
