#ifndef CHAR_HELP_H
#define CHAR_HELP_H

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t utf32;

bool is_uppercase(char c);

bool is_identifier_start(char c);
bool is_identifier(char c);
bool is_numeric(char c);
bool is_octal(char c);
bool is_decimal(char c);
bool is_hexadecimal(char c);

bool is_horizontal_whitespace(char c);
bool is_vertical_whitespace(char c);
bool is_whitespace(char c);

bool is_ascii(char c);

bool is_simple_escape(char c);
unsigned int convert_simple_escape(char c);

unsigned int convert_octal(char c);
unsigned int convert_decimal(char c);
unsigned int convert_hexadecimal(char c);

bool is_valid_character_in_base(char c, int base);
unsigned int convert_character_base(char c, int base);

bool is_valid_ucn(utf32 value);

#endif /* CHAR_HELP_H */
