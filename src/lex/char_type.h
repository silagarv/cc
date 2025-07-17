#ifndef CHAR_TYPE_H
#define CHAR_TYPE_H

#include <stdbool.h>

bool is_identifier_start(char c);
bool is_identifier(char c);
bool is_numeric(char c);
bool is_octal(char c);
bool is_hexadecimal(char c);

bool is_horizontal_whitespace(char c);
bool is_vertical_whitespace(char c);
bool is_whitespace(char c);

bool is_ascii(char c);

#endif /* CHAR_TYPE_H */
