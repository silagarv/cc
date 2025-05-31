#ifndef SOURCE_H
#define SOURCE_H

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/buffer.h"

struct Source
{
    FILE* fp;

    Buffer* buffer;
    size_t buffer_pos;

    char* name;
    char* current_name;

    uint32_t line_no;
    uint32_t current_line_no;
};
typedef struct Source Source;

typedef uint32_t LineID;

#define LINEID_MAX (UINT32_MAX)

struct Line
{
    char* source_name;
    char* source_real_name;

    uint32_t line_no;
    uint32_t real_line_no;

    Buffer* buffer;

    LineID id;

    bool replaced_trigraphs;
    bool backslash_newline;
    bool ending_newline;
};
typedef struct Line Line;

Source* source_new(char* start_name, FILE* fp);
void source_free(Source* source);

void source_set_current_name(Source* source, char* new_name);
void source_set_current_line_number(Source* source, uint32_t new_line);

Line* source_read_line(Source* source);
void line_free(Line* line);

void line_set_id(Line* line, LineID id);
LineID line_get_id(Line* line);

size_t line_get_length(Line* line);
char line_get_char(Line* line, size_t index);
const char* line_get_ptr(Line* line);

#endif /* SOURCE_H */
