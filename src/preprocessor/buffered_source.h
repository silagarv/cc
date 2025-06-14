#ifndef BUFFERED_SOURCE_H
#define BUFFERED_SOURCE_H

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "util/buffer.h"
#include "util/static_string.h"

enum BufferedSourceType {
    BUFFERED_SOURCE_FILE,
    BUFFERED_SOURCE_BUFFER
};
typedef enum BufferedSourceType BufferedSourceType;

struct BufferedSource {
    StaticString name;
    uint32_t line_no;
    
    StaticString current_name;
    uint32_t current_line_no;

    BufferedSourceType type;

    FILE* fp;

    Buffer* buffer;
    size_t buffer_pos;

    struct BufferedSource* prev;
};
typedef struct BufferedSource BufferedSource;

BufferedSource* buffered_source_from_file(FILE* fp, StaticString* start_name);
BufferedSource* buffered_source_from_buffer(Buffer* buffer, StaticString* start_name);

void buffered_source_free(BufferedSource* source);

void buffered_source_set_line_no(BufferedSource* source, uint32_t new_no);
void buffered_source_set_name(BufferedSource* source, StaticString* new_name);

int buffered_source_read_char(BufferedSource* source);

BufferedSource* buffered_source_push(BufferedSource* prev, BufferedSource* new);
BufferedSource* buffered_source_pop(BufferedSource* old);

bool buffered_source_has_prev(BufferedSource* source);

#endif /* BUFFERED_SOURCE_H */
