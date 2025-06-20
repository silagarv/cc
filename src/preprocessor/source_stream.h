#ifndef SOURCE_STREAM_H
#define SOURCE_STREAM_H

#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>

#include "util/buffer.h"

#include "preprocessor/source_line.h"

typedef struct SourceStream {
    char* fileguts;
    char* end;
    char* pos;
} SourceStream;

SourceStream source_stream(char* fileguts, size_t len);

void source_stream_free(SourceStream* stream);

#define source_stream_at_eof(stream) ((stream)->pos >= (stream)->end)
SourceLine source_stream_read_line(SourceStream* stream);

#endif /* SOURCE_STREAM_H */
