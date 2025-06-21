#ifndef LEXED_SOURCE_FILE_H
#define LEXED_SOURCE_FILE_H

#include <stddef.h>

#include "lex/source_line.h"
#include "lex/token.h"

typedef struct LexedSourceFile {
    

    SourceLine* lines;
    size_t num_lines;

    TokenList tokens;

    
} LexedSourceFile;


#endif /* LEXED_SOURCE_FILE_H */
