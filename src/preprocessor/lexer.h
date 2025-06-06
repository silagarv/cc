#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>
#include <stdbool.h>

#include "util/static_string.h"

#include "preprocessor/buffered_source.h"
#include "preprocessor/line.h"
#include "preprocessor/token.h"

struct IncludeDirectory {
    StaticString path;
    struct IncludeDirectory* next;
};
typedef struct IncludeDirectory IncludeDirectory;

// Our lexer struct to store our state in
struct Lexer {
    IncludeDirectory* include_dir; // TODO: add more of these

    // Our builtin and command line stuff note that they are done in the order
    // that they appear in here
    Buffer* builtin_defines;
    Buffer* command_line_macros; // for defs / undefs in order given
    // Buffer* command_line_imacros; // for -imacros `file`
    Buffer* command_line_include; // for -include `file`

    // Our source stack here
    BufferedSource* souce_stack;
    size_t include_depth;

    // The current line we are lexing
    Line* current_line;
    size_t line_pos;

    // State flags for the current lexing
    bool is_whitespace;
    bool is_line_start;
    bool is_preproc_line;
    bool is_lex_header;

    void* line_map;
    void* location_map;

    void* macro_map;
};
typedef struct Lexer Lexer;

void lexer_initialise(Lexer* lexer, void* line_map, void* location_map);
void lexer_finalise(Lexer* lexer);
void lexer_close(Lexer* lexer);

bool lexer_push_start_file(Lexer* lexer, StaticString* filename);

void lexer_add_include_path(Lexer* lexer, StaticString* path);

void lexer_add_builtin_macros(Lexer* lexer);
void lexer_undef_builtin_macros(Lexer* lexer);

void lexer_add_command_line_macro(Lexer* lexer, StaticString* definition);
void lexer_add_command_line_undef(Lexer* lexer, StaticString* undef);
// void lexer_add_command_line_imacros(Lexer* lexer, StaticString* filename);
void lexer_add_command_line_include(Lexer* lexer, StaticString* filename);

void lexer_read_macros_from(Lexer* lexer, StaticString* filename);

bool lexer_tokenise(Lexer* lexer, TokenList* tokens);

#endif /* LEXER_H */
