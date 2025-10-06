#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "driver/diagnostic.h"
#include "files/file_manager.h"
#include "files/location.h"
#include "files/source_manager.h"
#include "parse/statement.h"
#include "util/hash_map.h"
#include "util/panic.h"
#include "util/buffer.h"
#include "util/str.h"
#include "util/xmalloc.h"

#include "lex/token.h"
#include "files/line_map.h"

#include "parse/parser.h"
#include "lex/token.h"

#include "util/arena.h"

#include "lex/lexer.h"

#include "driver/target.h"

#include "lex/preprocessor.h"

#include <assert.h>
#include "files/filepath.h"

#include "lex/identifier_table.h"

int compiler_main(int argc, char** argv)
{
    SourceManager sm = source_manager();
    DiagnosticManager dm = diagnostic_manager_init(&sm, true);

    Filepath path = FILEPATH_STATIC_INIT("test.c");
    SourceFile* source = source_manager_create_filepath(&sm, path);
    assert(source);

    Preprocessor pp = preprocessor_create(&dm, &sm, source);
    parse_translation_unit(&dm, &pp);

    preprocessor_delete(&pp);
    source_manager_delete(&sm);

    return 0;
}

int main(int argc, char** argv)
{
    return compiler_main(argc, argv);
}
