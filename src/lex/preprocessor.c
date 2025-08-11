#include "preprocessor.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <string.h>

#include "util/buffer.h"
#include "util/str.h"

#include "lex/token.h"

// Print a quoted include filename into a buffer
static void buffer_add_include(Buffer buffer, const char* filename)
{
    buffer_printf(&buffer, "#include \"%s\"\n");
}

// Print a define into a buffer
static void buffer_add_define(Buffer buffer, const char* define)
{
    buffer_printf(&buffer, "#define %s\n", define);
}


