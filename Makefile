CC = clang
CFLAGS = -Wall \
	-Wextra \
	-Wpedantic \
	-std=c99 \
	-g3 \
	-O0
CFLAGS += -Wno-unused-parameter \
	-Wno-unused-variable \
	-Wno-unused-function \
	-Wno-unused-but-set-variable \
	-Wno-switch \
	-Wshadow \
	-Wvla \
	-ferror-limit=0

# CFLAGS += -fanalyzer
# CFLAGS += -flto

IFLAGS = -Isrc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

UTIL = src/util/panic.c \
	src/util/xmalloc.c \
	src/util/buffer.c \
	src/util/str.c \
	src/util/str_view.c \
	src/util/arena.c \
	src/util/hash.c \
	src/util/hash_map.c

FILES = src/files/filepath.c \
	src/files/file_manager.c \
	src/files/location.c \
	src/files/line_map.c \
	src/files/line_table.c \
	src/files/location_manager.c \
	src/files/source_manager.c

DRIVER = src/driver/diagnostic.c \
	src/driver/target.c \
	#src/driver/options.c \
	src/driver/command_line.c \
	src/driver/translation_unit.c \
	src/driver/driver.c

LEX = src/lex/identifier_table.c \
	src/lex/token.c \
	src/lex/char_help.c \
	src/lex/unicode.c \
	src/lex/lexer.c \
	src/lex/macro.c \
	src/lex/preprocessor.c

PARSE = src/parse/ast_allocator.c \
	src/parse/type.c \
	src/parse/symbol.c \
	src/parse/literal_parser.c \
	src/parse/expression.c \
	src/parse/statement.c \
	src/parse/declaration.c \
	src/parse/symbol.c \
	src/parse/ast.c \
	src/parse/parser.c \
	src/parse/semantic.c

SRC = $(UTIL) $(FILES) $(LEX) $(PARSE) $(DRIVER) 

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

