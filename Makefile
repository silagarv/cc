CC = clang-19
CFLAGS = -Wall -Wextra -Wpedantic -std=gnu99 -g3 -O0
CFLAGS += -Wshadow -Wno-unused-parameter -Wno-unused-function \
	-Wno-unused-variable -Wno-c23-extensions -Wno-switch -Wvla
CFLAGS += -x c
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
	src/files/source_file.c \
	src/files/location.c \
	src/files/line_map.c \
	src/files/location_manager.c \
	src/files/source_manager.c

DRIVER = src/driver/diagnostic.c \
	src/driver/target.c \
	#src/driver/options.c \
	src/driver/command_line.c \
	src/driver/translation_unit.c \
	src/driver/driver.c

LEX = src/lex/token.c \
	src/lex/char_help.c \
	src/lex/unicode.c \
	src/lex/lexer.c \
	src/lex/macro.c \
	src/lex/preprocessor.c

PARSE = src/parse/type.c \
	src/parse/literal_parser.c \
	src/parse/expression.c \
	src/parse/parser.c

SRC = $(UTIL) $(FILES) $(LEX) $(PARSE) $(DRIVER) 

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

