CC = clang
CFLAGS = -Wall -Wextra -Wpedantic -std=c99 -g3 -O0
CFLAGS += -Wshadow -Wno-unused-parameter -Wno-unused-function \
	-Wno-unused-variable 
CFLAGS += -x c
# CFLAGS += -fanalyzer
CFLAGS += -flto

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
	src/util/hash.c

DRIVER = src/driver/diagnostic.c \
	#src/driver/options.c \
	src/driver/command_line.c \
	src/driver/translation_unit.c \
	src/driver/driver.c

# FILES = src/files/file_entry.c

LEX = src/lex/location_map.c \
	src/lex/token.c \
	src/lex/char_type.c \
	src/lex/lexer.c

PARSE = src/parse/type.c \
	src/parse/literal_parser.c \
	src/parse/parser.c

SRC = $(UTIL) $(FILES) $(LEX) $(PARSE) $(DRIVER) 

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

