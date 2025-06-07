CC = gcc-12
CFLAGS = -Wall -Wextra -Wpedantic -std=gnu99 -g3 -O0
CFLAGS += -Wshadow -Wno-unused-parameter -Wno-unused-function \
	  -Wno-unused-variable
CFLAGS += -fanalyzer

IFLAGS = -Isrc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

UTIL = src/util/panic.c \
	src/util/xmalloc.c \
	src/util/buffer.c \
	src/util/static_string.c

DRIVER = src/driver/diagnostic.c \
	src/driver/command_line.c \
	src/driver/translation_unit.c \
	src/driver/driver.c

PREPROCESSOR = src/preprocessor/files.c \
	src/preprocessor/buffered_source.c \
	src/preprocessor/line.c \
	src/preprocessor/token.c \
	src/preprocessor/lexer.c

PARSE = src/parse/parser.c

SRC = $(UTIL) $(DRIVER) $(PREPROCESSOR) $(PARSE)

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
