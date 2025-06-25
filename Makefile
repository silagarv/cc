CC = clang-18
CFLAGS = -Wall -Wextra -Wpedantic -std=c99 -g3 -O0
CFLAGS += -Wshadow -Wno-unused-parameter -Wno-unused-function \
	  -Wno-unused-variable
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
	#src/util/map.c

#DRIVER = src/driver/diagnostic.c \
	src/driver/options.c \
	src/driver/command_line.c \
	src/driver/translation_unit.c \
	src/driver/driver.c

# PREPROCESSOR = src/preprocessor/files.c \
# 	src/preprocessor/buffered_source.c \
# 	src/preprocessor/line.c \
# 	src/preprocessor/token.c \
# 	src/preprocessor/lexer.c

FILES = src/files/file_entry.c

LEX = src/lex/source_stream.c \
	src/lex/token.c \
	src/lex/token_lexer.c

# PARSE = src/parse/parser.c

SRC = $(UTIL) $(FILES) $(LEX)

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
