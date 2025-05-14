CC = clang
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g3 -O0 -Wshadow -Wwrite-strings \
	-Wno-unused-parameter -Wno-unused-function -Wno-unused-variable

IFLAGS = -Isrc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

ADT = src/adt/arena.c \
	src/adt/vector.c

CORE = src/core/panic.c \
	src/core/xmalloc.c

DRIVER = #src/driver/diagnostic.c

TREE = #src/tree/ast.c

PARSE = src/parse/line.c \
	src/parse/input.c \
	src/parse/location.c

CODEGEN = 

SRC = $(ADT) $(CORE) $(DRIVER) $(TREE) $(PARSE) $(CODEGEN)

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
