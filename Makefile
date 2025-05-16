CC = clang
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g3 -O0 -Wshadow -Wwrite-strings \
	-Wno-unused-parameter -Wno-unused-function -Wno-unused-variable

IFLAGS = -Ilibcc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

ADT = libcc/adt/vector.c \
	libcc/adt/buffer.c

CORE = libcc/core/panic.c \
	libcc/core/xmalloc.c \
	libcc/core/location.c

PP = libcc/pp/token.c

DRIVER = #src/driver/diagnostic.c

TREE = #src/tree/ast.c

PARSE = 

CODEGEN = 

SRC = $(ADT) $(CORE) $(PP) $(DRIVER) $(TREE) $(PARSE) $(CODEGEN)

cc: $(SRC) libcc/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
