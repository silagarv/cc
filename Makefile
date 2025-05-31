CC = clang-18
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g3 -O0 -Wshadow \
	-Wno-unused-parameter -Wno-unused-function -Wno-unused-variable \
	-x c

IFLAGS = -Isrc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

UTIL = src/util/panic.c \
	src/util/xmalloc.c \
	src/util/buffer.c \
	src/util/vector.c

DIAGNOSTIC = src/diagnostic/diagnostic.c

FRONTEND = src/frontend/source.c \
	src/frontend/line_map.c \
	src/frontend/location_map.c \
	src/frontend/location_resolver.c \
	src/frontend/token.c

SRC = $(UTIL) $(DIAGNOSTIC) $(FRONTEND)

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
