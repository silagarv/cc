CC = clang-18
CFLAGS = -Wall -Wextra -pedantic -std=c99 -g3 -O0 
CFLAGS += -Wshadow -Wno-unused-parameter -Wno-unused-function \
	  -Wno-unused-variable
#CFLAGS += -fsanitize=address

IFLAGS = -Isrc
CFLAGS += $(IFLAGS)

.Default_Goal: cc

.PHONY: clean

UTIL = src/util/panic.c \
	src/util/xmalloc.c \
	src/util/buffer.c \
	src/util/static_string.c

DIAGNOSTIC = src/diagnostic/diagnostic.c

PREPROCESSOR = src/preprocessor/buffered_source.c \
	src/preprocessor/line.c \
	src/preprocessor/token.c

SRC = $(UTIL) $(DIAGNOSTIC) $(PREPROCESSOR) #$(FRONTEND)

cc: $(SRC) src/main.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f cc

remake: clean cc
