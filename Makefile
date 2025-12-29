CC = clang
CFLAGS = -Wall \
	-Wextra \
	-Wpedantic \
	-Werror \
	-std=c99 \
	-g3 \
	-O0 \
	-MMD \
	-MP

CFLAGS += -Wno-unused-parameter \
	-Wno-unused-variable \
	-Wno-unused-function \
	-Wno-unused-but-set-variable \
	-Wno-switch \
	-Wshadow \
	-Wvla \
	-ferror-limit=0

LFLAGS = -lm

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
	src/util/hash_map.c \
	src/util/ptr_set.c

FILES = src/files/filepath.c \
	src/files/file_manager.c \
	src/files/location.c \
	src/files/line_map.c \
	src/files/line_table.c \
	src/files/location_manager.c \
	src/files/source_manager.c

DRIVER = src/driver/diagnostic.c \
	#src/driver/target.c \
	src/driver/options.c \
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
	src/parse/ast.c \
	src/parse/type.c \
	src/parse/literal_parser.c \
	src/parse/expression.c \
	src/parse/statement.c \
	src/parse/declaration.c \
	src/parse/symbol.c \
	src/parse/scope.c \
	src/parse/parser.c \
	src/parse/semantic.c

SRCS = $(UTIL) $(FILES) $(DRIVER) $(LEX) $(PARSE) src/main.c

BUILDDIR = build
OBJS = $(patsubst src/%.c, $(BUILDDIR)/%.o, $(SRCS))
DEPS := $(OBJS:.o=.d)

cc: $(OBJS)
	$(CC) $(CFLAGS) $^ -o $@ $(LFLAGS)

$(BUILDDIR)/%.o: src/%.c | $(BUILDDIR)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	rm -r $(BUILDDIR)
	rm -f cc

-include $(DEPS)
