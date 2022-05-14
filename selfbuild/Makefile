CFLAGS:=-I ../lib ${CFLAGS}
# AS=clang
# ASFLAGS=-target x86_64-apple-darwin20.3.0 -g -c
# LD=clang
# LDFLAGS=-target x86_64-apple-darwin20.3.0 -mcpu=x86-64
AS=gcc
ASFLAGS=-g -c
LD=gcc
LDFLAGS=

SRC=array.c ast.c autoheap.c be.c cg.c elaborator.c fe.c hash.c lex.c \
	lex.c main.c parse.c parser_globals.c pool.c poolset.c preproc.c scope.c stringmap.c stringpool.c \
	symbol.c typestr.c

all: main.selfbuild

asm: $(SRC:.c=.asm)

main.asm: ../cli/main.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.asm: ../lib/%.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.o: %.asm
	${AS} ${ASFLAGS} $< -o $@

main.selfbuild: $(SRC:.c=.o)
	${LD} ${LDFLAGS} $^ -o $@

clean:
	rm *.o *.asm main.selfbuild
