CFLAGS:=-I ../lib ${CFLAGS}
# AS=clang
# ASFLAGS=-target x86_64-apple-darwin20.3.0 -g -c
# LD=clang
# LDFLAGS=-target x86_64-apple-darwin20.3.0 -mcpu=x86-64
AS=gcc
ASFLAGS=-g -c
LD=gcc
LDFLAGS=

all: main.selfbuild

main.asm: ../cli/main.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.asm: ../lib/%.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.o: %.asm
	${AS} ${ASFLAGS} $< -o $@

main.selfbuild: array.o ast.o autoheap.o be.o cg.o elaborator.o fe.o hash.o lex.o \
		lex.o main.o parse.o parser_globals.o pool.o poolset.o preproc.o scope.o stringmap.o stringpool.o \
		symbol.o typestr.o
	${LD} ${LDFLAGS} $^ -o $@

clean:
	rm *.o *.asm main.selfbuild
