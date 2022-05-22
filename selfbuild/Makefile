CFLAGS:=-I ../lib ${CFLAGS}
AS?=gcc
ASFLAGS?=-g -c
LD?=gcc

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

fibonacci.asm: ../examples/fibonacci.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

fibonacci: fibonacci.o
	${LD} ${LDFLAGS} $^ -o $@

stringmanip.asm: ../examples/stringmanip.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

stringmanip: stringmanip.o
	${LD} ${LDFLAGS} $^ -o $@
