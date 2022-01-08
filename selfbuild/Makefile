CFLAGS=-I ../lib -D __LP64__ -D __x86_64__ -D __STDC__ -D _POSIX_SOURCE -D "__DARWIN_OS_INLINE=static inline" -D __llvm__ -D __builtin_bswap32 -D __builtin_bswap16 -D __builtin_bswap64 -D "__func__=\"__func__\""

all: main.selfbuild

main.asm: ../cli/main.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.asm: ../lib/%.c ${CC}
	${CC} ${CFLAGS} $< -c -o $@

%.o: %.asm
	clang -target x86_64-apple-darwin20.3.0 -g $< -o $@ -c

main.selfbuild: array.o ast.o autoheap.o be.o cg.o elaborator.o fe.o hash.o lex.o \
		lex.o main.o parse.o parser_globals.o pool.o poolset.o preproc.o scope.o stringmap.o stringpool.o \
		symbol.o typestr.o
	clang -target x86_64-apple-darwin20.3.0 -mcpu=x86-64 $^ -o $@

clean:
	rm *.o *.asm main.selfbuild
