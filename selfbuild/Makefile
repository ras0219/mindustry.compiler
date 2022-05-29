SHELL = /bin/sh
.SUFFIXES:
.SUFFIXES: .o .asm

srcdir?=$(abspath ..)
AS?=gcc
ASFLAGS?=-g -c
LD?=gcc
CC?=cc

CFLAGS:=-I $(srcdir)/lib ${CFLAGS}

SRCS:=array.c ast.c autoheap.c be.c cg.c elaborator.c fe.c hash.c lex.c \
	lex.c main.c parse.c parser_globals.c pool.c poolset.c preproc.c scope.c stringmap.c stringpool.c \
	symbol.c typestr.c main.c

OBJS:=$(SRCS:.c=.o)

vpath %.c $(srcdir)/lib:$(srcdir)/cli:$(srcdir)/examples

all: main

stage2/%.asm: %.c main
	./main ${CFLAGS} $< -c -o $@

stage2: stage2/main

%.asm: %.c $(CC)
	${CC} ${CFLAGS} $< -c -o $@

%.o: %.asm
	${AS} ${ASFLAGS} $< -o $@

main: $(OBJS)
	${LD} ${LDFLAGS} $^ -o $@

asm: $(notdir $(SRCS:.c=.asm))

clean:
	rm -f *.o *.asm main.selfbuild

EXAMPLES:=fibonacci stringmanip va-example enums relations parsenum

$(addsuffix .asm,$(EXAMPLES)): %.asm: $(srcdir)/examples/%.c $(CC)
	${CC} ${CFLAGS} $< -c -o $@

$(addsuffix .o,$(EXAMPLES)): %.o: %.asm
	${AS} ${ASFLAGS} $< -o $@

$(EXAMPLES): %: %.o
	${LD} ${LDFLAGS} $^ -o $@

$(addprefix run-,$(EXAMPLES)): run-%: %
	./$^

examples: $(EXAMPLES)

.PHONY: ../out.tracing/main main.diff

export CFLAGS
main.diff: ../out.tracing/main main
	$(MAKE) -f Makefile.stage2 main.diff

../out.tracing/main:
	$(MAKE) -C ../out.tracing main
